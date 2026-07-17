"use client";

// WS handling for the main file view (fileview.tsx): opening/closing a
// file's connection, atom edits, chat commands, and scene presence.
// Colocated with fileview.tsx rather than living in a shared store file —
// see lib/serverCacheStore.ts's header for the write-access convention.

import { fileConn, saveRawFileAsNew } from "@/lib/ws";
import type { FileCommand, ContextItem, PickerRule } from "@/lib/ws";
import { getServerCache, mirrorServerEvent } from "@/lib/serverCacheStore";
import { useUI, dropFromSelection, setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { applyUpdate, isChatPreviewEvent, remapTickId, remapSet, atRebase } from "@/lib/wsHelpers";
import { clearPreviewDelayTimer, schedulePreviewPlaceholder, handleChatPreview } from "@/lib/chatPreview";
import { tickChain, promptGroupForAtom, activeCharacterBranches } from "@/lib/utils";
import { useSettings, contextFilterKey, toContextLayout } from "@/lib/settingsStore";
import { WRITER_STORY_SOURCE_ID, CHARACTER_CONTEXT_SOURCE_ID } from "@/lib/agents";
import { resolveMentions } from "@/lib/mentions";
import { triggeredLorePaths } from "@/lib/loreTrigger";

export async function openFile(path: string): Promise<void> {
  const { activeBranch, openFiles } = getServerCache();
  if (!activeBranch) return;
  useUI.setState({ rebaseMarker: null, pendingSubmit: null });
  if (openFiles[path]) return;

  const label = `file:${path}`;
  setConnStatus(label, "connecting");

  const fc = fileConn(activeBranch, path);

  fc.onStatus((s) => {
    if (s !== "connected") setConnStatus(label, "connecting");
  });

  fc.subscribe((evt) => {
    bumpActivity(label);
    if (evt.type === "file.present") {
      mirrorServerEvent((s) => ({
        openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "file.absent") {
      mirrorServerEvent((s) => ({
        openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: true, conn: fc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "update") {
      clearPreviewDelayTimer();
      mirrorServerEvent((s) => {
        const prev = s.openFiles[path];
        if (!prev) return {};
        return {
          openFiles: {
            ...s.openFiles,
            [path]: { ...prev, ticks: applyUpdate(prev.ticks, evt), head: evt.head, absent: false },
          },
          preview: null,
          previewCommandId: null,
        };
      });
    } else if (evt.type === "tick.remap") {
      handleTickRemap(evt.mapping);
    } else if (evt.type === "agent.log") {
      useUI.getState().addAgentLog(evt.level, evt.message);
    } else if (evt.type === "character.answered") {
      useUI.getState().addCharacterAnswer(evt.character, evt.question, evt.answer);
    } else if (isChatPreviewEvent(evt)) {
      handleChatPreview(evt);
    } else if (evt.type === "error") {
      clearPreviewDelayTimer();
      mirrorServerEvent({ preview: null, previewCommandId: null });
      setError(evt.message);
    }
  });

  try {
    await fc.connect();
    mirrorServerEvent((s) => ({
      openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
    }));
  } catch (err) {
    setConnStatus(label, "error");
    setError(String(err));
  }
}

// Explicit "new file" creation: opens the connection (same as selecting
// an existing file, which starts out absent), then sends file.create —
// the server introduces the path as its own empty tick, and the
// resulting file.present + update events flip 'absent' to false, same as
// any other write to a not-yet-tracked path would.
export async function createFile(path: string): Promise<void> {
  await openFile(path);
  getServerCache().openFiles[path]?.conn.send({ type: "file.create" });
}

// Whole-file delete: a forward tick, not a rebase (see
// Storyteller.Core.Create's Haddock) — path's whole tick history stays
// intact for anyone time-travelling to before the deletion, but the path
// itself drops out of the tree, so fileState on this same connection
// comes back present-but-headed-at-the-deletion-tick, not absent.
export function deleteFile(path: string) {
  sendFileCommand(path, { type: "delete" });
}

// Rename path's current lifetime to newPath — a rebase at the file's own
// creation tick (see Storage.Ops.renameFile), sent on the *old* path's
// connection since that's where the file currently lives. The server
// pushes file.absent on this connection once it lands (fileState on the
// old path genuinely empties out — rename rewrites those ticks' own
// atomPath, unlike delete's forward event); the caller is responsible for
// switching to the new path's connection afterward (see page.tsx's
// handleRenameFile), same as closing/reopening on any other path change.
export function renameFile(path: string, newPath: string) {
  sendFileCommand(path, { type: "rename", newPath });
}

// Freeze this file's current lifetime and clone it in full onto a fresh
// one (see Storage.Ops.checkpointFile) — an atom edit/delete issued after
// this point can only ever reach the new copies, while everything before
// it stays exactly as it was, just no longer reachable through ordinary
// editing. The lighter-weight alternative to saveFileAsNew below: content
// is untouched, only the editing boundary moves.
export function checkpointFile(path: string) {
  sendFileCommand(path, { type: "checkpoint" });
}

// "Save as new": replace this file's content wholesale, bypassing the
// usual atom-diff reconciliation entirely (see lib/ws.ts's saveRawFileAsNew /
// Storage.Ops.saveFileAsNew) — for a raw/markdown-editor structural change
// that shouldn't be tracked atom-by-atom. Goes over HTTP PUT, not the file
// connection's own command channel, same as saveRawFile already does for
// an ordinary raw-mode save; the caller is responsible for re-syncing the
// connection afterward the same way a rename's caller already is.
export async function saveFileAsNew(path: string, content: string, newPath?: string): Promise<void> {
  const { activeBranch } = getServerCache();
  if (!activeBranch) return;
  await saveRawFileAsNew(activeBranch, path, content, newPath);
}

export function closeFile(path: string) {
  getServerCache().openFiles[path]?.conn.close();
  mirrorServerEvent((s) => {
    const next = { ...s.openFiles };
    delete next[path];
    return { openFiles: next };
  });
  removeConn(`file:${path}`);
}

// rebaseMarker (see lib/utils.tailLeadTicks / wsHelpers.atRebase) is the
// tick the bar is stuck to — the first tick of whatever's suppressed after
// it. Whenever a command runs through this marker, that exact tick is the
// first thing 'at' pops and later replays back on top (see
// Storyteller.Core.Git.atGeneric) — its parent changed, so it's always one
// of the "from" ids in the resulting mapping. That makes keeping it current
// a plain table lookup, no different from contextAtoms/contextAnnotations
// below — no walking the chain, no reasoning about where "this file's own
// tail" starts versus some other branch's cascade caught in the same
// broadcast.
function handleTickRemap(mapping: [string, string][]) {
  const table = new Map(mapping);
  useUI.setState((s) => ({
    rebaseMarker: s.rebaseMarker ? remapTickId(table, s.rebaseMarker) : s.rebaseMarker,
    contextAtoms: remapSet(table, s.contextAtoms),
    contextAnnotations: remapSet(table, s.contextAnnotations),
  }));
}

// Build the read-only context items for a chat command from the current
// atom/annotation selection — chain order, atoms then annotations, per
// SELECTION.md. contextAtoms/contextAnnotations are shared across the open
// file and every open journal, so a selection made in a character's journal
// is a genuine cross-branch reference when it rides along on a command sent
// to the main file's connection — tagged with its own branch, since only
// the *file's* branch is implied by the connection.
function buildContextItems(path: string): ContextItem[] {
  const cache = getServerCache();
  const ui = useUI.getState();
  const fc = cache.openFiles[path];
  if (!fc) return [];
  const chain = tickChain(fc.ticks, fc.head);
  const atoms       = chain.filter((t) => ui.contextAtoms.has(t.tickId));
  const annotations = chain.filter((t) => ui.contextAnnotations.has(t.tickId));
  const local: ContextItem[] = [...atoms, ...annotations].map((t) => ({
    tickId: t.tickId,
    kind: t.kind,
    content: t.content ?? t.message,
  }));

  const crossBranch: ContextItem[] = [];
  for (const [branch, jc] of Object.entries(cache.openJournals)) {
    if (!jc) continue;
    const jchain = tickChain(jc.ticks, jc.head);
    const jAtoms       = jchain.filter((t) => ui.contextAtoms.has(t.tickId));
    const jAnnotations = jchain.filter((t) => ui.contextAnnotations.has(t.tickId));
    for (const t of [...jAtoms, ...jAnnotations]) {
      crossBranch.push({ tickId: t.tickId, kind: t.kind, content: t.content ?? t.message, branch });
    }
  }

  return [...local, ...crossBranch];
}

// Target ids for chat.fixer/chat.note, scoped to this file's own chain —
// contextAtoms is a shared, connection-agnostic set (a journal atom's id can
// be selected alongside a scene atom's, see character-sidebar.tsx), so a
// command sent on *this* file's connection must only carry the subset that
// actually belongs to it. Journal-selected ids are picked up by
// journalFix/deleteJournalAtom instead (see page.tsx's handleFix).
function buildContextTargets(path: string): string[] {
  const fc = getServerCache().openFiles[path];
  if (!fc) return [];
  return tickChain(fc.ticks, fc.head)
    .filter((t) => t.kind === "atom" && useUI.getState().contextAtoms.has(t.tickId))
    .map((t) => t.tickId);
}

// Send a command on path's file connection, rebased at the current marker
// if one's set (see wsHelpers.atRebase) — the one place that reads
// 'rebaseMarker'/'journalMarkers' and this file's own live 'ticks' to
// resolve the actual 'at' pivot, so every mutating command below gets that
// resolution identically instead of repeating it.
function sendFileCommand(path: string, cmd: FileCommand) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  const ui = useUI.getState();
  fc.conn.send(atRebase(ui.rebaseMarker, fc.ticks, cmd, ui.journalMarkers));
}

// Send a chat.* command now, or hold it if a generation is already
// streaming on this connection (server processes one command at a time).
// 'buildCmd' receives the captured flowTid (HEAD at queue-time) only when
// the command is actually being queued — an immediate send has no
// in-flight generation to be provisional about. An immediate send gets a
// fresh id attached and recorded as 'previewCommandId' — what a Stop
// button (see 'cancelGeneration') targets; the queued-and-flushed path
// gets its own id at flush time instead (see lib/chatPreview.ts).
function sendChatCommand(path: string, buildCmd: (flowTid?: string) => FileCommand) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  if (getServerCache().preview !== null) {
    useUI.setState({ pendingSubmit: { path, cmd: buildCmd(fc.head ?? undefined) } });
  } else {
    const cmd = { ...buildCmd(undefined), id: crypto.randomUUID() };
    mirrorServerEvent({ previewCommandId: cmd.id });
    schedulePreviewPlaceholder();
    sendFileCommand(path, cmd);
  }
}

// Ask the server to stop the in-flight chat/write generation early — see
// Server.Writer.Session.Protocol's Cancel. No-ops if nothing is generating
// (previewCommandId null) or the session connection isn't up.
export function cancelGeneration() {
  const targetId = getServerCache().previewCommandId;
  if (!targetId) return;
  getServerCache()._session?.send({ type: "cancel", targetId });
}

// Presence is scoped to a file (a scene), not the whole branch — see
// WRITER.md — so this sends on the file connection, same as any other
// mutating file command, and gets rebase-at-marker for free via 'atRebase'.
export function enterScene(path: string, character: string) {
  sendFileCommand(path, { type: "enter.scene", character });
}

export function leaveScene(path: string, character: string) {
  sendFileCommand(path, { type: "leave.scene", character });
}

// Ask 'character' a question, answered from only their own branch (sheet,
// journal — not this scene, not any other character; see Server.Writer.
// File.askCharacter). Recorded here as a CharacterAnswer tick on this
// branch, not the character's own — asking doesn't give them a new memory.
// Goes through sendFileCommand like any other mutating command, so it gets
// rebase-at-marker for free (asking "as of" a historical point in the
// story) — the answer itself arrives as a character.answered event, handled
// in openFile's subscribe callback above.
export function askCharacter(path: string, character: string, question: string) {
  sendFileCommand(path, { type: "ask.character", character, question });
}

export function appendToFile(path: string, content: string) {
  const text = content.replace(/^\/+/, "");
  sendChatCommand(path, () => ({ type: "chat.append", content: text }));
}

export function editAtom(path: string, tickId: string, content: string) {
  sendFileCommand(path, { type: "edit.atom", tickId, content });
  dropFromSelection([tickId]);
}

// Edit a chat prompt tick's text — see ws.ts's "edit.prompt": a prompt
// isn't file content, so this is a distinct command from editAtom.
export function editPrompt(path: string, tickId: string, content: string) {
  sendFileCommand(path, { type: "edit.prompt", tickId, content });
}

export function deleteAtom(path: string, tickId: string) {
  sendFileCommand(path, { type: "delete.atom", tickId });
  dropFromSelection([tickId]);
}

// Both reuse the existing atom context selection ('contextAtoms') rather
// than introducing a separate selection mode — same targets Fix/Note
// already operate on.
export function mergeSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 2) return;
  sendFileCommand(path, { type: "merge.atoms", targets });
  dropFromSelection(targets);
}

export function splitSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "split.atoms", targets });
  dropFromSelection(targets);
}

export function hideSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "hide.atoms", targets });
  dropFromSelection(targets);
}

export function unhideSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "unhide.atoms", targets });
  dropFromSelection(targets);
}

function writerContextLayout(): PickerRule[] {
  const branch = getServerCache().activeBranch;
  if (!branch) return [];
  const filter = useSettings.getState().contextFilters[contextFilterKey(branch, WRITER_STORY_SOURCE_ID)];
  return filter ? toContextLayout(filter) : [];
}

// Every codex-entry path the active branch's writer:story filter has
// flagged "Triggered" (see settingsStore.ts's ContextFilter.triggers and
// lore-selector.tsx) whose name or an alias is actually mentioned in
// `text` — see lib/loreTrigger.ts. Empty whenever there's no active
// branch, no triggers configured, or nothing matches.
function triggeredLoreForText(text: string): string[] {
  const branch = getServerCache().activeBranch;
  if (!branch) return [];
  const filter = useSettings.getState().contextFilters[contextFilterKey(branch, WRITER_STORY_SOURCE_ID)];
  if (!filter || filter.triggers.length === 0) return [];
  return triggeredLorePaths(text, getServerCache().loreTree, new Set(filter.triggers));
}

// One entry per currently-active (in-scene) character branch that's
// actually been curated via character-sidebar.tsx's Context panel — a
// branch with no configured override (the common case) is simply omitted,
// since an absent entry and an explicit empty layout resolve identically
// server-side (see Server.Writer.File.activeCharacterContext).
function activeCharacterLayouts(path: string): Record<string, PickerRule[]> {
  const fc = getServerCache().openFiles[path];
  if (!fc) return {};
  const result: Record<string, PickerRule[]> = {};
  for (const branch of activeCharacterBranches(fc.ticks, fc.head)) {
    const filter = useSettings.getState().contextFilters[contextFilterKey(branch, CHARACTER_CONTEXT_SOURCE_ID)];
    if (filter && filter.tags.length > 0) result[branch] = toContextLayout(filter);
  }
  return result;
}

// The Writer agent's own context shape (pinned selection, mention-aware
// bucket layout, active character overrides) — shared by chatWrite and
// correctAtom, since "correct" is the same agent/context, just landing
// back at the group's old position instead of appending at file end.
function writerCommandContext(path: string, text: string) {
  const context = buildContextItems(path);
  const { cleanText, paths: mentionedPaths } = resolveMentions(text);
  const baseLayout = writerContextLayout();
  // Two independent sources force a path to the front, ahead of the
  // curated layout: an explicit `@mention` and a Triggered lore entry whose
  // alias got matched in the raw text (see triggeredLoreForText/
  // lib/loreTrigger.ts) — same outcome either way, so they're deduped into
  // one forced-bucket-1 list.
  const forcedPaths = [...new Set([...mentionedPaths, ...triggeredLoreForText(cleanText)])];
  // An empty base layout already means "show everything" — a forced path
  // adds nothing there. Only a curated (non-empty) layout needs the
  // force-inclusion, prepended so it claims ahead of whatever the curated
  // layout would otherwise do (first-match-wins, see
  // Storyteller.Writer.Agent.ContextFilter.classifyPath).
  const contextLayout = baseLayout.length === 0
    ? []
    : [...forcedPaths.map((p) => ({ pattern: p, bucket: 1 })), ...baseLayout];
  const characterLayouts = activeCharacterLayouts(path);
  return { cleanText, context, contextLayout, characterLayouts };
}

export function chatWrite(path: string, text: string) {
  const { cleanText, context, contextLayout, characterLayouts } = writerCommandContext(path, text);
  sendChatCommand(path, (flowTid) => ({ type: "chat.writer", text: cleanText, context, contextLayout, flowTid, characterLayouts }));
}

// "Correct this" — regenerate the whole instruction-group a given atom
// belongs to (see lib/utils.promptGroupForAtom), via the same agent/context
// chatWrite already uses, landing back at the same position instead of
// appending at file end. One plain action, not three: all three DESIGN.md
// correction cases (bad roll, narrator misunderstood, character was wrong)
// reduce to this same call — a bad roll just calls it as-is; "narrator
// misunderstood" is editing the group's own Prompt tick first (see
// editPrompt, and fileview.tsx's PromptHeader — an independent action, no
// regeneration side-effect of its own) then calling this; "character was
// wrong" is appending to their journal first (manually, or via the
// /inform command — see lib/commands.ts) then calling this, so the fresh
// activeCharacterContext chatWrite pulls in already reflects it. Reading
// the prompt fresh here rather than taking it as a parameter is what makes
// that composition work — this always regenerates from whatever's
// currently saved, not a stale snapshot from when the button was drawn.
//
// One 'correct.group' command, not a client-composed delete-per-atom loop
// followed by a separate chat.writer: the server deletes the whole group
// and regenerates in its place inside one transaction (see
// Server.Writer.File.correctGroup). That's not just fewer round trips —
// it's what makes this a single undo point instead of one per deleted
// atom, and it means the group stays on screen untouched for the whole
// generation instead of visibly vanishing atom-by-atom before the
// replacement starts streaming in.
export function correctAtom(path: string, atomTickId: string) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  const group = promptGroupForAtom(fc.ticks, fc.head, atomTickId);
  if (!group) return;

  const { cleanText, context, contextLayout, characterLayouts } = writerCommandContext(path, group.promptTick.message);
  sendChatCommand(path, () => ({
    type: "correct.group",
    promptTickId: group.promptTick.tickId,
    targets: group.atomTickIds,
    text: cleanText,
    context,
    contextLayout,
    characterLayouts,
  }));
}

export function chatFix(path: string, text: string) {
  const context = buildContextItems(path);
  const targets = buildContextTargets(path);
  sendChatCommand(path, () => ({ type: "chat.fixer", text, context, targets }));
}

// A note with nothing selected implicitly targets the last atom in the
// file — notes aren't tied to a file at all server-side (Note []'s refs are
// just commentary payload, chained onto the current head regardless), so an
// empty targets list would otherwise float free of any atom instead of
// commenting on whatever the user was just looking at.
export function chatNote(path: string, text: string) {
  let targets = buildContextTargets(path);
  if (targets.length === 0) {
    const fc = getServerCache().openFiles[path];
    const atoms = fc ? tickChain(fc.ticks, fc.head).filter((t) => t.kind === "atom") : [];
    const last = atoms[atoms.length - 1];
    if (last) targets = [last.tickId];
  }
  sendChatCommand(path, () => ({ type: "chat.note", text, targets }));
}

export function chatRegen(path: string, text: string, byBeat: boolean) {
  const context = buildContextItems(path);
  sendChatCommand(path, () => ({ type: "chat.regen", text, context, byBeat }));
}

export function chatOutline(path: string) {
  sendChatCommand(path, () => ({ type: "chat.outline" }));
}

// Discuss, don't write — see WRITER.md's chat/ convention and ChatView.
export function chatConverse(path: string, text: string) {
  sendChatCommand(path, () => ({ type: "chat.converse", text }));
}

// Regenerate a chat exchange's reply, keeping the old reply as a
// cycle-able alternate (a "swipe") instead of discarding it. Only ever
// called on the *last* exchange in a chat file — the prompt is edited in
// place rather than resent, so redoing a non-final turn would leave a
// later reply answering a prompt that no longer matches what's above it.
export function chatConverseRegen(path: string, promptTickId: string, atomTickId: string, text: string) {
  sendChatCommand(path, () => ({ type: "chat.converse.regen", promptTickId, atomTickId, text }));
}

// Cycle an atom (chat reply or prose) forward through its own alternates —
// see Storyteller.Common.Swipe. Forward-only: the backend rotates a ring,
// there's no separate "previous."
export function cycleSwipe(path: string, tickId: string) {
  sendFileCommand(path, { type: "atom.swipe.cycle", tickId });
}
