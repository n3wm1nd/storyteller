"use client";

// WS handling for the Tasks feature (tasks-panel.tsx): experimental
// tasks.md sync/suggest, per character. Split out of
// character-sidebar.actions.ts so Tasks can be dropped or reused
// independently of the rest of the character sidebar.

import { branchConn, type BranchCommand } from "@/lib/ws";
import { getServerCache } from "@/lib/serverCacheStore";
import { useUI, setError } from "@/lib/uiStore";

export const TASKS_PATH = "tasks.md";

const JOURNAL_PATH = "journal.md";

// Same short-lived-connection shape as tracker.actions.trackOne: fire the
// command on the character branch's own connection, close on the resulting
// update/error. Resolves once the command has actually landed (or failed)
// — not on send — so a caller that wants to refetch tasks.md afterward
// (see tasks-panel.tsx's TasksPanel) doesn't race the mutation.
//
// Also forwards "agent.log" events to the global agent log strip (same as
// openJournal's own connection handler) — the server interprets a branch
// connection's Logging effect via loggingWS (Server.Writer.Run), which
// pushes every info/warning straight to this connection as agent.log,
// never to the server's own stdout. Without this, Storyteller.Writer.
// Agent.Tasks's own progress logging (querying the model, "no source
// material found", etc.) would be sent and then silently dropped, on a
// connection nobody's watching.
//
// Every /branch/{name} connection unconditionally pushes one "branch.ready"
// + one "update" the instant it opens (Server.Writer.Branch.Connection's
// pushInitial) -- before the server has even read this connection's first
// message, let alone processed our command. That initial "update" is
// *never* our command's own result; treating it as one (an earlier version
// of this function did, matching trackOne's own shape) closes the
// connection within milliseconds of connecting, every single time -- not a
// race, a guaranteed miss -- so every agent.log this command was ever
// going to emit, and its real completion event, land on an already-dead
// socket. 'seenUpdate' skips exactly that first one.
function runTasksCommand(characterBranch: string, cmd: BranchCommand): Promise<void> {
  return new Promise((resolve) => {
    const conn = branchConn(characterBranch);
    let seenUpdate = false;
    conn.subscribe((evt) => {
      if (evt.type === "agent.log") useUI.getState().addAgentLog(evt.level, evt.message);
      if (evt.type === "update" && !seenUpdate) {
        seenUpdate = true; // the connection's own initial push, not our command's result
        return;
      }
      if (evt.type === "update" || evt.type === "error" || evt.type === "file.added") {
        conn.close();
        resolve();
      }
      if (evt.type === "error") setError(evt.message);
    });
    conn.connect().then(() => {
      conn.send(cmd);
    }).catch(() => { conn.close(); resolve(); });
  });
}

// Reconcile this character's tasks.md against whatever's new in their own
// journal since the last sync — see Storyteller.Writer.Agent.Tasks.syncTasks.
// Restricted to journal.md, same "only what this character actually
// witnessed" reasoning as trackJournal itself (the journal is already
// presence-gated on the way in).
export function syncTasks(characterBranch: string) {
  return runTasksCommand(characterBranch, { type: "sync.tasks", onlyFile: JOURNAL_PATH, to: TASKS_PATH });
}

// Propose new tasks for this character from their full character context
// (sheet, other context files, recent journal) plus the active story's
// world lore — never the story's raw scene content (see
// Server.Writer.Branch.Protocol.SuggestTasks's own Haddock on why). No
// onlyFile here (unlike syncTasks) — suggestion always reads this
// character's whole context, not a caller-picked file.
export function suggestTasks(characterBranch: string) {
  const loreSource = getServerCache().activeBranch;
  return runTasksCommand(characterBranch, {
    type: "suggest.tasks", loreSource: loreSource ?? undefined, to: TASKS_PATH,
  });
}
