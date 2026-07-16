"use client";

// WS handling for the Tracker feature: explicit, one-shot invocation of the
// raw Tracker agent (see Storyteller.Writer.Agent.Tracker) — copies new
// deltas from a source branch into a character's journal.md, verbatim,
// with a cross-branch ref back to each source atom. Split out of
// character-sidebar.actions.ts so Tracker can be reused/dropped
// independently of the rest of the character sidebar.

import { branchConn } from "@/lib/ws";
import { getServerCache } from "@/lib/serverCacheStore";
import { setError } from "@/lib/uiStore";

const JOURNAL_PATH = "journal.md";

// 'track' is a BranchCommand sent on the *character* branch's own
// connection, not the currently open story branch's — so this opens a
// short-lived branch connection just to fire it, and closes it the moment
// the resulting update/error lands. No persistent state needed: the
// journal's own file connection (if open) receives the new ticks through
// its own push, same as any other write to that branch.
//
// 'onlyFile' omitted pulls every file on the source branch (not just the
// one currently open) into the journal in one call — presence gating
// (Server.Writer.Branch.onlyWhilePresent) still applies per atom, so this
// is safe (and cheap, see trackBranch's own Haddock on the shallow walk)
// to call for a character who isn't even in the current scene; see
// 'trackAllJournals' below for the sidebar's "Track All" button.
function trackOne(characterBranch: string, source: string, onlyFile: string | undefined) {
  const conn = branchConn(characterBranch);
  conn.subscribe((evt) => {
    if (evt.type === "update" || evt.type === "error") conn.close();
    if (evt.type === "error") setError(evt.message);
  });
  conn.connect().then(() => {
    conn.send({ type: "track", source, onlyFile, to: JOURNAL_PATH });
  }).catch(() => { conn.close(); });
}

export function trackJournal(characterBranch: string, fromPath: string) {
  const source = getServerCache().activeBranch;
  if (!source) return;
  trackOne(characterBranch, source, fromPath);
}

// The sidebar's "Track All" button: every known character branch (not just
// the ones present in the current scene), pulling every source file (not
// just whatever's open) into each one's own journal — see 'trackOne's
// Haddock for why this is safe/cheap to run indiscriminately, including for
// characters absent from every recent scene.
export function trackAllJournals(characterBranches: string[]) {
  const source = getServerCache().activeBranch;
  if (!source) return;
  for (const branch of characterBranches) trackOne(branch, source, undefined);
}
