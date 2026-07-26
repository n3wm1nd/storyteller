"use client";

// One live file list for a whole branch, as a hook. Lifted out of
// app/agentstab.tsx (its only consumer until user-defined agents needed
// the same list to discover themselves from — see lib/customAgents.ts):
// both want "what's currently on the prompts/contexts branch", kept live
// as files appear and vanish, and neither wants its own connection
// bookkeeping.
//
// `branch.ready` carries the full list; `file.added`/`file.removed` patch
// it incrementally afterwards, so a file committed from another surface
// (the DSL editor, the library) shows up without a refresh.

import { useEffect, useState } from "react";
import { branchConn } from "./ws";
import { setConnStatus, removeConn, bumpActivity } from "./uiStore";

export function useBranchFiles(branchName: string): string[] | null {
  const [files, setFiles] = useState<string[] | null>(null);

  useEffect(() => {
    const label = `branch:${branchName}`;
    setConnStatus(label, "connecting");
    const conn = branchConn(branchName);

    conn.subscribe((evt) => {
      bumpActivity(label);
      if (evt.type === "branch.ready") setFiles(evt.files);
      else if (evt.type === "file.added") setFiles((fs) => (fs ? [...fs, evt.path] : fs));
      else if (evt.type === "file.removed") setFiles((fs) => (fs ? fs.filter((f) => f !== evt.path) : fs));
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(label, "connected");
      } catch {
        setConnStatus(label, "error");
      }
    })();

    return () => { conn.close(); removeConn(label); };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [branchName]);

  return files;
}
