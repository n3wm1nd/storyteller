"use client";

// The saved-functions library -- a small browser that lists every
// `context/*.dsl` file on the `contexts` branch (lib/contextBranch.ts)
// and lets the user load one as this file's active context program.
//
// Each row: the function's name, a "Load" button (sets this file's
// mode to "named" with that name), and a "View source" affordance
// (read-only peek without committing). Deleting is Phase-2 (the file
// API has no DELETE) -- power users can edit the contexts branch
// directly via git for now.
//
// Refresh is manual (a button) -- the contexts branch changes rarely
// (only when the user themselves saves), so a live connection would
// be overkill. A one-shot `listContextFunctions` per refresh-click is
// enough.

import { useEffect, useState } from "react";
import { RefreshCw, FileCode, Upload, Eye } from "lucide-react";
import { useCallContext } from "@/lib/callContextStore";
import {
  listContextFunctions, readContextFunction,
  type SavedContextFunction,
} from "@/lib/contextBranch";
import { setError } from "@/lib/uiStore";

interface ContextLibraryProps {
  path: string;
}

export function ContextLibrary({ path }: ContextLibraryProps) {
  const fileState = useCallContext((s) => s.files[path]);
  const loadNamed = useCallContext((s) => s.loadNamed);
  const [items, setItems] = useState<SavedContextFunction[] | null>(null);
  const [loading, setLoading] = useState(false);
  const [preview, setPreview] = useState<{ name: string; source: string } | null>(null);
  const [previewLoading, setPreviewLoading] = useState(false);

  const activeName = fileState?.mode === "named" ? fileState.namedName : null;

  async function refresh() {
    setLoading(true);
    try {
      const list = await listContextFunctions();
      setItems(list);
    } catch (err) {
      setError(String(err));
      setItems([]);
    } finally {
      setLoading(false);
    }
  }

  // Load once on mount; further refreshes are explicit (button).
  useEffect(() => { refresh(); /* eslint-disable-next-line */ }, []);

  async function previewSource(name: string) {
    setPreviewLoading(true);
    try {
      const src = await readContextFunction(name);
      setPreview({ name, source: src });
    } catch (err) {
      setError(String(err));
    } finally {
      setPreviewLoading(false);
    }
  }

  if (items === null) {
    return (
      <div style={{ display: "flex", alignItems: "center", gap: 6, padding: 8, fontSize: 10.5, color: "var(--text-ghost)" }}>
        <RefreshCw style={{ width: 11, height: 11 }} className={loading ? "animate-spin" : ""} />
        Loading saved functions…
      </div>
    );
  }

  if (items.length === 0) {
    return (
      <div style={{
        padding: 8, fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic",
        border: "1px dashed var(--border-subtle)", borderRadius: 5,
      }}>
        No saved functions yet. Use the editor above to author one, or the casual panel's "Save as…" to promote selections.
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <div style={{ display: "flex", alignItems: "center", gap: 6, marginBottom: 2 }}>
        <span style={{ fontSize: 10, color: "var(--text-ghost)", flex: 1 }}>
          {items.length} function{items.length === 1 ? "" : "s"} on the contexts branch
        </span>
        <button onClick={refresh} disabled={loading} title="Refresh" style={libBtnStyle}>
          <RefreshCw style={{ width: 10, height: 10 }} className={loading ? "animate-spin" : ""} />
        </button>
      </div>
      {items.map((fn) => {
        const isActive = fn.name === activeName;
        return (
          <div
            key={fn.path}
            style={{
              display: "flex", alignItems: "center", gap: 6,
              padding: "3px 6px", borderRadius: 5,
              background: isActive ? "var(--accent-tint, var(--amber-tint))" : "var(--card)",
              border: `1px solid ${isActive ? "var(--accent, var(--amber))" : "var(--border-subtle)"}`,
              fontSize: 11,
            }}
          >
            <FileCode style={{
              width: 11, height: 11,
              color: isActive ? "var(--accent, var(--amber))" : "var(--text-dim)",
            }} />
            <code style={{
              flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
              fontFamily: "monospace",
              color: isActive ? "var(--accent, var(--amber))" : "var(--foreground)",
            }}>
              {fn.name}
            </code>
            <button
              onClick={() => previewSource(fn.name)}
              disabled={previewLoading}
              title="View source"
              style={libBtnStyle}
            >
              <Eye style={{ width: 10, height: 10 }} />
            </button>
            <button
              onClick={() => loadNamed(path, fn.name)}
              disabled={isActive}
              title={isActive ? "Currently active" : "Use this function"}
              style={{
                ...libBtnStyle,
                color: isActive ? "var(--text-ghost)" : "var(--accent, var(--amber))",
                cursor: isActive ? "default" : "pointer",
              }}
            >
              <Upload style={{ width: 10, height: 10 }} /> {isActive ? "Active" : "Load"}
            </button>
          </div>
        );
      })}

      {preview && (
        <div style={{
          marginTop: 6, padding: 6, background: "var(--surface-deep)",
          border: "1px solid var(--border-subtle)", borderRadius: 5,
          display: "flex", flexDirection: "column", gap: 4,
        }}>
          <div style={{
            display: "flex", alignItems: "center", gap: 6, fontSize: 10.5,
            color: "var(--text-dim)",
          }}>
            <code style={{ fontFamily: "monospace", color: "var(--accent, var(--amber))" }}>{preview.name}</code>
            <code style={{ fontFamily: "monospace", color: "var(--text-ghost)" }}>· source</code>
            <button onClick={() => setPreview(null)} style={{ ...libBtnStyle, marginLeft: "auto" }}>
              Close
            </button>
          </div>
          <pre style={{
            margin: 0, padding: 6, maxHeight: 180, overflow: "auto",
            background: "var(--card)", borderRadius: 4,
            fontFamily: "monospace", fontSize: 10.5, lineHeight: 1.45,
            color: "var(--foreground)", whiteSpace: "pre-wrap",
          }}>
{preview.source}
          </pre>
        </div>
      )}
    </div>
  );
}

const libBtnStyle: React.CSSProperties = {
  display: "inline-flex", alignItems: "center", gap: 3,
  fontSize: 10, padding: "1px 5px", borderRadius: 3,
  border: "1px solid var(--border-subtle)", background: "transparent",
  color: "var(--text-dim)", cursor: "pointer",
};
