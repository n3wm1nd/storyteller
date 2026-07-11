import { Trash2 } from "lucide-react";
import { bucketColor } from "./bucket";

// Small round badge shared by tag chips (context-source.tsx) and codex
// cards (codex.tsx) — a number on its bucket's colour, or a trash icon in a
// muted/red treatment. `onClick` absent renders it inert (used wherever a
// view only ever displays a file's resolved bucket, never edits it
// directly).
export function BucketBadge({ bucket, onClick, title }: {
  bucket: number | null;
  onClick?: () => void;
  title: string;
}) {
  const interactive = !!onClick;
  const style: React.CSSProperties = {
    display: "flex", alignItems: "center", justifyContent: "center",
    width: 15, height: 15, borderRadius: "50%", flexShrink: 0,
    fontSize: 9, fontWeight: 700, fontFamily: "monospace",
    border: "none", padding: 0, cursor: interactive ? "pointer" : "default",
    background: bucket === null ? "oklch(0.55 0.15 25 / 0.18)" : bucketColor(bucket, 0.22),
    color: bucket === null ? "oklch(0.65 0.18 25)" : bucketColor(bucket),
  };
  const content = bucket === null ? <Trash2 style={{ width: 9, height: 9 }} /> : bucket;
  return interactive ? (
    <button onClick={onClick} title={title} style={style}>{content}</button>
  ) : (
    <span title={title} style={style}>{content}</span>
  );
}
