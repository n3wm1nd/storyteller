// Pure bucket-numbering logic shared by every bucket-picker UI (currently
// context-source.tsx's tree editor and codex.tsx's card grid) — both edit
// the same lib/settingsStore.ts ContextFilter shape, just render it
// differently, so the badge/cycling/colour logic lives here once rather
// than being forked per view.

// Cycle order for a badge click: trash, then bucket 1..MAX_BUCKET, then back
// to trash. Small on purpose — this is for "a handful of named groups"
// (WRITER.md's outline/notes/chapters split has three), not a
// general-purpose numbering scheme; nothing stops a bucket above this from
// being reached by editing settings storage directly; a badge can only
// reach one via serial clicks.
export const MAX_BUCKET = 4;

// Stable colour per bucket number, cycling through a small palette — used
// for both a tag's own badge and a matching file's badge in the tree, so
// the two are visually the same group at a glance. Trash gets its own fixed
// (muted/red) treatment, never one of these.
const BUCKET_HUES = [65, 200, 320, 140]; // amber, blue, magenta, green

export function bucketColor(bucket: number, alpha = 1): string {
  const hue = BUCKET_HUES[(bucket - 1) % BUCKET_HUES.length];
  return `oklch(0.75 0.13 ${hue} / ${alpha})`;
}

export function nextBucket(current: number | null): number | null {
  if (current === null) return 1;
  if (current >= MAX_BUCKET) return null;
  return current + 1;
}
