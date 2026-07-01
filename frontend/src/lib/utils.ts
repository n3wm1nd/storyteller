import type { WireTick } from "./store";

export type AnnotationMode = "hidden" | "dots" | "expanded";

export function tickChain(ticks: Record<string, WireTick>, head: string | null): WireTick[] {
  if (!head || !ticks[head]) return [];
  const chain: WireTick[] = [];
  let cur: string | null = head;
  const seen = new Set<string>();
  while (cur && ticks[cur] && !seen.has(cur)) {
    seen.add(cur);
    const t = ticks[cur];
    if (t.kind !== "root") chain.push(t);
    cur = t.parent;
  }
  return chain.reverse();
}

export function tickPayload(msg: string): string {
  const nl = msg.indexOf("\n");
  return nl >= 0 ? msg.slice(nl + 1) : msg;
}

export function tickField(tick: WireTick, key: string): string | undefined {
  return tick.fields?.[key];
}

export function statusColor(status: string): string {
  switch (status) {
    case "connected":  return "var(--emerald)";
    case "connecting": return "var(--amber)";
    case "error":      return "var(--rose)";
    default:           return "var(--text-dim)";
  }
}
