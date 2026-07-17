import { describe, expect, test } from "bun:test";
import {
  activeCharacterBranches,
  allPresentCharacters,
  basenameNoExt,
  flattenLore,
  nearestJournalMarker,
  presentDuringAtoms,
  promptGroupForAtom,
  splitQuestionAnswer,
  tailLeadTicks,
  tickChain,
  tickPreview,
} from "./utils";
import type { LoreNode, WireTick } from "./ws";

function tick(id: string, parent: string | null, kind: string, overrides: Partial<WireTick> = {}): WireTick {
  return { tickId: id, kind, refs: [], message: "", parent, ...overrides };
}

// root -> a1 (atom) -> p1 (prompt) -> a2 (atom) -> a3 (atom) -> n1 (note)
function sampleTicks(): Record<string, WireTick> {
  const ts = [
    tick("root", null, "root"),
    tick("a1", "root", "atom"),
    tick("p1", "a1", "prompt"),
    tick("a2", "p1", "atom"),
    tick("a3", "a2", "atom"),
    tick("n1", "a3", "note"),
  ];
  return Object.fromEntries(ts.map((t) => [t.tickId, t]));
}

describe("tickChain", () => {
  const ticks = sampleTicks();

  test("no head yields an empty chain", () => {
    expect(tickChain(ticks, null)).toEqual([]);
  });

  test("unknown head yields an empty chain", () => {
    expect(tickChain(ticks, "nope")).toEqual([]);
  });

  test("walks parent links from head back to (excluding) root, oldest first", () => {
    expect(tickChain(ticks, "n1").map((t) => t.tickId)).toEqual(["a1", "p1", "a2", "a3", "n1"]);
  });

  test("stops on a cycle instead of looping forever", () => {
    const cyclic: Record<string, WireTick> = {
      a: tick("a", "b", "atom"),
      b: tick("b", "a", "atom"),
    };
    expect(tickChain(cyclic, "a").map((t) => t.tickId)).toEqual(["b", "a"]);
  });
});

describe("tailLeadTicks", () => {
  test("no atoms in the chain: nothing to map", () => {
    expect(tailLeadTicks([tick("n1", "root", "note")])).toEqual(new Map());
  });

  test("single atom with a trailing run: pivot is the tick right after the last trailer", () => {
    const chain = [tick("a1", "root", "atom"), tick("p1", "a1", "presence"), tick("n1", "p1", "note")];
    expect(tailLeadTicks(chain)).toEqual(new Map());
  });

  test("two atoms: the first atom's lead is the tick right after its own trailing run", () => {
    const chain = [
      tick("a1", "root", "atom"),
      tick("presence1", "a1", "presence"),
      tick("a2", "presence1", "atom"),
    ];
    expect(tailLeadTicks(chain)).toEqual(new Map([["a1", "a2"]]));
  });

  test("an atom with no trailing ticks: its own tick is the pivot, so the lead is simply the next atom", () => {
    const chain = [tick("a1", "root", "atom"), tick("a2", "a1", "atom")];
    expect(tailLeadTicks(chain)).toEqual(new Map([["a1", "a2"]]));
  });

  test("the last atom has no lead entry when nothing trails it", () => {
    const chain = [tick("a1", "root", "atom"), tick("a2", "a1", "atom"), tick("a3", "a2", "atom")];
    const leads = tailLeadTicks(chain);
    expect(leads.has("a3")).toBe(false);
  });
});

describe("promptGroupForAtom", () => {
  const ticks = sampleTicks();

  test("unknown atom id returns null", () => {
    expect(promptGroupForAtom(ticks, "n1", "nope")).toBeNull();
  });

  test("an atom with no preceding prompt tick returns null", () => {
    expect(promptGroupForAtom(ticks, "n1", "a1")).toBeNull();
  });

  test("collects every contiguous atom after the nearest preceding prompt", () => {
    const result = promptGroupForAtom(ticks, "n1", "a3");
    expect(result?.promptTick.tickId).toBe("p1");
    expect(result?.atomTickIds).toEqual(["a2", "a3"]);
  });

  test("a non-atom trailing tick (note) does not get pulled into the group", () => {
    const result = promptGroupForAtom(ticks, "n1", "a2");
    expect(result?.atomTickIds).toEqual(["a2", "a3"]);
  });
});

describe("activeCharacterBranches / allPresentCharacters / presentDuringAtoms", () => {
  const ts = [
    tick("root", null, "root"),
    tick("enter-alice", "root", "presence", { fields: { character: "character/alice", event: "enter" } }),
    tick("a1", "enter-alice", "atom"),
    tick("leave-alice", "a1", "presence", { fields: { character: "character/alice", event: "leave" } }),
    tick("enter-bob", "leave-alice", "presence", { fields: { character: "character/bob", event: "enter" } }),
    tick("a2", "enter-bob", "atom"),
  ];
  const ticks = Object.fromEntries(ts.map((t) => [t.tickId, t]));

  test("active branches reflect the last enter/leave, in first-entered order", () => {
    expect(activeCharacterBranches(ticks, "a2")).toEqual(["character/bob"]);
  });

  test("re-entering after leaving doesn't reorder in allPresentCharacters", () => {
    expect(allPresentCharacters(ticks, "a2")).toEqual(["character/alice", "character/bob"]);
  });

  test("presentDuringAtoms only marks atoms written while that character was active", () => {
    expect([...presentDuringAtoms(ticks, "a2", "character/alice")]).toEqual(["a1"]);
    expect([...presentDuringAtoms(ticks, "a2", "character/bob")]).toEqual(["a2"]);
  });
});

describe("nearestJournalMarker", () => {
  const sceneTs = [
    tick("root", null, "root"),
    tick("s1", "root", "atom"),
    tick("s2", "s1", "atom"),
    tick("s3", "s2", "atom"),
  ];
  const sceneTicks = Object.fromEntries(sceneTs.map((t) => [t.tickId, t]));

  const journalTs = [
    tick("jroot", null, "root"),
    tick("j1", "jroot", "atom", { refs: ["s1"] }),
    tick("j2", "j1", "atom", { refs: ["s3"] }),
  ];
  const journalTicks = Object.fromEntries(journalTs.map((t) => [t.tickId, t]));

  test("no scene marker: no journal marker", () => {
    expect(nearestJournalMarker(journalTicks, "j2", sceneTicks, "s3", null)).toBeNull();
  });

  test("scene marker not found on the scene chain: no journal marker", () => {
    expect(nearestJournalMarker(journalTicks, "j2", sceneTicks, "s3", "nope")).toBeNull();
  });

  test("picks the last journal atom whose ref sits at or before the marker", () => {
    expect(nearestJournalMarker(journalTicks, "j2", sceneTicks, "s3", "s2")).toBe("j1");
  });

  test("a journal atom referencing exactly the marker position counts", () => {
    expect(nearestJournalMarker(journalTicks, "j2", sceneTicks, "s3", "s3")).toBe("j2");
  });

  test("marker before every journal ref: no match", () => {
    const lateJournalTs = [tick("jroot", null, "root"), tick("j1", "jroot", "atom", { refs: ["s3"] })];
    const lateJournalTicks = Object.fromEntries(lateJournalTs.map((t) => [t.tickId, t]));
    expect(nearestJournalMarker(lateJournalTicks, "j1", sceneTicks, "s3", "s1")).toBeNull();
  });
});

describe("splitQuestionAnswer", () => {
  test("splits on the NUL separator", () => {
    expect(splitQuestionAnswer("what happened?\0nothing much")).toEqual(["what happened?", "nothing much"]);
  });

  test("missing separator: whole message is the answer, question is empty", () => {
    expect(splitQuestionAnswer("just an answer")).toEqual(["", "just an answer"]);
  });
});

describe("tickPreview", () => {
  test("collapses internal whitespace/newlines and trims", () => {
    expect(tickPreview("  hello\n\n  world  ")).toBe("hello world");
  });

  test("truncates with an ellipsis past maxLen", () => {
    expect(tickPreview("abcdefghij", 5)).toBe("abcde…");
  });

  test("leaves short text untouched", () => {
    expect(tickPreview("short", 5)).toBe("short");
  });
});

describe("basenameNoExt", () => {
  test("strips directory and extension", () => {
    expect(basenameNoExt("lore/alice.md")).toBe("alice");
  });

  test("no extension: returns the basename as-is", () => {
    expect(basenameNoExt("lore/README")).toBe("README");
  });

  test("a leading dot is not treated as an extension separator", () => {
    expect(basenameNoExt(".gitignore")).toBe(".gitignore");
  });
});

describe("flattenLore", () => {
  function node(path: string, children: LoreNode[] = []): LoreNode {
    return { path, name: path, blurb: "", aliases: [], children };
  }

  test("a tree with children only keeps the leaves", () => {
    const tree = [node("folder", [node("folder/a.md"), node("folder/b.md")])];
    expect(flattenLore(tree).map((n) => n.path)).toEqual(["folder/a.md", "folder/b.md"]);
  });

  test("depth-first across siblings: a childless node is itself a leaf", () => {
    const tree = [node("x.md"), node("folder", [node("folder/a.md")]), node("y.md")];
    expect(flattenLore(tree).map((n) => n.path)).toEqual(["x.md", "folder/a.md", "y.md"]);
  });
});
