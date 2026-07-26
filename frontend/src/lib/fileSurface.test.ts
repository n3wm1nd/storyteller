import { describe, expect, test } from "bun:test";
import { fileSurfaceOf, centerTabsFor } from "./fileSurface";

describe("fileSurfaceOf", () => {
  test("ordinary story files are prose", () => {
    expect(fileSurfaceOf("master", "chapters/ch1.md")).toBe("prose");
    expect(fileSurfaceOf("master", "lore/magic.md")).toBe("prose");
    expect(fileSurfaceOf("character/alice", "journal.md")).toBe("prose");
  });

  test("a .dsl is the DSL surface wherever it lives", () => {
    expect(fileSurfaceOf("contexts", "context/lore.dsl")).toBe("dsl");
    expect(fileSurfaceOf("master", "scratch.dsl")).toBe("dsl");
  });

  // The prompts branch holds nothing that isn't prompt material, and its
  // .md files would otherwise read as ordinary prose.
  test("everything on the prompts branch is the prompt surface", () => {
    expect(fileSurfaceOf("prompts", "agent/writer.md")).toBe("prompt");
    expect(fileSurfaceOf("prompts", "agent/writer.llmsettings.yaml")).toBe("prompt");
  });

  test("the same path is prose on a story branch and a prompt on the prompts branch", () => {
    expect(fileSurfaceOf("master", "agent/writer.md")).toBe("prose");
    expect(fileSurfaceOf("prompts", "agent/writer.md")).toBe("prompt");
  });

  test("no file selected is prose — the surface only means anything with one", () => {
    expect(fileSurfaceOf("master", null)).toBe("prose");
  });
});

describe("centerTabsFor", () => {
  // Agents is a settings surface over whatever is open, so it survives
  // everywhere; what it *lists* is filtered separately (agentsForSurface).
  test("every surface keeps File, Ticks and Agents", () => {
    for (const [surface, path] of [["prose", "chapters/ch1.md"], ["dsl", "context/lore.dsl"], ["prompt", "agent/writer.md"]] as const) {
      const tabs = centerTabsFor(surface, path);
      expect(tabs).toContain("file");
      expect(tabs).toContain("ticks");
      expect(tabs).toContain("agents");
    }
  });

  // Chat is this file's own conversation with a writing agent — a program
  // or a prompt override doesn't have one.
  test("Chat only for a prose chat/ file", () => {
    expect(centerTabsFor("prose", "chat/brainstorm.md")).toContain("chat");
    expect(centerTabsFor("prose", "chapters/ch1.md")).not.toContain("chat");
    expect(centerTabsFor("dsl", "chat/weird.dsl")).not.toContain("chat");
    expect(centerTabsFor("prompt", "agent/writer.md")).not.toContain("chat");
  });

  test("File comes first, so it's what a surface switch can always fall back to", () => {
    expect(centerTabsFor("prose", "chat/x.md")[0]).toBe("file");
    expect(centerTabsFor("dsl", "context/lore.dsl")[0]).toBe("file");
  });
});
