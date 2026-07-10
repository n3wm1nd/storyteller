// Forward-compatible slot for Text-mode (see fileview.tsx's TextEditPanel)
// token highlighting — e.g. "[[Character Name]]" style references rendered
// with a live decoration as you type, the way Novelcrafter highlights
// character/location mentions inline. Not yet registered in TextEditPanel's
// extensions list: the matching rule (what counts as a token, how it maps to
// a character/file) isn't designed yet. This only proves out the mechanism —
// ProseMirror decorations are non-destructive (never alter the document or
// its markdown serialization), so registering this later is additive.
import { Extension } from "@tiptap/core";
import { Plugin, PluginKey } from "@tiptap/pm/state";
import { Decoration, DecorationSet } from "@tiptap/pm/view";

const TOKEN_RE = /\[\[([^\]]+)\]\]/g;

export const TokenHighlight = Extension.create({
  name: "tokenHighlight",
  addProseMirrorPlugins() {
    return [
      new Plugin({
        key: new PluginKey("tokenHighlight"),
        props: {
          decorations(state) {
            const decorations: Decoration[] = [];
            state.doc.descendants((node, pos) => {
              if (!node.isText || !node.text) return;
              for (const m of node.text.matchAll(TOKEN_RE)) {
                const from = pos + m.index;
                decorations.push(Decoration.inline(from, from + m[0].length, { class: "token-highlight" }));
                // later: characterColor(m[1]) for a per-character inline color
              }
            });
            return DecorationSet.create(state.doc, decorations);
          },
        },
      }),
    ];
  },
});
