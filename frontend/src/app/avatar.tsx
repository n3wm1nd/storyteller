"use client";

import { useState } from "react";
import { Users } from "lucide-react";
import { useFloating, offset, flip, shift, autoUpdate } from "@floating-ui/react";
import { branchFileUrl } from "@/lib/ws";

// Shared between sidebar.tsx's flat character list and character-sidebar.tsx's
// scene cards -- a circular avatar.png when the branch has one (see
// Server.Writer.Character's charHasAvatar / Server.Writer.Session.Protocol's
// CharacterSummary.avatar, both existence flags only), falling back to the
// same Users glyph both views already used before avatars existed. The image
// itself is a plain GET at branchFileUrl, not pushed over the wire.
//
// A circular thumbnail this small (11-14px, see both call sites) is too
// small to actually recognize a face in -- hovering shows a real-size
// preview via Floating UI, same flip()/shift() edge-avoidance
// fileview.tsx's mode dropdown already uses, since both sidebars this
// mounts in are narrow enough to sit flush against a screen edge.
export function CharacterAvatar({ branch, hasAvatar, color, size = 11, fallback }: {
  branch: string;
  hasAvatar: boolean;
  color: string;
  size?: number;
  // character-sidebar.tsx's scene cards use their own colored identity dot
  // (see characterColor) as the no-avatar case instead of this generic
  // Users glyph -- everything else about this component (avatar rendering,
  // hover preview) is identical either way.
  fallback?: React.ReactNode;
}) {
  const [hovered, setHovered] = useState(false);
  const preview = useFloating({
    open: hovered,
    placement: "right",
    middleware: [offset(8), flip(), shift({ padding: 8 })],
    whileElementsMounted: autoUpdate,
  });

  if (!hasAvatar) {
    return fallback ?? <Users style={{ width: size, height: size, flexShrink: 0, color }} />;
  }

  const url = branchFileUrl(branch, "avatar.png");

  return (
    <>
      <img
        ref={preview.refs.setReference}
        src={url}
        alt=""
        onMouseEnter={() => setHovered(true)}
        onMouseLeave={() => setHovered(false)}
        style={{ width: size, height: size, borderRadius: "50%", objectFit: "cover", flexShrink: 0 }}
      />
      {hovered && (
        <div
          ref={preview.refs.setFloating}
          style={{
            ...preview.floatingStyles, zIndex: 20, pointerEvents: "none",
            background: "var(--card)", border: "1px solid var(--border)", borderRadius: 8,
            boxShadow: "0 4px 16px oklch(0 0 0 / 0.4)", padding: 4,
          }}
        >
          <img src={url} alt="" style={{ width: 160, height: 160, borderRadius: 6, objectFit: "cover", display: "block" }} />
        </div>
      )}
    </>
  );
}
