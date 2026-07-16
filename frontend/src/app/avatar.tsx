"use client";

import { Users } from "lucide-react";
import { branchFileUrl } from "@/lib/ws";

// Shared between sidebar.tsx's flat character list and character-sidebar.tsx's
// scene cards -- a circular avatar.png when the branch has one (see
// Server.Writer.Character's charHasAvatar / Server.Writer.Session.Protocol's
// CharacterSummary.avatar, both existence flags only), falling back to the
// same Users glyph both views already used before avatars existed. The image
// itself is a plain GET at branchFileUrl, not pushed over the wire.
export function CharacterAvatar({ branch, hasAvatar, color, size = 11 }: {
  branch: string;
  hasAvatar: boolean;
  color: string;
  size?: number;
}) {
  if (!hasAvatar) {
    return <Users style={{ width: size, height: size, flexShrink: 0, color }} />;
  }
  return (
    <img
      src={branchFileUrl(branch, "avatar.png")}
      alt=""
      style={{ width: size, height: size, borderRadius: "50%", objectFit: "cover", flexShrink: 0 }}
    />
  );
}
