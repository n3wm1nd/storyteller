import type { NextConfig } from "next";

// STATIC_EXPORT=1 (set only by `npm run build:static`, see package.json)
// switches this to a plain static export for story-server to serve
// directly (see app/Server.hs's STATIC_DIR) — everything under src/app is
// "use client" with no API routes/middleware/SSR data fetching, so nothing
// here actually needs a Node server at request time. `next dev` never sets
// this, so the ordinary dev workflow (this file's `rewrites()` included) is
// completely unaffected.
const staticExport = process.env.STATIC_EXPORT === "1";

const nextConfig: NextConfig = {
  allowedDevOrigins: ["10.1.2.1"],
  output: staticExport ? "export" : "standalone",
  reactStrictMode: false,
  typescript: {
    ignoreBuildErrors: true,
  },
  // Static export doesn't support rewrites() at all (build-time error) — in
  // that mode, story-server's own SPA-fallback-to-index.html (staticApp in
  // app/Server.hs) takes over this exact job instead.
  ...(staticExport ? {} : {
    async rewrites() {
      return [{ source: "/:path*", destination: "/" }];
    },
  }),
};

export default nextConfig;
