import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Storyteller",
  description: "LLM-powered story development system",
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body>{children}</body>
    </html>
  );
}
