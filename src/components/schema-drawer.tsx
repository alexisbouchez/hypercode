"use client";

import { useEffect, useState } from "react";
import { useTheme } from "next-themes";
import { codeToHtml } from "shiki";
import { EXERCISE_SCHEMA } from "@/lib/sql-shared";

interface SchemaDrawerProps {
  open: boolean;
  onClose: () => void;
}

export function SchemaDrawer({ open, onClose }: SchemaDrawerProps) {
  const [html, setHtml] = useState<string | null>(null);
  const { resolvedTheme } = useTheme();

  useEffect(() => {
    if (!open) return;
    let cancelled = false;
    const theme =
      resolvedTheme === "light" ? "github-light-default" : "github-dark-default";
    codeToHtml(EXERCISE_SCHEMA.trim(), { lang: "sql", theme }).then((result) => {
      if (!cancelled) setHtml(result);
    });
    return () => {
      cancelled = true;
    };
  }, [open, resolvedTheme]);

  if (!open) return null;

  return (
    <div className="absolute inset-0 z-20 flex flex-col bg-background">
      <div className="px-4 py-2 border-b border-border flex items-center gap-2 shrink-0">
        <span className="text-sm font-semibold text-foreground">Database Schema</span>
        <div className="flex-1" />
        <button
          onClick={onClose}
          className="text-xs text-muted-foreground hover:text-foreground px-2 py-1 rounded hover:bg-muted transition-colors"
        >
          Close
        </button>
      </div>
      <div className="flex-1 overflow-auto p-4">
        {html ? (
          <div
            className="text-sm leading-relaxed [&_pre]:p-4 [&_pre]:rounded-lg [&_code]:font-mono"
            dangerouslySetInnerHTML={{ __html: html }}
          />
        ) : (
          <pre className="text-sm font-mono text-foreground/80 p-4">
            {EXERCISE_SCHEMA.trim()}
          </pre>
        )}
      </div>
    </div>
  );
}
