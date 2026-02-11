"use client";

import Editor, { type OnMount } from "@monaco-editor/react";
import { MultiFileDiff } from "@pierre/diffs/react";
import { useCallback, useEffect, useRef, useState } from "react";

interface CodeEditorProps {
  value: string;
  onChange: (value: string) => void;
  readOnly?: boolean;
  solution?: string;
  onRun?: () => void;
}

function useIsDark() {
  const [isDark, setIsDark] = useState(false);

  useEffect(() => {
    const root = document.documentElement;
    setIsDark(root.classList.contains("dark"));

    const observer = new MutationObserver(() => {
      setIsDark(root.classList.contains("dark"));
    });
    observer.observe(root, { attributes: true, attributeFilter: ["class"] });

    const mq = window.matchMedia("(prefers-color-scheme: dark)");
    const handler = (e: MediaQueryListEvent) => {
      if (!root.classList.contains("dark") && !root.classList.contains("light")) {
        setIsDark(e.matches);
      }
    };
    mq.addEventListener("change", handler);
    if (!root.classList.contains("dark") && !root.classList.contains("light")) {
      setIsDark(mq.matches);
    }

    return () => {
      observer.disconnect();
      mq.removeEventListener("change", handler);
    };
  }, []);

  return isDark;
}

const editorOptions = {
  fontSize: 14,
  fontFamily: "var(--font-geist-mono), 'Fira Code', 'Cascadia Code', Menlo, monospace",
  fontLigatures: true,
  minimap: { enabled: false },
  scrollBeyondLastLine: false,
  padding: { top: 16, bottom: 16 },
  lineNumbers: "on" as const,
  renderLineHighlight: "line" as const,
  tabSize: 4,
  insertSpaces: false,
  automaticLayout: true,
  wordWrap: "on" as const,
  bracketPairColorization: { enabled: true },
  guides: { indentation: true },
  suggest: { showKeywords: true },
  smoothScrolling: true,
  cursorBlinking: "smooth" as const,
  cursorSmoothCaretAnimation: "on" as const,
};

export function CodeEditor({ value, onChange, readOnly = false, solution, onRun }: CodeEditorProps) {
  const editorRef = useRef<Parameters<OnMount>[0] | null>(null);
  const onRunRef = useRef(onRun);
  onRunRef.current = onRun;
  const isDark = useIsDark();
  const theme = isDark ? "vs-dark" : "light";

  const handleMount: OnMount = useCallback((editor, monaco) => {
    editorRef.current = editor;
    editor.focus();

    editor.addAction({
      id: "run-code",
      label: "Run Code",
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
      run: () => onRunRef.current?.(),
    });

  }, []);

  if (solution !== undefined) {
    return (
      <div className="h-full overflow-auto">
        <MultiFileDiff
          oldFile={{
            name: "Your Code",
            contents: value,
            lang: "go",
          }}
          newFile={{
            name: "Solution",
            contents: solution,
            lang: "go",
          }}
          options={{
            theme: {
              light: "github-light-default",
              dark: "github-dark-default",
            },
            themeType: isDark ? "dark" : "light",
            diffStyle: "split",
            diffIndicators: "bars",
            lineDiffType: "word-alt",
            overflow: "wrap",
          }}
        />
      </div>
    );
  }

  return (
    <Editor
      height="100%"
      language="go"
      theme={theme}
      value={value}
      onChange={(v) => onChange(v ?? "")}
      onMount={handleMount}
      options={{
        ...editorOptions,
        readOnly,
      }}
    />
  );
}
