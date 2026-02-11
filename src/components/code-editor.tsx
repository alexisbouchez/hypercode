"use client";

import Editor, { type OnMount } from "@monaco-editor/react";
import { MultiFileDiff } from "@pierre/diffs/react";
import { useCallback, useEffect, useRef, useState } from "react";

interface CodeEditorProps {
  value: string;
  onChange: (value: string) => void;
  language?: string;
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

export function CodeEditor({ value, onChange, language = "go", readOnly = false, solution, onRun }: CodeEditorProps) {
  const editorRef = useRef<Parameters<OnMount>[0] | null>(null);
  const onRunRef = useRef(onRun);
  onRunRef.current = onRun;
  const isDark = useIsDark();
  const theme = isDark ? "vs-dark" : "light";

  const handleMount: OnMount = useCallback((editor, monaco) => {
    editorRef.current = editor;
    editor.focus();

    // Register Zig language if not already registered
    if (!monaco.languages.getLanguages().some((l: { id: string }) => l.id === "zig")) {
      monaco.languages.register({ id: "zig" });
      monaco.languages.setMonarchTokensProvider("zig", {
        keywords: [
          "addrspace", "align", "allowzero", "and", "anyframe", "anytype",
          "asm", "async", "await", "break", "callconv", "catch", "comptime",
          "const", "continue", "defer", "else", "enum", "errdefer", "error",
          "export", "extern", "fn", "for", "if", "inline", "linksection",
          "noalias", "nosuspend", "noinline", "opaque", "or", "orelse",
          "packed", "pub", "resume", "return", "struct", "suspend",
          "switch", "test", "threadlocal", "try", "union", "unreachable",
          "undefined", "var", "volatile", "while",
        ],
        builtinTypes: [
          "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "i128", "u128",
          "isize", "usize", "f16", "f32", "f64", "f80", "f128",
          "bool", "void", "noreturn", "type", "anyerror", "comptime_int",
          "comptime_float",
        ],
        builtinFunctions: [
          "@abs", "@addWithOverflow", "@alignCast", "@alignOf", "@as",
          "@atomicLoad", "@atomicRmw", "@atomicStore", "@bitCast",
          "@bitOffsetOf", "@bitSizeOf", "@boolToInt", "@bitReverse",
          "@breakpoint", "@byteSwap", "@call", "@cDefine", "@cImport",
          "@cInclude", "@clz", "@cmpxchgStrong", "@cmpxchgWeak",
          "@compileError", "@compileLog", "@ctz", "@cUndef", "@divExact",
          "@divFloor", "@divTrunc", "@embedFile", "@enumToInt",
          "@errorName", "@errorReturnTrace", "@errorToInt", "@errSetCast",
          "@export", "@extern", "@fence", "@field", "@fieldParentPtr",
          "@floatCast", "@floatToInt", "@frameAddress", "@hasDecl",
          "@hasField", "@import", "@intCast", "@intToEnum", "@intToFloat",
          "@intToPtr", "@max", "@memcpy", "@memset", "@min", "@mod",
          "@mulAdd", "@mulWithOverflow", "@offsetOf", "@panic",
          "@popCount", "@prefetch", "@ptrCast", "@ptrToInt", "@reduce",
          "@rem", "@returnAddress", "@select", "@setAlignStack",
          "@setCold", "@setEvalBranchQuota", "@setFloatMode",
          "@setRuntimeSafety", "@shlExact", "@shlWithOverflow",
          "@shrExact", "@shuffle", "@sizeOf", "@splat", "@src",
          "@sqrt", "@subWithOverflow", "@tagName", "@This", "@trap",
          "@truncate", "@typeInfo", "@typeName", "@TypeOf",
          "@unionInit", "@Vector", "@wasmMemorySize",
          "@wasmMemoryGrow",
        ],
        operators: [
          "=", ">", "<", "!", "~", "?", ":", "==", "<=", ">=", "!=",
          "&&", "||", "++", "--", "+", "-", "*", "/", "&", "|", "^",
          "%", "<<", ">>", "+=", "-=", "*=", "/=", "&=", "|=", "^=",
          "%=", "<<=", ">>=", "=>",
        ],
        symbols: /[=><!~?:&|+\-*/^%]+/,
        tokenizer: {
          root: [
            [/@[a-zA-Z_]\w*/, "keyword"],
            [/[a-z_$][\w$]*/, {
              cases: {
                "@keywords": "keyword",
                "@builtinTypes": "type",
                "@default": "identifier",
              },
            }],
            [/[A-Z][\w$]*/, "type.identifier"],
            { include: "@whitespace" },
            [/[{}()[\]]/, "@brackets"],
            [/@symbols/, {
              cases: {
                "@operators": "operator",
                "@default": "",
              },
            }],
            [/0[xX][0-9a-fA-F_]+/, "number.hex"],
            [/0[oO][0-7_]+/, "number.octal"],
            [/0[bB][01_]+/, "number.binary"],
            [/[0-9][0-9_]*(\.[0-9_]*)?([eE][+-]?[0-9_]+)?/, "number"],
            [/"([^"\\]|\\.)*$/, "string.invalid"],
            [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],
            [/'[^\\']'/, "string"],
            [/'/, "string.invalid"],
          ],
          string: [
            [/[^\\"]+/, "string"],
            [/\\./, "string.escape"],
            [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
          ],
          whitespace: [
            [/[ \t\r\n]+/, "white"],
            [/\/\/.*$/, "comment"],
          ],
        },
      });
    }

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
            lang: language as "go",
          }}
          newFile={{
            name: "Solution",
            contents: solution,
            lang: language as "go",
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
      language={language}
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
