"use client";

import { useState, useEffect, useRef } from "react";
import { useTheme } from "next-themes";
import { codeToHtml } from "shiki";
import type { TestResult } from "@/lib/lessons/types";

function AssemblyView({ code }: { code: string }) {
  const [html, setHtml] = useState<string | null>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const { resolvedTheme } = useTheme();

  useEffect(() => {
    let cancelled = false;
    const theme = resolvedTheme === "light" ? "github-light-default" : "github-dark-default";
    codeToHtml(code, { lang: "asm", theme }).then((result) => {
      if (!cancelled) setHtml(result);
    });
    return () => { cancelled = true; };
  }, [code, resolvedTheme]);

  if (!html) {
    return (
      <pre className="text-foreground whitespace-pre-wrap text-xs leading-relaxed">
        {code}
      </pre>
    );
  }

  return (
    <div
      ref={containerRef}
      className="overflow-x-auto text-xs leading-relaxed [&_pre]:!bg-transparent [&_code]:font-mono"
      dangerouslySetInnerHTML={{ __html: html }}
    />
  );
}

interface OutputPanelProps {
  output: string;
  error: string;
  testResults: TestResult[];
  isRunning: boolean;
  generatedCode?: string;
}

export function OutputPanel({
  output,
  error,
  testResults,
  isRunning,
  generatedCode,
}: OutputPanelProps) {
  const [activeTab, setActiveTab] = useState<"output" | "assembly">("output");

  if (isRunning) {
    return (
      <div className="font-mono text-sm p-4 text-muted-foreground">
        Running...
      </div>
    );
  }

  const hasOutput = output || error;
  const hasTests = testResults.length > 0;
  const hasTabs = !!generatedCode;

  if (!hasOutput && !hasTests && !generatedCode) {
    return (
      <div className="font-mono text-sm p-4 text-muted-foreground">
        Click &quot;Run&quot; to execute your code.
      </div>
    );
  }

  const allPassed = hasTests && testResults.every((t) => t.passed);

  const outputContent = (
    <div className="space-y-3">
      {error && (
        <div className="text-destructive whitespace-pre-wrap">{error}</div>
      )}
      {output && !error && (
        <div>
          <div className="text-muted-foreground text-xs mb-1">Output</div>
          <div className="text-foreground whitespace-pre-wrap">{output}</div>
        </div>
      )}
      {hasTests && (
        <div>
          <div className="text-muted-foreground text-xs mb-2">
            Tests: {testResults.filter((t) => t.passed).length}/{testResults.length} passed
          </div>
          <div className="space-y-1.5">
            {testResults.map((t, i) => (
              <div key={i} className="flex items-start gap-2">
                <span className={t.passed ? "text-emerald-500" : "text-destructive"}>
                  {t.passed ? "[PASS]" : "[FAIL]"}
                </span>
                <div className="flex-1 min-w-0">
                  <span className="text-foreground">{t.name}</span>
                  {!t.passed && (
                    <div className="mt-1 text-xs text-muted-foreground">
                      <div>expected: {JSON.stringify(t.expected)}</div>
                      <div>     got: {JSON.stringify(t.actual)}</div>
                    </div>
                  )}
                </div>
              </div>
            ))}
          </div>
          {allPassed && (
            <div className="mt-3 text-emerald-500 font-semibold" data-testid="all-passed">
              All tests passed!
            </div>
          )}
        </div>
      )}
    </div>
  );

  const assemblyContent = generatedCode ? (
    <div>
      <div className="text-muted-foreground text-xs mb-1">Generated ARM64 Assembly</div>
      <AssemblyView code={generatedCode} />
    </div>
  ) : null;

  if (!hasTabs) {
    return (
      <div className="font-mono text-sm p-4 overflow-auto">
        {outputContent}
      </div>
    );
  }

  return (
    <div className="font-mono text-sm overflow-auto">
      <div className="flex border-b border-border sticky top-0 bg-muted z-10">
        <button
          onClick={() => setActiveTab("output")}
          className={`px-3 py-1.5 text-xs transition-colors ${
            activeTab === "output"
              ? "text-foreground border-b-2 border-primary"
              : "text-muted-foreground hover:text-foreground"
          }`}
        >
          Output
        </button>
        <button
          onClick={() => setActiveTab("assembly")}
          className={`px-3 py-1.5 text-xs transition-colors ${
            activeTab === "assembly"
              ? "text-foreground border-b-2 border-primary"
              : "text-muted-foreground hover:text-foreground"
          }`}
        >
          Assembly
        </button>
      </div>
      <div className="p-4">
        {activeTab === "output" ? outputContent : assemblyContent}
      </div>
    </div>
  );
}
