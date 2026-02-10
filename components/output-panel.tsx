"use client";

import type { TestResult } from "@/lib/lessons/types";

interface OutputPanelProps {
  output: string;
  error: string;
  testResults: TestResult[];
  isRunning: boolean;
}

export function OutputPanel({ output, error, testResults, isRunning }: OutputPanelProps) {
  if (isRunning) {
    return (
      <div className="font-mono text-sm p-4 text-muted-foreground">
        Running...
      </div>
    );
  }

  const hasOutput = output || error;
  const hasTests = testResults.length > 0;

  if (!hasOutput && !hasTests) {
    return (
      <div className="font-mono text-sm p-4 text-muted-foreground">
        Click &quot;Run&quot; to execute your code.
      </div>
    );
  }

  const allPassed = hasTests && testResults.every((t) => t.passed);

  return (
    <div className="font-mono text-sm p-4 space-y-3 overflow-auto">
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
            <div className="mt-3 text-emerald-500 font-semibold">
              All tests passed!
            </div>
          )}
        </div>
      )}
    </div>
  );
}
