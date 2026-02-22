"use client";

import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let isReady = false;

export function initHtmlRunner(): Promise<void> {
  isReady = true;
  return Promise.resolve();
}

export function isHtmlReady(): boolean {
  return isReady;
}

function buildPreviewHtml(code: string): string {
  const trimmed = code.trim().toLowerCase();
  if (trimmed.startsWith("<!doctype") || trimmed.startsWith("<html")) {
    return code;
  }
  return `<!DOCTYPE html><html><head><meta charset="utf-8"><style>body { font-family: sans-serif; padding: 16px; margin: 0; }</style></head><body>${code}</body></html>`;
}

export async function runHtml(code: string): Promise<RunResult> {
  return { stdout: "", stderr: "", error: "", previewHtml: buildPreviewHtml(code) };
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  return tests.map((test) => {
    let actual = "";
    if (test.code) {
      const codeToRun = test.code.replace("{{HTML}}", JSON.stringify(code));
      const originalLog = console.log;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      console.log = (...args: any[]) => {
        actual += args.map((a) => (typeof a === "object" && a !== null ? JSON.stringify(a) : String(a))).join(" ") + "\n";
      };
      try {
        new Function(codeToRun)();
      } catch (_) {
        // ignore
      } finally {
        console.log = originalLog;
      }
    }
    return {
      name: test.name,
      passed: actual === test.expected,
      actual,
      expected: test.expected,
    };
  });
}
