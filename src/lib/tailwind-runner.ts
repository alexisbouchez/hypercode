"use client";

import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let isReady = false;

export function initTailwindRunner(): Promise<void> {
  isReady = true;
  return Promise.resolve();
}

export function isTailwindReady(): boolean {
  return isReady;
}

function buildTailwindPreviewHtml(code: string): string {
  return `<!DOCTYPE html><html><head><meta charset="utf-8"><script src="https://cdn.tailwindcss.com" crossorigin="anonymous"><\/script></head><body class="p-4 bg-white">${code}</body></html>`;
}

export async function runTailwind(code: string): Promise<RunResult> {
  return { stdout: "", stderr: "", error: "", previewHtml: buildTailwindPreviewHtml(code) };
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
