"use client";

import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

const THREE_CDN = "https://cdn.jsdelivr.net/npm/three@0.169.0/build/three.min.js";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let THREE: any = null;
let isReady = false;
let loadPromise: Promise<void> | null = null;

export function initThreeJsRunner(): Promise<void> {
  if (loadPromise) return loadPromise;
  loadPromise = new Promise<void>((resolve, reject) => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    if ((window as any).THREE) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      THREE = (window as any).THREE;
      isReady = true;
      resolve();
      return;
    }
    const script = document.createElement("script");
    script.src = THREE_CDN;
    script.crossOrigin = "anonymous";
    script.onload = () => {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      THREE = (window as any).THREE;
      isReady = true;
      resolve();
    };
    script.onerror = () => reject(new Error("Failed to load Three.js from CDN"));
    document.head.appendChild(script);
  });
  return loadPromise;
}

export function isThreeJsReady(): boolean {
  return isReady;
}

function buildPreviewHtml(code: string): string {
  const indented = code.split("\n").map((l) => "    " + l).join("\n");
  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { background: #0a0a0f; overflow: hidden; width: 100vw; height: 100vh; }
    canvas { display: block; }
    #error { color: #f87171; padding: 16px; font-family: monospace; font-size: 12px; white-space: pre-wrap; }
  </style>
</head>
<body>
  <script src="${THREE_CDN}" crossorigin="anonymous"><\/script>
  <script>
    window.addEventListener('load', function () {
      try {
${indented}
      } catch (e) {
        const div = document.createElement('div');
        div.id = 'error';
        div.textContent = e.message;
        document.body.appendChild(div);
      }
    });
  <\/script>
</body>
</html>`;
}

/** Extract top-level function declarations and arrow-function assignments. */
function extractFunctions(code: string): string {
  const lines = code.split("\n");
  const blocks: string[] = [];
  let depth = 0;
  let inBlock = false;
  let current: string[] = [];

  for (const line of lines) {
    const trimmed = line.trim();
    const isFuncDecl = /^function\s+\w+/.test(trimmed);
    const isArrowOrExpr = /^(const|let|var)\s+\w+\s*=\s*(async\s+)?(function|\()/.test(trimmed);

    if (!inBlock && (isFuncDecl || isArrowOrExpr)) {
      inBlock = true;
      current = [line];
      depth = 0;
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      // One-liner arrow: const f = x => x;
      if (depth === 0 && !trimmed.endsWith("{")) {
        blocks.push(line);
        current = [];
        inBlock = false;
      }
    } else if (inBlock) {
      current.push(line);
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth <= 0) {
        blocks.push(current.join("\n"));
        current = [];
        inBlock = false;
        depth = 0;
      }
    }
  }

  return blocks.join("\n\n");
}

export async function runThreeJs(code: string): Promise<RunResult> {
  const previewHtml = buildPreviewHtml(code);

  let stdout = "";
  let errorMsg = "";

  const originalLog = console.log;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  console.log = (...args: any[]) => {
    stdout += args.map((a) => (typeof a === "object" && a !== null ? JSON.stringify(a) : String(a))).join(" ") + "\n";
  };

  try {
    const fn = new Function("THREE", code);
    fn(THREE);
  } catch (e: unknown) {
    const msg = (e as Error).message ?? String(e);
    // Suppress expected browser-API errors (WebGL, canvas, DOM, rAF) that happen in test context
    const isExpected = /webgl|canvas|document|window|requestanimationframe|getcontext/i.test(msg);
    if (!isExpected) errorMsg = msg;
  } finally {
    console.log = originalLog;
  }

  return { stdout, stderr: "", error: errorMsg, previewHtml };
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const userFunctions = extractFunctions(code);

  return tests.map((test) => {
    const testCode = test.code ? test.code.replace("{{FUNC}}", userFunctions) : code;

    let actual = "";
    const originalLog = console.log;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    console.log = (...args: any[]) => {
      actual += args.map((a) => (typeof a === "object" && a !== null ? JSON.stringify(a) : String(a))).join(" ") + "\n";
    };

    try {
      const fn = new Function("THREE", testCode);
      fn(THREE);
    } catch (_) {
      // Ignore errors silently
    } finally {
      console.log = originalLog;
    }

    return {
      name: test.name,
      passed: actual === test.expected,
      actual,
      expected: test.expected,
    };
  });
}
