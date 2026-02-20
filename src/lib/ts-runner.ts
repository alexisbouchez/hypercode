import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let tsReady = false;
let tsLoadPromise: Promise<void> | null = null;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let tsLib: any = null;

export function isTsReady(): boolean {
  return tsReady;
}

export function initTsRunner(): Promise<void> {
  if (tsReady) return Promise.resolve();
  if (tsLoadPromise) return tsLoadPromise;

  tsLoadPromise = (async () => {
    try {
      tsLib = await import("typescript");
      tsReady = true;
    } catch (err) {
      tsReady = false;
      tsLoadPromise = null;
      throw err;
    }
  })();

  return tsLoadPromise;
}

function transpile(code: string): string {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const result = tsLib.transpileModule(code, {
    compilerOptions: {
      target: tsLib.ScriptTarget.ES2020,
      module: tsLib.ModuleKind.None,
      strict: false,
    },
  });
  return result.outputText;
}

export async function runTs(code: string): Promise<RunResult> {
  if (!tsReady || !tsLib) {
    return { stdout: "", stderr: "", error: "TypeScript compiler not loaded." };
  }

  try {
    const js = transpile(code);
    let output = "";
    const originalLog = console.log;
    console.log = (...args: unknown[]) => {
      output += args.map((a) => (typeof a === "string" ? a : String(a))).join(" ") + "\n";
    };
    try {
      // eslint-disable-next-line no-new-func
      new Function(js)();
    } finally {
      console.log = originalLog;
    }
    return { stdout: output, stderr: "", error: "" };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts top-level declarations (functions, interfaces, type aliases, enums, classes)
 * from TypeScript source, leaving out standalone top-level calls.
 */
export function extractTsFunctions(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inBlock = false;

  for (const line of lines) {
    const opens = (line.match(/\{/g) || []).length;
    const closes = (line.match(/\}/g) || []).length;

    if (!inBlock && depth === 0) {
      if (
        line.match(/^\s*function\s+\w+/) ||
        line.match(/^\s*(?:const|let|var)\s+\w+\s*=\s*(?:async\s*)?\(/) ||
        line.match(/^\s*interface\s+\w+/) ||
        line.match(/^\s*type\s+\w+\s*=/) ||
        line.match(/^\s*enum\s+\w+/) ||
        line.match(/^\s*class\s+\w+/)
      ) {
        inBlock = true;
        result.push(line);
        depth += opens - closes;
        if (depth <= 0) {
          inBlock = false;
          depth = 0;
        }
      }
      // Skip standalone top-level calls
    } else {
      result.push(line);
      depth += opens - closes;
      if (depth <= 0) {
        inBlock = false;
        depth = 0;
      }
    }
  }

  return result.join("\n").trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractTsFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runTs(codeToRun);
    const hasError = result.error !== "";

    results.push({
      name: test.name,
      passed: !hasError && result.stdout === test.expected,
      actual: hasError ? result.error : result.stdout,
      expected: test.expected,
    });
  }

  return results;
}
