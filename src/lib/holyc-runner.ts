import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let holycReady = false;
let holycLoading = false;
let holycLoadPromise: Promise<void> | null = null;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let Module: any = null;

export function isHolycReady(): boolean {
  return holycReady;
}

export function isHolycLoading(): boolean {
  return holycLoading;
}

export function initHolycRunner(): Promise<void> {
  if (holycReady) return Promise.resolve();
  if (holycLoadPromise) return holycLoadPromise;

  holycLoadPromise = (async () => {
    holycLoading = true;
    try {
      // Load the Emscripten-generated module — dynamic import bypasses webpack bundling
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const moduleFactory: any = await import(/* webpackIgnore: true */ "/holyc/aiwnios.js" as any);

      Module = await moduleFactory.default({
        // Provide no-op callbacks; they will be replaced per-run
        print: () => {},
        printErr: () => {},
        // Do not auto-run main; we call callMain explicitly
        noInitialRun: true,
      });

      holycReady = true;
    } catch (err) {
      holycReady = false;
      holycLoadPromise = null;
      throw err;
    } finally {
      holycLoading = false;
    }
  })();

  return holycLoadPromise;
}

export async function runHolyC(code: string): Promise<RunResult> {
  if (!holycReady || !Module) {
    return {
      stdout: "",
      stderr: "",
      error: "HolyC runtime is not loaded. Please wait for it to initialize.",
    };
  }

  let stdout = "";
  let stderr = "";

  // Save original callbacks and override for this run
  const savedPrint = Module.print;
  const savedPrintErr = Module.printErr;

  Module.print = (text: string) => {
    stdout += text + "\n";
  };
  Module.printErr = (text: string) => {
    stderr += text + "\n";
  };

  try {
    // Write the user's code to the virtual filesystem
    Module.FS.writeFile("/main.hc", code);

    // Execute the HolyC source file
    Module.callMain(["/main.hc"]);
  } catch (err) {
    const errorMsg = err instanceof Error ? err.message : String(err);
    return {
      stdout,
      stderr,
      error: errorMsg,
    };
  } finally {
    Module.print = savedPrint;
    Module.printErr = savedPrintErr;
  }

  return { stdout, stderr, error: "" };
}

/**
 * Extracts user-defined functions and class definitions from HolyC source.
 * Strips top-level statements (Print calls, variable declarations, etc.)
 * so the extracted functions can be injected into a test template via {{FUNC}}.
 */
export function extractHolycFunctions(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inDefinition = false;

  // Regex for the start of a function definition: ReturnType FuncName(...)
  const funcStartRe = /^(I8|I16|I32|I64|U8|U16|U32|U64|F64|Bool|U0)\s+\w+\s*\(/;
  // Regex for a class definition
  const classStartRe = /^class\s+\w+/;

  for (const line of lines) {
    const trimmed = line.trim();

    if (inDefinition) {
      result.push(line);
      for (const ch of trimmed) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth <= 0) {
        inDefinition = false;
        depth = 0;
      }
    } else if (funcStartRe.test(trimmed) || classStartRe.test(trimmed)) {
      result.push(line);
      for (const ch of trimmed) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth > 0) {
        inDefinition = true;
      }
    } else if (trimmed.startsWith("//") || trimmed === "") {
      // Keep comments and blank lines at top level
      result.push(line);
    }
    // Otherwise: top-level statement (Print, variable init, etc.) — skip it
  }

  return result.join("\n").trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractHolycFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runHolyC(codeToRun);

    const hasError = result.error !== "";
    const actual = hasError ? result.error : result.stdout;

    results.push({
      name: test.name,
      passed: !hasError && result.stdout === test.expected,
      actual,
      expected: test.expected,
    });
  }

  return results;
}
