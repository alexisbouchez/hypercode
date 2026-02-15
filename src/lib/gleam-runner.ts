import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let gleamReady = false;
let gleamLoading = false;
let gleamLoadPromise: Promise<void> | null = null;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let wasm: any = null;
let projectId = 0;
let stdlibWritten = false;

export function isGleamReady(): boolean {
  return gleamReady;
}

export function isGleamLoading(): boolean {
  return gleamLoading;
}

export function initGleamRunner(): Promise<void> {
  if (gleamReady) return Promise.resolve();
  if (gleamLoadPromise) return gleamLoadPromise;

  gleamLoadPromise = (async () => {
    gleamLoading = true;
    try {
      // Load WASM compiler -- dynamic URL import bypasses TS module resolution
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const mod: any = await import(/* webpackIgnore: true */ "/gleam/gleam_wasm.js" as any);
      await mod.default();
      mod.initialise_panic_hook(false);
      wasm = mod;

      // Load stdlib sources and write them to the virtual FS
      const stdlibResponse = await fetch("/gleam/stdlib-sources.json");
      if (!stdlibResponse.ok) {
        throw new Error(`Failed to load stdlib sources: ${stdlibResponse.status}`);
      }
      const stdlibSources: Record<string, string> = await stdlibResponse.json();

      for (const [name, code] of Object.entries(stdlibSources)) {
        wasm.write_module(projectId, name, code);
      }
      stdlibWritten = true;

      gleamReady = true;
    } catch (err) {
      gleamReady = false;
      gleamLoadPromise = null;
      throw err;
    } finally {
      gleamLoading = false;
    }
  })();

  return gleamLoadPromise;
}

export async function runGleam(code: string): Promise<RunResult> {
  if (!gleamReady || !wasm) {
    return {
      stdout: "",
      stderr: "",
      error: "Gleam runtime is not loaded. Please wait for it to initialize.",
    };
  }

  try {
    // If the project got into a bad state, create a fresh one
    if (!stdlibWritten) {
      projectId++;
      const stdlibResponse = await fetch("/gleam/stdlib-sources.json");
      const stdlibSources: Record<string, string> = await stdlibResponse.json();
      for (const [name, src] of Object.entries(stdlibSources)) {
        wasm.write_module(projectId, name, src);
      }
      stdlibWritten = true;
    }

    // Write user code
    wasm.write_module(projectId, "main", code);

    // Compile
    wasm.compile_package(projectId, "javascript");

    // Collect warnings
    const warnings: string[] = [];
    while (true) {
      const w = wasm.pop_warning(projectId);
      if (!w) break;
      warnings.push(w.trimStart());
    }

    // Read compiled JS
    const js: string | undefined = wasm.read_compiled_javascript(projectId, "main");
    if (!js) {
      return { stdout: "", stderr: "", error: "Compilation produced no output" };
    }

    // Rewrite imports to full URLs (blob: URLs can't resolve absolute paths)
    const origin = window.location.origin;
    const rewrittenJs = js.replaceAll(
      /from\s+"\.\/(.+)"/g,
      `from "${origin}/gleam/precompiled/$1"`,
    );

    // Execute via blob URL with console.log interception
    const result = await executeJs(rewrittenJs);

    return {
      stdout: result.stdout,
      stderr: warnings.join("\n"),
      error: "",
    };
  } catch (err) {
    // On compilation error, reset the project to clean state
    const errorMsg = err instanceof Error ? err.message : String(err);

    // If the compiler threw, we might need to recreate the project
    if (errorMsg.includes("panic") || errorMsg.includes("unreachable")) {
      stdlibWritten = false;
    }

    return {
      stdout: "",
      stderr: "",
      error: cleanError(errorMsg),
    };
  }
}

function cleanError(error: string): string {
  // Strip ANSI escape codes
  return error.replace(/\x1B\[[0-9;]*[a-zA-Z]/g, "");
}

async function executeJs(js: string): Promise<{ stdout: string }> {
  let stdout = "";

  // Create a blob URL for the compiled JS module
  const blob = new Blob([js], { type: "text/javascript" });
  const url = URL.createObjectURL(blob);

  // Intercept console.log
  const origLog = console.log;
  const origError = console.error;
  const origWarn = console.warn;

  console.log = (...args: unknown[]) => {
    stdout += args.map(formatValue).join(" ") + "\n";
  };
  console.error = (...args: unknown[]) => {
    stdout += args.map(formatValue).join(" ") + "\n";
  };
  console.warn = (...args: unknown[]) => {
    stdout += args.map(formatValue).join(" ") + "\n";
  };

  try {
    const mod = await import(/* webpackIgnore: true */ url);
    if (typeof mod.main === "function") {
      mod.main();
    }
  } finally {
    console.log = origLog;
    console.error = origError;
    console.warn = origWarn;
    URL.revokeObjectURL(url);
  }

  return { stdout };
}

function formatValue(v: unknown): string {
  if (typeof v === "string") return v;
  if (v === null) return "Nil";
  if (v === undefined) return "Nil";
  return String(v);
}

/**
 * Extracts user-defined functions from Gleam source.
 * Strips import lines and the pub fn main() block.
 */
export function extractGleamFunctions(code: string): string {
  let result = code;

  // Remove import lines
  result = result.replace(/^\s*import\s+.+$/gm, "");

  // Remove pub fn main()
  const mainRegex = /pub\s+fn\s+main\s*\(\s*\)\s*(\{|->\s*\w+\s*\{)/;
  const match = mainRegex.exec(result);
  if (match) {
    const startIndex = match.index;
    let braceCount = 0;
    let endIndex = startIndex;
    let foundOpen = false;

    for (let i = startIndex; i < result.length; i++) {
      if (result[i] === "{") {
        braceCount++;
        foundOpen = true;
      } else if (result[i] === "}") {
        braceCount--;
        if (foundOpen && braceCount === 0) {
          endIndex = i + 1;
          break;
        }
      }
    }

    result = result.slice(0, startIndex) + result.slice(endIndex);
  }

  return result.trim();
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractGleamFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runGleam(codeToRun);

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
