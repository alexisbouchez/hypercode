import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

// Module factory cached promise — loaded once, instances created per-run
// eslint-disable-next-line @typescript-eslint/no-explicit-any
let moduleFactoryPromise: Promise<{ default: (...args: any[]) => any }> | null =
  null;

// Noise patterns emitted during HolyC VM initialization/boot
const NOISE_PATTERNS = [
  "Unresolved Reference:",
  "WARNING:",
  "ERROR:",
];

function filterOutput(raw: string): string {
  const lines = raw
    .split("\n")
    .filter((l) => l.trim() && !NOISE_PATTERNS.some((p) => l.includes(p)));
  // FULL_PACKAGE.HC's CmdLine task uses ExePrint2("...#include \"%s\";;") —
  // the ;; operator prints the return value of #include (always "1" = success)
  // before user code runs. Strip it unconditionally since it is always first.
  if (lines[0] === "1") lines.shift();
  return lines.join("\n");
}

function loadModuleFactory() {
  if (!moduleFactoryPromise) {
    moduleFactoryPromise = import(
      /* webpackIgnore: true */ "/holyc/aiwnios.js" as never
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    ) as Promise<{ default: (...args: any[]) => any }>;
  }
  return moduleFactoryPromise;
}

export function initHolycRunner(): Promise<void> {
  // Preload the WASM factory in the background
  return loadModuleFactory().then(() => {});
}

export function isHolycReady(): boolean {
  // In this design, readiness is checked per-run via the module factory
  return moduleFactoryPromise !== null;
}

export function isHolycLoading(): boolean {
  return false; // No separate loading state needed
}

export async function runHolyC(code: string): Promise<RunResult> {
  let factory: Awaited<typeof moduleFactoryPromise>;
  try {
    factory = await loadModuleFactory();
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: `Failed to load HolyC runtime: ${err instanceof Error ? err.message : String(err)}`,
    };
  }

  let stdout = "";
  let stderr = "";

  // Create a fresh Module instance for this run
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let Module: any;
  try {
    Module = await factory!.default({
      print: (text: string) => {
        stdout += text + "\n";
      },
      printErr: (text: string) => {
        stderr += text + "\n";
      },
      noInitialRun: true,
    });
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: `Failed to initialize HolyC runtime: ${err instanceof Error ? err.message : String(err)}`,
    };
  }

  // Write the user's code to the virtual filesystem
  Module.FS.writeFile("/main.hc", code);

  // Enable headless mode: run /main.hc without SDL graphics
  const filePtr = Module.stringToNewUTF8("/main.hc");
  Module._holyc_set_file(filePtr);
  Module._emscripten_builtin_free(filePtr);

  // Start the runtime — in EMSCRIPTEN mode this registers the main loop and
  // throws ExitStatus (sim_infinite_loop=1); we catch and ignore it.
  try {
    Module.callMain([]);
  } catch (e) {
    // ExitStatus throw is expected from emscripten_set_main_loop(…,…,1)
    // Any other error is a real failure
    if (
      e instanceof Error &&
      !e.message.includes("ExitStatus") &&
      !(e as { status?: number }).status !== undefined
    ) {
      return {
        stdout: filterOutput(stdout),
        stderr,
        error: e.message,
      };
    }
  }

  // Drive execution until done.
  // - In the browser: emscripten_set_main_loop fires via requestAnimationFrame;
  //   we also call holyc_step() from a polling interval to cover both cases.
  // - In Node.js: no requestAnimationFrame, so holyc_step() is the only driver.
  const isBrowser = typeof window !== "undefined";

  await new Promise<void>((resolve, reject) => {
    const TIMEOUT_MS = 30_000;
    const POLL_MS = isBrowser ? 50 : 0; // browser: 50ms polls; Node: tight loop
    const MAX_STEPS = 10_000;
    let steps = 0;

    const timeoutId = setTimeout(() => {
      reject(new Error("HolyC execution timed out"));
    }, TIMEOUT_MS);

    function step() {
      if (Module._holyc_is_done()) {
        clearTimeout(timeoutId);
        resolve();
        return;
      }
      // Manually drive one step (safe alongside emscripten main loop because
      // holyc_step() is idempotent when called between requestAnimationFrame ticks)
      Module._holyc_step();
      steps++;
      if (steps >= MAX_STEPS) {
        clearTimeout(timeoutId);
        reject(new Error("HolyC execution step limit exceeded"));
        return;
      }
      if (POLL_MS > 0) {
        setTimeout(step, POLL_MS);
      } else {
        // Node.js: synchronous tight loop
        step();
      }
    }

    // Kick off the polling loop
    if (POLL_MS > 0) {
      setTimeout(step, POLL_MS);
    } else {
      step();
    }
  });

  return {
    stdout: filterOutput(stdout),
    stderr,
    error: "",
  };
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

export async function runTests(
  code: string,
  tests: Test[]
): Promise<TestResult[]> {
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
