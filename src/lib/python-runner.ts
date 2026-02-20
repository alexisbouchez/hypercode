import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

const PYODIDE_INDEX_URL = "https://cdn.jsdelivr.net/pyodide/v0.26.2/full/";
const PYODIDE_SCRIPT_URL = `${PYODIDE_INDEX_URL}pyodide.js`;

let pyodide: any = null;
let isReady = false;
let loadPromise: Promise<void> | null = null;

export function isPythonReady(): boolean {
  return isReady;
}

export function initPythonRunner(): Promise<void> {
  if (isReady) return Promise.resolve();
  if (loadPromise) return loadPromise;

  loadPromise = new Promise<void>((resolve, reject) => {
    if ((window as any).loadPyodide && pyodide) {
      isReady = true;
      resolve();
      return;
    }

    const script = document.createElement("script");
    script.src = PYODIDE_SCRIPT_URL;
    script.onload = async () => {
      try {
        pyodide = await (window as any).loadPyodide({ indexURL: PYODIDE_INDEX_URL });
        isReady = true;
        resolve();
      } catch (e) {
        loadPromise = null;
        reject(e);
      }
    };
    script.onerror = () => {
      loadPromise = null;
      reject(new Error("Failed to load Pyodide from CDN"));
    };
    document.head.appendChild(script);
  });

  return loadPromise;
}

/**
 * Extracts top-level imports, function definitions, and class definitions from Python source.
 * Skips top-level standalone calls (print, variable assignments, etc.).
 */
export function extractPythonFunctions(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let inBlock = false;

  for (const line of lines) {
    const isImport = /^(import |from )/.test(line);
    const isTopLevelDef = /^(def |class |@)/.test(line);
    const isEmpty = line.trim() === "";
    const isIndented = line.startsWith(" ") || line.startsWith("\t");

    if (isImport) {
      result.push(line);
      inBlock = false;
    } else if (isTopLevelDef) {
      inBlock = true;
      result.push(line);
    } else if (inBlock && (isEmpty || isIndented)) {
      result.push(line);
    } else {
      inBlock = false;
    }
  }

  // Trim trailing empty lines
  while (result.length > 0 && result[result.length - 1].trim() === "") {
    result.pop();
  }

  return result.join("\n");
}

export async function runPython(code: string): Promise<RunResult> {
  if (!pyodide) return { stdout: "", stderr: "", error: "Python runtime not initialized" };

  let stdout = "";
  let stderr = "";
  let error = "";

  try {
    pyodide.runPython(
      `import sys as _sys, io as _io\n_hyp_out = _io.StringIO()\n_hyp_err = _io.StringIO()\n_hyp_orig_out = _sys.stdout\n_hyp_orig_err = _sys.stderr\n_sys.stdout = _hyp_out\n_sys.stderr = _hyp_err`
    );
  } catch {
    return { stdout: "", stderr: "", error: "Failed to initialize output capture" };
  }

  try {
    pyodide.runPython(code);
  } catch (e: any) {
    error = (e.message || String(e)).replace(/^PythonError:\s*/, "").trim();
  }

  try {
    stdout = pyodide.runPython("_hyp_out.getvalue()");
    stderr = pyodide.runPython("_hyp_err.getvalue()");
  } catch {}

  try {
    pyodide.runPython(`_sys.stdout = _hyp_orig_out\n_sys.stderr = _hyp_orig_err`);
  } catch {}

  return { stdout, stderr, error };
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;

    if (test.code) {
      const funcs = extractPythonFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", funcs);
    }

    const result = await runPython(codeToRun);
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
