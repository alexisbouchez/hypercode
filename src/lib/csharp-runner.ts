import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let wasmSharpModule: any = null;
let csharpReady = false;
let initPromise: Promise<void> | null = null;

export function isCSharpReady(): boolean {
  return csharpReady;
}

export async function initCSharpRunner(): Promise<void> {
  if (csharpReady) return;
  if (initPromise) return initPromise;

  initPromise = (async () => {
    try {
      const { WasmSharpModule } = await import("@wasmsharp/core");
      wasmSharpModule = await WasmSharpModule.initializeAsync({
        disableWebWorker: true,
      });
      csharpReady = true;
    } catch (err) {
      initPromise = null;
      throw err;
    }
  })();

  return initPromise;
}

export async function runCSharp(code: string): Promise<RunResult> {
  if (!wasmSharpModule) {
    return { stdout: "", stderr: "", error: "C# runtime not initialized" };
  }

  try {
    const compilation = await wasmSharpModule.createCompilationAsync(code);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const result: any = await compilation.run();

    if (result.success) {
      return {
        stdout: result.stdOut ?? "",
        stderr: result.stdErr ?? "",
        error: "",
      };
    } else {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const errors = (result.diagnostics as any[])
        .filter((d) => d.severity === "Error")
        .map((d) => d.message)
        .join("\n");
      return { stdout: "", stderr: errors, error: errors };
    }
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts top-level method, class, interface, and struct declarations
 * from C# script code. Strips top-level executable statements.
 */
export function extractCSharpDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inDecl = false;
  let hasSeenBrace = false;

  for (const line of lines) {
    const trimmed = line.trim();

    if (!inDecl) {
      const startsDecl =
        /^(static|public|private|protected|internal|async|virtual|override|abstract|sealed|readonly)\b/.test(trimmed) ||
        /^(class|interface|struct|record|enum)\b/.test(trimmed);

      if (startsDecl) {
        inDecl = true;
        hasSeenBrace = false;
        result.push(line);
        const opens = (line.match(/\{/g) ?? []).length;
        const closes = (line.match(/\}/g) ?? []).length;
        depth += opens - closes;
        if (opens > 0) hasSeenBrace = true;
        if (hasSeenBrace && depth <= 0) {
          inDecl = false;
          depth = 0;
        }
      }
      // Skip top-level executable statements
    } else {
      result.push(line);
      const opens = (line.match(/\{/g) ?? []).length;
      const closes = (line.match(/\}/g) ?? []).length;
      depth += opens - closes;
      if (opens > 0) hasSeenBrace = true;
      if (hasSeenBrace && depth <= 0) {
        inDecl = false;
        depth = 0;
      }
    }
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
      const declarations = extractCSharpDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", declarations);
    }

    const result = await runCSharp(codeToRun);
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
