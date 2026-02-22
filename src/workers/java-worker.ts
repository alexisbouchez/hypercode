/* eslint-disable @typescript-eslint/no-explicit-any */

type LoadFn = (
  src: string | ArrayBuffer | ArrayBufferView,
  opts?: {
    stackDeobfuscator?: { enabled: boolean };
    installImports?: (importObject: any) => void;
    noAutoImports?: boolean;
  }
) => Promise<{ exports: any }>;

let teavmLoad: LoadFn | null = null;
let createCompilerFn: (() => any) | null = null;
let sdkBytes: Int8Array | null = null;
let rtBytes: Int8Array | null = null;
let isReady = false;

async function init() {
  // Load TeaVM runtime — ES module with named export 'load'
  // Served from /public/java/ at runtime; not in node_modules so we bypass TS
  const runtimeUrl = "/java/compiler.wasm-runtime.js";
  const runtime = (await import(/* webpackIgnore: true */ runtimeUrl)) as {
    load: LoadFn;
  };
  teavmLoad = runtime.load;

  // Load and instantiate compiler.wasm
  const compilerModule = await teavmLoad(
    `${location.origin}/java/compiler.wasm`,
    { stackDeobfuscator: { enabled: false } }
  );
  createCompilerFn = compilerModule.exports.createCompiler as () => any;

  // Fetch and cache classlibs (loaded once, reused per run)
  const [sdkBuf, rtBuf] = await Promise.all([
    fetch("/java/compile-classlib-teavm.bin").then((r) => r.arrayBuffer()),
    fetch("/java/runtime-classlib-teavm.bin").then((r) => r.arrayBuffer()),
  ]);
  sdkBytes = new Int8Array(sdkBuf);
  rtBytes = new Int8Array(rtBuf);

  isReady = true;
  postMessage({ loaded: true });
}

async function runJava(
  code: string
): Promise<{ stdout: string; stderr: string; error: string }> {
  if (!createCompilerFn || !teavmLoad || !sdkBytes || !rtBytes) {
    return { stdout: "", stderr: "", error: "Java runtime not initialized" };
  }

  // Create a fresh compiler instance for each run
  const compiler = createCompilerFn();
  compiler.setSdk(sdkBytes);
  compiler.setTeaVMClasslib(rtBytes);
  compiler.addSourceFile("Main.java", code);

  const diagnostics: any[] = [];
  const reg = compiler.onDiagnostic((d: any) => diagnostics.push(d));
  const compileOk: boolean = compiler.compile();
  reg.destroy();

  if (!compileOk) {
    const errors = diagnostics
      .filter((d) => d.severity === "error")
      .map((d) => `${d.fileName || "Main.java"}:${d.lineNumber}: ${d.message}`)
      .join("\n");
    return { stdout: "", stderr: "", error: errors || "Compilation failed" };
  }

  const mainClasses: string[] = compiler.detectMainClasses();
  if (!mainClasses || mainClasses.length === 0) {
    return {
      stdout: "",
      stderr: "",
      error:
        "No main class found. Ensure your class has:\n  public static void main(String[] args)",
    };
  }
  const mainClass = mainClasses[0].replace(/\//g, ".");

  const wasmDiags: any[] = [];
  const wasmReg = compiler.onDiagnostic((d: any) => wasmDiags.push(d));
  const wasmOk: boolean = compiler.generateWebAssembly({
    outputName: "app",
    mainClass,
  });
  wasmReg.destroy();

  if (!wasmOk) {
    const errors = wasmDiags
      .filter((d) => d.severity === "error" || d.severity === "ERROR")
      .map((d) => d.message)
      .join("\n");
    return {
      stdout: "",
      stderr: "",
      error: errors || "WebAssembly generation failed",
    };
  }

  const wasmBytes: Int8Array = compiler.getWebAssemblyOutputFile("app.wasm");

  let stdout = "";
  let stderr = "";
  let stdoutBuf = "";
  let stderrBuf = "";

  try {
    const programModule = await teavmLoad(wasmBytes, {
      stackDeobfuscator: { enabled: false },
      installImports(o: any) {
        o.teavmConsole.putcharStdout = (ch: number) => {
          if (ch === 0x0a) {
            stdout += stdoutBuf + "\n";
            stdoutBuf = "";
          } else {
            stdoutBuf += String.fromCharCode(ch);
          }
        };
        o.teavmConsole.putcharStderr = (ch: number) => {
          if (ch === 0x0a) {
            stderr += stderrBuf + "\n";
            stderrBuf = "";
          } else {
            stderrBuf += String.fromCharCode(ch);
          }
        };
      },
    });
    programModule.exports.main([]);
    if (stdoutBuf) stdout += stdoutBuf;
    if (stderrBuf) stderr += stderrBuf;
  } catch (e) {
    const errMsg = e instanceof Error ? e.message : String(e);
    return { stdout, stderr, error: errMsg };
  }

  return { stdout, stderr, error: "" };
}

(async () => {
  try {
    await init();
  } catch (e) {
    postMessage({
      error: `Init failed: ${e instanceof Error ? e.message : String(e)}`,
    });
    return;
  }

  addEventListener("message", async (e: MessageEvent) => {
    if (!isReady) {
      postMessage({ stdout: "", stderr: "", error: "Java runtime not ready" });
      return;
    }
    const result = await runJava(e.data.code as string);
    postMessage(result);
  });
})();
