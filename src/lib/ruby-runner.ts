import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let rubyVm: any = null;
let rubyReady = false;
let initializing = false;

export function isRubyReady(): boolean {
  return rubyReady;
}

export async function initRubyRunner(): Promise<void> {
  if (rubyReady || initializing) return;
  initializing = true;

  try {
    const [{ WASI, OpenFile, File: WasiFile, ConsoleStdout }, { RubyVM }] = await Promise.all([
      import("@bjorn3/browser_wasi_shim"),
      import("@ruby/wasm-wasi"),
    ]);

    const response = await fetch("/ruby/ruby.wasm");
    if (!response.ok) throw new Error(`Failed to fetch ruby.wasm: ${response.status}`);
    const module = await WebAssembly.compileStreaming(response);

    // stdin unused; stdout/stderr go to browser console by default
    const stdin = new OpenFile(new WasiFile(new Uint8Array()));
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const stdout = ConsoleStdout.lineBuffered((msg: any) => console.log(msg));
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const stderr = ConsoleStdout.lineBuffered((msg: any) => console.error(msg));

    const wasi = new WASI([], [], [stdin, stdout, stderr]);
    const { vm } = await RubyVM.instantiateModule({ module, wasip1: wasi });

    // Set up a custom IO class for capturing output without requiring stdlib
    vm.eval(`
class __HypercodeIO
  attr_reader :__buffer
  def initialize
    @__buffer = ""
  end
  def reset
    @__buffer = ""
  end
  def write(str)
    @__buffer << str.to_s
    str.to_s.bytesize
  end
  def puts(*args)
    if args.empty?
      @__buffer << "\\n"
    else
      args.each do |a|
        if a.nil?
          @__buffer << "\\n"
        elsif a.is_a?(Array)
          a.flatten.each { |x| @__buffer << x.to_s << "\\n" }
        else
          s = a.to_s
          @__buffer << s
          @__buffer << "\\n" unless s.end_with?("\\n")
        end
      end
    end
    nil
  end
  def print(*args)
    args.each { |a| @__buffer << a.to_s }
    nil
  end
  def flush; self; end
  def sync; true; end
  def sync=(v); v; end
  def fileno; 1; end
  def isatty; false; end
  def tty?; false; end
end

$__hypercode_stdout = __HypercodeIO.new
$__hypercode_stderr = __HypercodeIO.new
`);

    rubyVm = vm;
    rubyReady = true;
  } finally {
    initializing = false;
  }
}

export async function runRuby(code: string): Promise<RunResult> {
  if (!rubyVm) return { stdout: "", stderr: "", error: "Ruby not initialized" };

  try {
    // Reset buffers and redirect output
    rubyVm.eval(`
$__hypercode_stdout.reset
$__hypercode_stderr.reset
$stdout = $__hypercode_stdout
$stderr = $__hypercode_stderr
`);

    let rubyError = "";
    try {
      rubyVm.eval(code);
    } catch (e) {
      rubyError = e instanceof Error ? e.message : String(e);
    }

    // Always restore global stdout/stderr
    rubyVm.eval(`
$stdout = STDOUT
$stderr = STDERR
`);

    const stdout = rubyVm.eval("$__hypercode_stdout.__buffer").toString();
    let stderr = rubyVm.eval("$__hypercode_stderr.__buffer").toString();

    if (rubyError && !stderr) {
      stderr = rubyError;
    }

    return { stdout, stderr, error: "" };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

/**
 * Extracts top-level def...end blocks from Ruby source.
 * Removes standalone top-level calls (puts, print, etc.).
 */
export function extractRubyMethods(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let collecting = false;

  for (const line of lines) {
    const trimmed = line.trim();

    if (!collecting) {
      if (/^def\s/.test(trimmed)) {
        collecting = true;
        depth = 1;
        result.push(line);
      }
      // Skip standalone top-level calls
    } else {
      result.push(line);

      // Count block-opening keywords at the start of a trimmed line
      if (/^(def|class|module|begin|case)\b/.test(trimmed)) {
        depth++;
      } else if (/^(if|unless|while|until|for)\b/.test(trimmed) && !trimmed.includes(";")) {
        depth++;
      } else if (/\bdo\s*(\|[^|]*\|)?\s*$/.test(trimmed)) {
        depth++;
      }

      if (/^end\b/.test(trimmed)) {
        depth--;
        if (depth <= 0) {
          collecting = false;
          depth = 0;
        }
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
      const methods = extractRubyMethods(code);
      codeToRun = test.code.replace("{{FUNC}}", methods);
    }

    const result = await runRuby(codeToRun);
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
