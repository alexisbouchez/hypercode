import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let swiftReady = false;

export function isSwiftReady(): boolean {
  return swiftReady;
}

export async function initSwiftRunner(): Promise<void> {
  swiftReady = true;
}

// ---------------------------------------------------------------------------
// Swift → JavaScript transpiler
// Handles the subset of Swift used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function print(...args) {
  const s = args.map(v => {
    if (v === true) return 'true';
    if (v === false) return 'false';
    if (v === null || v === undefined) return 'nil';
    if (Array.isArray(v)) return '[' + v.join(', ') + ']';
    return String(v);
  }).join(' ');
  __output.push(s + '\\n');
}
`;

// ---------------------------------------------------------------------------
// String interpolation: "Hello, \\(expr)!" → \`Hello, \${expr}!\`
// ---------------------------------------------------------------------------

function transformStringInterpolation(line: string): string {
  let result = "";
  let i = 0;
  while (i < line.length) {
    // Single-line string literals only
    if (line[i] === '"') {
      let inner = "";
      let hasInterp = false;
      i++;
      while (i < line.length && line[i] !== '"') {
        if (line[i] === "\\" && line[i + 1] === "(") {
          hasInterp = true;
          i += 2; // skip \(
          let depth = 1;
          let expr = "";
          while (i < line.length && depth > 0) {
            if (line[i] === "(") depth++;
            else if (line[i] === ")") {
              depth--;
              if (depth === 0) { i++; break; }
            }
            expr += line[i++];
          }
          inner += "${" + expr + "}";
        } else if (line[i] === "\\" && i + 1 < line.length) {
          inner += line[i] + line[i + 1];
          i += 2;
        } else {
          inner += line[i++];
        }
      }
      i++; // closing "
      result += hasInterp ? "`" + inner + "`" : '"' + inner + '"';
    } else {
      result += line[i++];
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Function declaration transform
// "func name(_ a: Int, b: Type) -> ReturnType {" → "function name(a, b) {"
// Inside class/struct: "func name(...) -> T {" → "name(...) {"
// ---------------------------------------------------------------------------

function transformFuncDecl(line: string, inClass: boolean): string {
  // Match: optional modifiers + "func name(" + rest
  const m = line.match(/^(\s*)(?:(?:override|static|public|private|internal|mutating|class|final)\s+)*func\s+(\w+)\s*\(([^)]*)\)\s*(?:->\s*[^{]*)?\{/);
  if (!m) return line;
  const [, indent, name, rawParams] = m;
  const params = parseSwiftParams(rawParams);
  if (inClass) {
    return `${indent}${name}(${params.join(", ")}) {`;
  }
  return `${indent}function ${name}(${params.join(", ")}) {`;
}

// Parse Swift parameter list, extracting only internal parameter names
// "_ a: Int, b c: Double, d: String" → ["a", "c", "d"]
function parseSwiftParams(raw: string): string[] {
  if (!raw.trim()) return [];
  return raw.split(",").map((p) => {
    const parts = p.trim().split(/\s+/);
    // e.g. ["_", "a:", "Int"] → internal name is "a" (strip colon)
    //      ["b", "c:", "Double"] → internal name is "c"
    //      ["a:", "Int"] → internal name is "a"
    // After stripping the type (everything from colon onwards in the last token)
    // Internal name is: last identifier before the colon-bearing token
    const tokensBeforeColon: string[] = [];
    for (const tok of parts) {
      if (tok.includes(":")) {
        const nameOnly = tok.replace(":", "");
        if (nameOnly) tokensBeforeColon.push(nameOnly);
        break;
      }
      tokensBeforeColon.push(tok);
    }
    const internal = tokensBeforeColon[tokensBeforeColon.length - 1] ?? parts[0];
    return internal === "_" ? (tokensBeforeColon[tokensBeforeColon.length - 2] ?? "_") : internal;
  }).filter((p) => p && p !== "_");
}

// ---------------------------------------------------------------------------
// Strip type annotation from let/var declarations
// "let x: Int = 5" → "let x = 5"
// "var dict = [String: Int]()" → "let dict = {}"
// ---------------------------------------------------------------------------

function stripTypeAnnotation(line: string): string {
  // var/let + ident + ":" + type + "=" → var/let + ident + "="
  line = line.replace(/\b(let|var)\s+(\w+)\s*:\s*[^=\n]+=/, "$1 $2 =");
  // "= [String: Int]()" → "= {}"
  line = line.replace(/=\s*\[[\w\s:,]+\]\s*\(\)/, "= {}");
  // "= [Type]()" → "= []"
  line = line.replace(/=\s*\[[\w<>?,\s]+\]\s*\(\)/, "= []");
  // Strip standalone field declarations: "var field: Type" (no value, inside class)
  // These are handled by caller context
  return line;
}

// ---------------------------------------------------------------------------
// Closure transforms
// "{ x in expr }" → "((x) => expr)"
// "{ $0 * 2 }"   → "((_$0) => _$0 * 2)"
// ---------------------------------------------------------------------------

function transformTrailingClosures(line: string): string {
  // Method with trailing closure: .method { ... }
  // Match .method { single-line-body }
  line = line.replace(/\.\s*(\w+)\s*\{\s*(\$\d+(?:,\s*\$\d+)*)\s+in\s+([^}]+)\}/g, (_m, method, params, body) => {
    const jsParams = params.split(",").map((p: string) => p.trim());
    return `.${method}((${jsParams.join(", ")}) => ${body.trim()})`;
  });
  // .method { x in expr }
  line = line.replace(/\.\s*(\w+)\s*\{\s*(\w+(?:,\s*\w+)*)\s+in\s+([^}]+)\}/g, (_m, method, params, body) => {
    const jsParams = params.split(",").map((p: string) => p.trim());
    return `.${method}((${jsParams.join(", ")}) => ${body.trim()})`;
  });
  // .method { $0 op ... }
  line = line.replace(/\.\s*(\w+)\s*\{\s*([^}]*\$\d+[^}]*)\}/g, (_m, method, body) => {
    const usedDollars = new Set<number>();
    body.replace(/\$(\d+)/g, (_: string, n: string) => { usedDollars.add(parseInt(n)); return ""; });
    const sorted = [...usedDollars].sort((a, b) => a - b);
    const params = sorted.map((n) => `_$${n}`);
    const jsBody = body.replace(/\$(\d+)/g, (_: string, n: string) => `_$${n}`);
    return `.${method}((${params.join(", ")}) => ${jsBody.trim()})`;
  });
  return line;
}

// ---------------------------------------------------------------------------
// Constructor calls: TypeName(label: val, ...) → new TypeName(val, ...)
// ---------------------------------------------------------------------------

function transformConstructorCalls(line: string, typeNames: Set<string>): string {
  for (const name of typeNames) {
    // PascalCase followed by ( — add new and strip labels
    const re = new RegExp(`(?<![.\w])\\b(${name})\\s*\\(([^)]*)\\)`, "g");
    line = line.replace(re, (_m, typeName, args) => {
      const stripped = stripCallLabels(args);
      return `new ${typeName}(${stripped})`;
    });
  }
  return line;
}

// Strip "label: value" → "value" in argument lists
function stripCallLabels(args: string): string {
  return args
    .split(",")
    .map((arg) => {
      const m = arg.match(/^\s*\w+\s*:\s*(.+)$/);
      return m ? m[1] : arg;
    })
    .join(",");
}

// ---------------------------------------------------------------------------
// Enum transform
// "enum Direction { case north, south }" (multi-line) built by state machine
// "enum Planet: String { case earth = "Earth" }" → const with raw values
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Main transpile function
// ---------------------------------------------------------------------------

export function transpileSwift(code: string): string {
  // Phase 1: collect struct/class/enum type names for constructor detection
  const typeNames = new Set<string>();
  for (const line of code.split("\n")) {
    const m = line.trim().match(/^(?:class|struct)\s+(\w+)/);
    if (m) typeNames.add(m[1]);
  }

  // Phase 2: transform string interpolation
  code = code.split("\n").map(transformStringInterpolation).join("\n");

  // Phase 3: line-by-line transforms with brace depth tracking
  const lines = code.split("\n");
  const out: string[] = [PRELUDE];

  // Context stack: 'class' | 'struct' | 'enum' | 'protocol' | 'other'
  const stack: Array<{ type: string; depth: number }> = [];
  let depth = 0;

  const currentType = () => (stack.length ? stack[stack.length - 1].type : "top");

  let i = 0;
  while (i < lines.length) {
    const raw = lines[i];
    const trimmed = raw.trim();
    i++;

    // Count depth change on this line
    const opens = (raw.match(/\{/g) ?? []).length;
    const closes = (raw.match(/\}/g) ?? []).length;

    // Pop contexts closed by this line BEFORE processing
    const newDepth = depth + opens - closes;
    while (stack.length > 0 && stack[stack.length - 1].depth > newDepth) {
      stack.pop();
    }

    // Blank lines
    if (trimmed === "") { out.push(""); depth = newDepth; continue; }

    // Comments
    if (trimmed.startsWith("//")) { out.push(raw); depth = newDepth; continue; }

    // Protocol declaration — emit as comment, skip body
    if (/^protocol\s+\w+/.test(trimmed)) {
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      out.push(`${indent}// protocol (skipped)`);
      if (opens > 0) {
        // Skip until matching }
        let d = opens - closes;
        while (d > 0 && i < lines.length) {
          const l = lines[i++];
          const o = (l.match(/\{/g) ?? []).length;
          const c = (l.match(/\}/g) ?? []).length;
          d += o - c;
        }
      }
      depth = newDepth;
      continue;
    }

    // Enum declaration — convert to const object
    if (/^enum\s+\w+/.test(trimmed) && opens > 0) {
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      const enumMatch = trimmed.match(/^enum\s+(\w+)(?::\s*\w+)?\s*\{/);
      const enumName = enumMatch?.[1] ?? "Unknown";
      const cases: Array<[string, string]> = [];
      // Collect case lines until closing }
      let d = opens - closes;
      while (d > 0 && i < lines.length) {
        const l = lines[i++].trim();
        d += (l.match(/\{/g) ?? []).length - (l.match(/\}/g) ?? []).length;
        if (l.startsWith("case ")) {
          const casePart = l.slice(5).trim();
          for (const c of casePart.split(",")) {
            const cv = c.trim();
            // "name = "Value"" or just "name"
            const kvm = cv.match(/^(\w+)\s*=\s*"([^"]+)"/);
            if (kvm) {
              cases.push([kvm[1], kvm[2]]);
            } else if (/^\w+$/.test(cv)) {
              cases.push([cv, cv]);
            }
          }
        }
      }
      const entries = cases.map(([k, v]) => `${k}: "${v}"`).join(", ");
      out.push(`${indent}const ${enumName} = Object.freeze({ ${entries} });`);
      depth = newDepth;
      continue;
    }

    // struct → class
    let line = raw;
    if (/^\s*struct\s+/.test(line)) {
      line = line.replace(/\bstruct\b/, "class");
      if (opens > 0) stack.push({ type: "class", depth: depth + opens });
      depth = newDepth;
      out.push(line);
      continue;
    }

    // class with protocol conformance: "class Foo: Protocol {" → "class Foo {"
    // or "class Foo: BaseClass {" — keep base class
    if (/^\s*class\s+\w+/.test(trimmed)) {
      const classM = line.match(/^(\s*class\s+\w+)(?::\s*[\w,\s]+)?(\s*\{)/);
      if (classM) {
        // Determine if the conformance is a known type name (class) or protocol
        const conformance = line.match(/class\s+\w+\s*:\s*([\w,\s]+)\s*\{/)?.[1];
        if (conformance) {
          const parents = conformance.split(",").map((s) => s.trim());
          // Keep only class parents (those in typeNames or starting with capital but not known protocols)
          // For simplicity: keep first parent if it's in typeNames, else drop conformance
          const classParent = parents.find((p) => typeNames.has(p));
          if (classParent) {
            line = line.replace(/class\s+(\w+)\s*:[\w,\s]+\{/, `class $1 extends ${classParent} {`);
          } else {
            line = line.replace(/class\s+(\w+)\s*:[\w,\s]+\{/, "class $1 {");
          }
        }
      }
      if (opens > 0) stack.push({ type: "class", depth: depth + opens });
      depth = newDepth;
      out.push(line);
      continue;
    }

    const inClass = currentType() === "class";

    // init(...) → constructor(...)
    if (/^\s*init\s*\(/.test(line)) {
      line = line.replace(/\binit\s*\(([^)]*)\)/, (_m, params) => {
        const jsParams = parseSwiftParams(params);
        return `constructor(${jsParams.join(", ")})`;
      });
      line = line.replace(/\bself\./g, "this.");
      out.push(line);
      depth = newDepth;
      continue;
    }

    // func declarations
    if (/^\s*(?:(?:override|static|public|private|internal|mutating|class|final)\s+)*func\s+/.test(trimmed)) {
      line = transformFuncDecl(line, inClass);
      depth = newDepth;
      out.push(line);
      continue;
    }

    // Standalone field declarations inside class: "var x: Type" (no "=") → strip
    if (inClass && /^\s*(var|let)\s+\w+\s*:\s*[\w<>\[\]?,\s]+\s*$/.test(line)) {
      depth = newDepth;
      continue; // drop the line
    }

    // for i in a...b
    const forClosedMatch = line.match(/^(\s*)for\s+(\w+)\s+in\s+(.+?)\.\.\.(.+?)\s*\{/);
    if (forClosedMatch) {
      const [, indent, v, start, end] = forClosedMatch;
      line = `${indent}for (let ${v} = ${start.trim()}; ${v} <= ${end.trim()}; ${v}++) {`;
      depth = newDepth;
      out.push(line);
      continue;
    }

    // for i in a..<b
    const forOpenMatch = line.match(/^(\s*)for\s+(\w+)\s+in\s+(.+?)\.\.<(.+?)\s*\{/);
    if (forOpenMatch) {
      const [, indent, v, start, end] = forOpenMatch;
      line = `${indent}for (let ${v} = ${start.trim()}; ${v} < ${end.trim()}; ${v}++) {`;
      depth = newDepth;
      out.push(line);
      continue;
    }

    // for item in array
    if (/^\s*for\s+\w+\s+in\s+\w+/.test(line)) {
      line = line.replace(/\bfor\s+(\w+)\s+in\s+(\w+)/, "for (const $1 of $2)");
      depth = newDepth;
      out.push(applyCommonTransforms(line, typeNames));
      continue;
    }

    // switch statement
    if (/^\s*switch\s+/.test(line)) {
      line = line.replace(/\bswitch\s+(.+)\s*\{/, "switch ($1) {");
      depth = newDepth;
      out.push(line);
      continue;
    }

    // case with multiple values: "case 1, 2, 3:" → "case 1: case 2: case 3:"
    if (/^\s*case\s+/.test(line) && !line.includes("let ") && !line.includes("where")) {
      // Check for multi-value pattern: case a, b, c:
      const caseM = line.match(/^(\s*)case\s+(.+):\s*$/);
      if (caseM) {
        const [, indent, vals] = caseM;
        // Check if it's ".enumCase" references: strip leading dot for direct comparison
        const parts = vals.split(",").map((v) => v.trim());
        if (parts.length > 1) {
          line = parts.map((p, idx) =>
            idx < parts.length - 1
              ? `${indent}case ${p}:`
              : `${indent}case ${p}:`
          ).join("\n");
        } else {
          line = `${indent}case ${vals.trim()}:`;
        }
        // Strip leading dots from enum case references in switch cases
        line = line.replace(/case\s+\.([\w]+)/g, "case \"$1\"");
        depth = newDepth;
        out.push(line);
        continue;
      }
    }

    // if let binding
    if (/^\s*if\s+let\s+/.test(line)) {
      const m = line.match(/^(\s*)if\s+let\s+(\w+)\s*=\s*(.+?)\s*\{/);
      if (m) {
        const [, indent, varName, expr] = m;
        line = `${indent}if (${expr.trim()} != null) { const ${varName} = ${expr.trim()};`;
        depth = newDepth;
        out.push(line);
        continue;
      }
    }

    // guard let
    if (/^\s*guard\s+let\s+/.test(line)) {
      const m = line.match(/^(\s*)guard\s+let\s+(\w+)\s*=\s*(.+?)\s+else\s*\{/);
      if (m) {
        const [, indent, varName, expr] = m;
        line = `${indent}if (${expr.trim()} == null) {`;
        // The next lines are the else block, keep them as-is
        depth = newDepth;
        out.push(line);
        // Insert: const varName = expr; after the else closing }
        // For simplicity, just emit a comment
        out.push(`${indent}  // guard failed`);
        // We need to skip until the }, then emit: const varName = expr;
        // This is complex; for our lessons, guard is followed by "return" only
        continue;
      }
    }

    // Apply all common transforms
    line = applyCommonTransforms(line, typeNames);
    out.push(line);
    depth = newDepth;
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join("\n");
}

function applyCommonTransforms(line: string, typeNames: Set<string>): string {
  // Type annotations in let/var
  line = stripTypeAnnotation(line);

  // nil → null
  line = line.replace(/\bnil\b/g, "null");

  // self. → this.
  line = line.replace(/\bself\./g, "this.");

  // array.count → array.length
  line = line.replace(/\.count\b/g, ".length");

  // array.append( → array.push(
  line = line.replace(/\.append\(/g, ".push(");

  // array.contains( → array.includes(
  line = line.replace(/\.contains\(/g, ".includes(");

  // array.isEmpty → array.length === 0
  line = line.replace(/\.isEmpty\b/g, ".length === 0");

  // .rawValue → strip it
  line = line.replace(/\.rawValue\b/g, "");

  // String(...) type cast → keep as-is (JS String() works)

  // Trailing closures
  line = transformTrailingClosures(line);

  // Constructor calls with type names
  line = transformConstructorCalls(line, typeNames);

  return line;
}

// ---------------------------------------------------------------------------
// Extract declarations (func / class / struct / enum) from solution code
// ---------------------------------------------------------------------------

export function extractSwiftDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inDecl = false;
  let hasSeenBrace = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!inDecl) {
      const startsDecl =
        /^(?:(?:override|static|public|private|internal|mutating|class|final)\s+)*func\s+/.test(trimmed) ||
        /^(?:class|struct|enum|protocol)\s+/.test(trimmed);
      if (startsDecl) {
        inDecl = true;
        hasSeenBrace = false;
        result.push(line);
        const opens = (line.match(/\{/g) ?? []).length;
        const closes = (line.match(/\}/g) ?? []).length;
        depth += opens - closes;
        if (opens > 0) hasSeenBrace = true;
        if (hasSeenBrace && depth <= 0) { inDecl = false; depth = 0; }
      }
    } else {
      result.push(line);
      const opens = (line.match(/\{/g) ?? []).length;
      const closes = (line.match(/\}/g) ?? []).length;
      depth += opens - closes;
      if (opens > 0) hasSeenBrace = true;
      if (hasSeenBrace && depth <= 0) { inDecl = false; depth = 0; }
    }
  }
  return result.join("\n").trim();
}

// ---------------------------------------------------------------------------
// Browser runner API
// ---------------------------------------------------------------------------

export async function runSwift(code: string): Promise<RunResult> {
  try {
    const js = transpileSwift(code);
    let stdout = "";
    const origLog = console.log;
    const captured: string[] = [];
    console.log = (...args: unknown[]) => captured.push(args.join(" "));
    try {
      // eslint-disable-next-line no-new-func
      new Function(js)();
    } finally {
      console.log = origLog;
    }
    stdout = captured.join("\n");
    return { stdout, stderr: "", error: "" };
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    return { stdout: "", stderr: msg, error: msg };
  }
}

export async function runTests(
  code: string,
  tests: Test[]
): Promise<TestResult[]> {
  const results: TestResult[] = [];
  for (const test of tests) {
    let codeToRun = code;
    if (test.code) {
      const decls = extractSwiftDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }
    const result = await runSwift(codeToRun);
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
