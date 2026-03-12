import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let phpReady = false;

export function isPhpReady(): boolean {
  return phpReady;
}

export async function initPhpRunner(): Promise<void> {
  phpReady = true;
}

// ---------------------------------------------------------------------------
// PHP -> JavaScript transpiler
// Handles the subset of PHP used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function echo_(...args) {
  __output.push(args.map(String).join(''));
}
function strlen(s) { return s.length; }
function strtoupper(s) { return s.toUpperCase(); }
function strtolower(s) { return s.toLowerCase(); }
function substr(s, start, len) {
  if (len === undefined) return s.substring(start);
  return s.substring(start, start + len);
}
function str_replace(search, replace, subject) {
  return subject.split(search).join(replace);
}
function str_repeat(s, n) { return s.repeat(n); }
function strrev(s) { return s.split('').reverse().join(''); }
function strpos(haystack, needle) {
  const i = haystack.indexOf(needle);
  return i === -1 ? false : i;
}
function str_contains(haystack, needle) { return haystack.includes(needle); }
function trim(s) { return s.trim(); }
function ltrim(s) { return s.trimStart(); }
function rtrim(s) { return s.trimEnd(); }
function str_pad(input, length, pad, type) {
  input = String(input);
  pad = pad || ' ';
  if (type === 1) return input.padStart(length, pad);
  return input.padEnd(length, pad);
}
const STR_PAD_LEFT = 1;
const STR_PAD_RIGHT = 0;
function implode(glue, arr) { return arr.join(glue); }
function explode(delimiter, str) { return str.split(delimiter); }
function count(arr) { return Array.isArray(arr) ? arr.length : Object.keys(arr).length; }
function array_push(arr, ...vals) { arr.push(...vals); return arr.length; }
function array_pop(arr) { return arr.pop(); }
function array_shift(arr) { return arr.shift(); }
function array_unshift(arr, ...vals) { arr.unshift(...vals); return arr.length; }
function in_array(needle, haystack) { return haystack.includes(needle); }
function array_key_exists(key, obj) { return key in obj; }
function array_keys(obj) { return Object.keys(obj); }
function array_values(obj) { return Array.isArray(obj) ? [...obj] : Object.values(obj); }
function array_reverse(arr) { return [...arr].reverse(); }
function array_slice(arr, start, len) {
  if (len === undefined) return arr.slice(start);
  return arr.slice(start, start + len);
}
function array_map(fn, arr) { return arr.map(fn); }
function array_filter(arr, fn) { return fn ? arr.filter(fn) : arr.filter(Boolean); }
function array_reduce(arr, fn, init) { return arr.reduce(fn, init); }
function array_sum(arr) { return arr.reduce((a, b) => a + b, 0); }
function sort(arr) { arr.sort((a, b) => a < b ? -1 : a > b ? 1 : 0); return true; }
function rsort(arr) { arr.sort((a, b) => a > b ? -1 : a < b ? 1 : 0); return true; }
function intdiv(a, b) { return Math.trunc(a / b); }
function abs(n) { return Math.abs(n); }
function ceil(n) { return Math.ceil(n); }
function floor(n) { return Math.floor(n); }
function round(n, p) { if (p === undefined) return Math.round(n); const f = 10 ** p; return Math.round(n * f) / f; }
function max(...args) { if (args.length === 1 && Array.isArray(args[0])) return Math.max(...args[0]); return Math.max(...args); }
function min(...args) { if (args.length === 1 && Array.isArray(args[0])) return Math.min(...args[0]); return Math.min(...args); }
function range(start, end, step) {
  step = step || 1;
  const arr = [];
  if (start <= end) { for (let i = start; i <= end; i += step) arr.push(i); }
  else { for (let i = start; i >= end; i -= step) arr.push(i); }
  return arr;
}
function unset(obj, key) { delete obj[key]; }
`;

// ---------------------------------------------------------------------------
// String interpolation: "Hello $name" -> template literal
// Handles $var, $this->prop, $arr[idx], and $arr["key"] inside double-quoted strings.
// ---------------------------------------------------------------------------

function transformStringInterpolation(line: string): string {
  let result = "";
  let i = 0;
  while (i < line.length) {
    if (line[i] === '"') {
      i++;
      let inner = "";
      let hasInterp = false;
      while (i < line.length && line[i] !== '"') {
        if (line[i] === "\\" && i + 1 < line.length) {
          const next = line[i + 1];
          if (next === "n") { inner += "\\n"; i += 2; }
          else if (next === "t") { inner += "\\t"; i += 2; }
          else if (next === '"') { inner += '"'; i += 2; }
          else if (next === "\\") { inner += "\\\\"; i += 2; }
          else if (next === "$") { inner += "\\$"; i += 2; }
          else { inner += line[i] + line[i + 1]; i += 2; }
        } else if (line[i] === "$") {
          hasInterp = true;
          i++;
          if (i < line.length && line[i] === "{") {
            i++;
            let depth = 1;
            let expr = "";
            while (i < line.length && depth > 0) {
              if (line[i] === "{") depth++;
              else if (line[i] === "}") { depth--; if (depth === 0) { i++; break; } }
              expr += line[i++];
            }
            inner += "${" + expr + "}";
          } else {
            // $this->prop
            let name = "";
            while (i < line.length && /\w/.test(line[i])) name += line[i++];
            if (i + 1 < line.length && line[i] === "-" && line[i + 1] === ">") {
              i += 2;
              let prop = "";
              while (i < line.length && /\w/.test(line[i])) prop += line[i++];
              inner += "${" + name + "." + prop + "}";
            } else if (i < line.length && line[i] === "[") {
              i++;
              let idx = "";
              while (i < line.length && line[i] !== "]") idx += line[i++];
              if (i < line.length) i++;
              inner += "${" + name + "[" + idx + "]}";
            } else {
              inner += "${" + name + "}";
            }
          }
        } else if (line[i] === "`") {
          inner += "\\`";
          i++;
        } else {
          inner += line[i++];
        }
      }
      if (i < line.length) i++;
      if (hasInterp) {
        result += "`" + inner + "`";
      } else {
        result += '"' + inner + '"';
      }
    } else if (line[i] === "'") {
      result += line[i++];
      while (i < line.length && line[i] !== "'") {
        if (line[i] === "\\" && i + 1 < line.length && line[i + 1] === "'") {
          result += "\\'";
          i += 2;
        } else {
          result += line[i++];
        }
      }
      if (i < line.length) result += line[i++];
    } else if (line[i] === "/" && i + 1 < line.length && line[i + 1] === "/") {
      result += line.slice(i);
      break;
    } else if (line[i] === "#") {
      result += "//" + line.slice(i + 1);
      break;
    } else {
      result += line[i++];
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Main transpile function
// ---------------------------------------------------------------------------

export function transpilePhp(code: string): string {
  const rawLines = code.split("\n");
  const filteredLines: string[] = [];
  for (const l of rawLines) {
    const t = l.trim();
    if (t === "<?php" || t === "?>") continue;
    filteredLines.push(l);
  }

  // Join multi-line expressions (unbalanced [ or ( only, not {)
  const joinedLines: string[] = [];
  for (let j = 0; j < filteredLines.length; j++) {
    let current = filteredLines[j];
    while (j + 1 < filteredLines.length) {
      const openSquare = (current.match(/\[/g) ?? []).length - (current.match(/\]/g) ?? []).length;
      const openParen = (current.match(/\(/g) ?? []).length - (current.match(/\)/g) ?? []).length;
      if (openSquare > 0 || openParen > 0) {
        j++;
        current = current.trimEnd() + " " + filteredLines[j].trim();
      } else {
        break;
      }
    }
    joinedLines.push(current);
  }

  const interpLines = joinedLines.map(transformStringInterpolation);
  const out: string[] = [PRELUDE];

  // Track classes for proper transpilation
  let inClass = false;
  let classDepth = 0;
  let className = "";

  for (let i = 0; i < interpLines.length; i++) {
    const line = interpLines[i];
    const trimmed = line.trim();
    const indent = line.match(/^(\s*)/)?.[1] ?? "";

    if (trimmed === "") { out.push(""); continue; }
    if (trimmed.startsWith("//")) { out.push(line); continue; }
    if (trimmed === ";") continue;

    const stmt = trimmed.replace(/;\s*$/, "");

    // --- class declaration ---
    const classMatch = stmt.match(/^class\s+(\w+)\s*\{$/);
    if (classMatch) {
      className = classMatch[1];
      inClass = true;
      classDepth = 1;
      out.push(`${indent}class ${className} {`);
      continue;
    }

    if (inClass) {
      const opens = (trimmed.match(/\{/g) || []).length;
      const closes = (trimmed.match(/\}/g) || []).length;

      // --- public/private/protected property ---
      const propMatch = stmt.match(/^(?:public|private|protected)\s+\$(\w+)(?:\s*=\s*(.+))?$/);
      if (propMatch && classDepth === 1) {
        // Skip property declarations - handled in constructor
        classDepth += opens - closes;
        continue;
      }

      // --- method declaration ---
      const methodMatch = stmt.match(/^(?:public|private|protected)?\s*function\s+(\w+)\s*\(([^)]*)\)\s*\{$/);
      if (methodMatch) {
        const [, methodName, params] = methodMatch;
        const jsParams = params.split(",").map(p => {
          p = p.trim();
          if (!p) return "";
          // Handle default values: $x = 5
          const defMatch = p.match(/^\$(\w+)\s*=\s*(.+)$/);
          if (defMatch) return `${defMatch[1]} = ${transformExpr(defMatch[2])}`;
          return p.replace(/^\$/, "");
        }).filter(Boolean).join(", ");

        if (methodName === "__construct") {
          out.push(`${indent}constructor(${jsParams}) {`);
        } else {
          out.push(`${indent}${methodName}(${jsParams}) {`);
        }
        classDepth += opens - closes;
        continue;
      }

      // Transform $this->prop
      let transformed = stmt;
      transformed = transformed.replace(/\$this->(\w+)\s*\(/g, "this.$1(");
      transformed = transformed.replace(/\$this->(\w+)/g, "this.$1");

      // Handle closing brace for class
      if (trimmed === "}") {
        classDepth--;
        if (classDepth === 0) {
          inClass = false;
          className = "";
          out.push(`${indent}}`);
          continue;
        }
      } else {
        classDepth += opens - closes;
      }

      // Transform the rest of the line
      out.push(`${indent}${transformStmt(transformed, indent)}`);
      continue;
    }

    out.push(`${indent}${transformStmt(stmt, indent)}`);
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join("\n");
}

function transformStmt(stmt: string, indent: string): string {
  // --- echo ---
  const echoMatch = stmt.match(/^echo\s+(.+)$/);
  if (echoMatch) {
    const arg = transformExpr(echoMatch[1].trim());
    return `echo_(${arg});`;
  }

  // --- function declaration ---
  const funcMatch = stmt.match(/^function\s+(\w+)\s*\(([^)]*)\)\s*\{$/);
  if (funcMatch) {
    const [, name, params] = funcMatch;
    const jsParams = params.split(",").map(p => {
      p = p.trim();
      if (!p) return "";
      const defMatch = p.match(/^\$(\w+)\s*=\s*(.+)$/);
      if (defMatch) return `${defMatch[1]} = ${transformExpr(defMatch[2])}`;
      return p.replace(/^\$/, "");
    }).filter(Boolean).join(", ");
    return `function ${name}(${jsParams}) {`;
  }

  // --- $var = function(...) use (&$x) { ---
  const closureMatch = stmt.match(/^\$(\w+)\s*=\s*function\s*\(([^)]*)\)\s*(?:use\s*\(([^)]*)\)\s*)?\{$/);
  if (closureMatch) {
    const [, varName, params, _useVars] = closureMatch;
    const jsParams = params.split(",").map(p => p.trim().replace(/^\$/, "")).filter(Boolean).join(", ");
    return `let ${varName} = function(${jsParams}) {`;
  }

  // --- return function(...) use (&$x) { ---
  const returnClosureMatch = stmt.match(/^return\s+function\s*\(([^)]*)\)\s*(?:use\s*\(([^)]*)\)\s*)?\{$/);
  if (returnClosureMatch) {
    const [, params, _useVars] = returnClosureMatch;
    const jsParams = params.split(",").map(p => p.trim().replace(/^\&?\$/, "")).filter(Boolean).join(", ");
    return `return function(${jsParams}) {`;
  }

  // --- if ---
  const ifMatch = stmt.match(/^if\s*\((.+)\)\s*\{$/);
  if (ifMatch) {
    return `if (${transformExpr(ifMatch[1])}) {`;
  }

  // --- elseif ---
  const elseifMatch = stmt.match(/^}\s*elseif\s*\((.+)\)\s*\{$/);
  if (elseifMatch) {
    return `} else if (${transformExpr(elseifMatch[1])}) {`;
  }

  // --- else ---
  const elseMatch = stmt.match(/^}\s*else\s*\{$/);
  if (elseMatch) {
    return `} else {`;
  }

  // --- while ---
  const whileMatch = stmt.match(/^while\s*\((.+)\)\s*\{$/);
  if (whileMatch) {
    return `while (${transformExpr(whileMatch[1])}) {`;
  }

  // --- do { ---
  if (stmt === "do {") {
    return `do {`;
  }

  // --- } while (...); ---
  const doWhileMatch = stmt.match(/^}\s*while\s*\((.+)\)$/);
  if (doWhileMatch) {
    return `} while (${transformExpr(doWhileMatch[1])});`;
  }

  // --- for ---
  const forMatch = stmt.match(/^for\s*\((.+)\)\s*\{$/);
  if (forMatch) {
    const parts = forMatch[1].split(";").map(p => transformExpr(p.trim()));
    // Convert $i = 0 to let i = 0 in init
    parts[0] = parts[0].replace(/^(\w+)\s*=/, "let $1 =");
    return `for (${parts.join("; ")}) {`;
  }

  // --- foreach ($arr as $key => $val) { ---
  const foreachKvMatch = stmt.match(/^foreach\s*\(\$(\w+)\s+as\s+\$(\w+)\s*=>\s*\$(\w+)\)\s*\{$/);
  if (foreachKvMatch) {
    const [, arr, key, val] = foreachKvMatch;
    return `for (const [${key}, ${val}] of Object.entries(${arr})) {`;
  }

  // --- foreach ($arr as $item) { ---
  const foreachMatch = stmt.match(/^foreach\s*\(\$(\w+)\s+as\s+\$(\w+)\)\s*\{$/);
  if (foreachMatch) {
    const [, arr, item] = foreachMatch;
    return `for (const ${item} of ${arr}) {`;
  }

  // --- $var = expr ---
  const assignMatch = stmt.match(/^\$(\w+)\s*=\s*(.+)$/);
  if (assignMatch) {
    const [, name, rhs] = assignMatch;
    return `let ${name} = ${transformExpr(rhs.trim())};`;
  }

  // --- $var++ / $var-- ---
  const incrMatch = stmt.match(/^\$(\w+)(\+\+|--)$/);
  if (incrMatch) {
    return `${incrMatch[1]}${incrMatch[2]};`;
  }

  // --- $var += expr etc ---
  const compoundMatch = stmt.match(/^\$(\w+)\s*(\+=|-=|\*=|\/=|%=|\.=)\s*(.+)$/);
  if (compoundMatch) {
    const [, name, op, rhs] = compoundMatch;
    const jsOp = op === ".=" ? "+=" : op;
    return `${name} ${jsOp} ${transformExpr(rhs.trim())};`;
  }

  // --- $arr[] = expr ---
  const pushMatch = stmt.match(/^\$(\w+)\[\]\s*=\s*(.+)$/);
  if (pushMatch) {
    return `${pushMatch[1]}.push(${transformExpr(pushMatch[2].trim())});`;
  }

  // --- $arr[$key] = expr ---
  const indexAssignMatch = stmt.match(/^\$(\w+)\[([^\]]+)\]\s*=\s*(.+)$/);
  if (indexAssignMatch) {
    const [, arr, key, val] = indexAssignMatch;
    return `${arr}[${transformExpr(key)}] = ${transformExpr(val.trim())};`;
  }

  // --- unset($arr[$key]) ---
  const unsetMatch = stmt.match(/^unset\s*\(\s*\$(\w+)\[([^\]]+)\]\s*\)$/);
  if (unsetMatch) {
    return `delete ${unsetMatch[1]}[${transformExpr(unsetMatch[2])}];`;
  }

  // --- sort($arr) ---
  const sortMatch = stmt.match(/^sort\s*\(\s*\$(\w+)\s*\)$/);
  if (sortMatch) {
    return `sort(${sortMatch[1]});`;
  }

  // --- rsort($arr) ---
  const rsortMatch = stmt.match(/^rsort\s*\(\s*\$(\w+)\s*\)$/);
  if (rsortMatch) {
    return `rsort(${rsortMatch[1]});`;
  }

  // --- return expr ---
  const returnMatch = stmt.match(/^return\s+(.+)$/);
  if (returnMatch) {
    return `return ${transformExpr(returnMatch[1].trim())};`;
  }

  // --- return (bare) ---
  if (stmt === "return") {
    return `return;`;
  }

  // --- closing brace ---
  if (stmt === "}") {
    return `}`;
  }

  // --- General: transform and emit ---
  return `${transformExpr(stmt)};`;
}

// ---------------------------------------------------------------------------
// Transform a PHP expression to JavaScript
// ---------------------------------------------------------------------------

function transformExpr(expr: string): string {
  expr = expr.trim();
  if (expr === "") return expr;

  // Ternary: keep as-is but transform parts
  // $x ? "a" : "b" -> x ? "a" : "b"

  // Arrow function: fn($x) => $x * 2
  expr = expr.replace(/\bfn\s*\(([^)]*)\)\s*=>\s*/g, (_, params) => {
    const jsParams = params.split(",").map((p: string) => p.trim().replace(/^\$/, "")).filter(Boolean).join(", ");
    return `(${jsParams}) => `;
  });

  // . concatenation -> + (careful with decimals)
  // Replace " . " with " + "
  expr = expr.replace(/\s+\.\s+/g, " + ");
  // Replace .= (shouldn't appear here but just in case)
  expr = expr.replace(/\.=/g, "+=");

  // $this->method() and $this->prop
  expr = expr.replace(/\$this->(\w+)\s*\(/g, "this.$1(");
  expr = expr.replace(/\$this->(\w+)/g, "this.$1");

  // $var->method() and $var->prop
  expr = expr.replace(/\$(\w+)->(\w+)\s*\(/g, "$1.$2(");
  expr = expr.replace(/\$(\w+)->(\w+)/g, "$1.$2");

  // $arr[$idx] -> arr[idx]
  expr = expr.replace(/\$(\w+)\[([^\]]+)\]/g, (_, arr, idx) => {
    return `${arr}[${idx}]`;
  });

  // $var -> var
  expr = expr.replace(/\$(\w+)/g, "$1");

  // === and !== are the same in JS
  // == in PHP with loose comparison is fine for our subset

  // Transform PHP array literals: [...] with => becomes {...}
  expr = transformArrayLiteral(expr);

  return expr;
}

// ---------------------------------------------------------------------------
// Transform PHP array literal with => into JS object literal
// [key => val, ...] -> {key: val, ...}
// ---------------------------------------------------------------------------

function transformArrayLiteral(expr: string): string {
  // Find [...] that contain =>
  let result = "";
  let i = 0;
  while (i < expr.length) {
    if (expr[i] === "[") {
      // Extract the entire bracket content
      let depth = 1;
      let j = i + 1;
      while (j < expr.length && depth > 0) {
        if (expr[j] === "[") depth++;
        else if (expr[j] === "]") depth--;
        j++;
      }
      const inner = expr.slice(i + 1, j - 1);
      if (inner.includes("=>")) {
        // This is an associative array -> convert to object
        const transformed = inner.replace(/("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\w+)\s*=>\s*/g, (_, key) => {
          // Ensure key is quoted
          if (/^["']/.test(key)) return `${key}: `;
          if (/^\d+$/.test(key)) return `${key}: `;
          return `"${key}": `;
        });
        result += "{" + transformed + "}";
      } else {
        result += "[" + inner + "]";
      }
      i = j;
    } else {
      result += expr[i++];
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Extract PHP function and class declarations for test harness
// ---------------------------------------------------------------------------

export function extractPhpDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let collecting = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed === "<?php" || trimmed === "?>") continue;

    if (!collecting) {
      if (/^(function|class)\s/.test(trimmed)) {
        collecting = true;
        depth = 0;
        result.push(line);
        depth += (line.match(/\{/g) ?? []).length;
        depth -= (line.match(/\}/g) ?? []).length;
        if (depth <= 0 && line.includes("{") && line.includes("}")) {
          collecting = false;
          depth = 0;
        }
      }
    } else {
      result.push(line);
      depth += (line.match(/\{/g) ?? []).length;
      depth -= (line.match(/\}/g) ?? []).length;
      if (depth <= 0) {
        collecting = false;
        depth = 0;
      }
    }
  }
  return result.join("\n").trim();
}

// ---------------------------------------------------------------------------
// Browser runner API
// ---------------------------------------------------------------------------

export async function runPhp(code: string): Promise<RunResult> {
  try {
    const js = transpilePhp(code);
    const captured: string[] = [];
    const origLog = console.log;
    console.log = (...args: unknown[]) => captured.push(args.join(" "));
    try {
      // eslint-disable-next-line no-new-func
      new Function(js)();
    } finally {
      console.log = origLog;
    }
    const stdout = captured.join("\n");
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
      const decls = extractPhpDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }
    const result = await runPhp(codeToRun);
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
