import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let haskellReady = false;

export function isHaskellReady(): boolean {
  return haskellReady;
}

export async function initHaskellRunner(): Promise<void> {
  haskellReady = true;
}

// ---------------------------------------------------------------------------
// Haskell prelude available inside evaluated code
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function putStrLn(s) { __output.push(String(s === undefined ? "" : s) + "\\n"); }
function putStr(s) { __output.push(String(s === undefined ? "" : s)); }
function print(v) { __output.push(__show(v) + "\\n"); }
function __show(v) {
  if (v === null || v === undefined) return "Nothing";
  if (typeof v === "boolean") return v ? "True" : "False";
  if (typeof v === "number") {
    if (Number.isInteger(v)) return String(v);
    // show floats with decimal point like Haskell
    let s = String(v);
    if (!s.includes(".") && !s.includes("e")) s += ".0";
    return s;
  }
  if (typeof v === "string") return JSON.stringify(v);
  if (Array.isArray(v)) {
    // check if it's a string (char array) - we represent strings as JS strings
    return "[" + v.map(__show).join(",") + "]";
  }
  if (v && v.__type === "Just") return "Just " + __show(v.value);
  if (v && v.__type === "Nothing") return "Nothing";
  if (v && v.__type === "Tuple") return "(" + v.values.map(__show).join(",") + ")";
  return String(v);
}
function show(v) { return __show(v); }
function __range(a, b, step) {
  step = step || 1;
  const arr = [];
  if (step > 0) { for (let i = a; i <= b; i += step) arr.push(i); }
  else { for (let i = a; i >= b; i += step) arr.push(i); }
  return arr;
}
function __rangeFrom(a) {
  // infinite list — we cap at 1000 for safety
  const arr = [];
  for (let i = a; i < a + 1000; i++) arr.push(i);
  return arr;
}
function head(xs) { if (!xs.length) throw new Error("Prelude.head: empty list"); return xs[0]; }
function tail(xs) { if (!xs.length) throw new Error("Prelude.tail: empty list"); return xs.slice(1); }
function last(xs) { return xs[xs.length - 1]; }
function init(xs) { return xs.slice(0, -1); }
function length(xs) { return xs.length; }
function null_(xs) { return xs.length === 0; }
function reverse(xs) { return xs.slice().reverse(); }
function map(f, xs) { return xs.map(f); }
function filter(f, xs) { return xs.filter(f); }
function foldl(f, z, xs) { return xs.reduce((acc, x) => f(acc, x), z); }
function foldl1(f, xs) { return xs.reduce((acc, x) => f(acc, x)); }
function foldr(f, z, xs) { return xs.reduceRight((acc, x) => f(x, acc), z); }
function foldr1(f, xs) { return xs.reduceRight((acc, x) => f(x, acc)); }
function sum(xs) { return xs.reduce((a, b) => a + b, 0); }
function product(xs) { return xs.reduce((a, b) => a * b, 1); }
function take(n, xs) { return xs.slice(0, n); }
function drop(n, xs) { return xs.slice(n); }
function takeWhile(f, xs) { const i = xs.findIndex(x => !f(x)); return i === -1 ? xs : xs.slice(0, i); }
function dropWhile(f, xs) { const i = xs.findIndex(x => !f(x)); return i === -1 ? [] : xs.slice(i); }
function zip(xs, ys) { const n = Math.min(xs.length, ys.length); return Array.from({length: n}, (_, i) => ({__type:"Tuple",values:[xs[i],ys[i]]})); }
function zipWith(f, xs, ys) { const n = Math.min(xs.length, ys.length); return Array.from({length: n}, (_, i) => f(xs[i], ys[i])); }
function concat(xss) { return xss.reduce((a, b) => a.concat(b), []); }
function concatMap(f, xs) { return concat(xs.map(f)); }
function elem(x, xs) { return xs.includes(x); }
function notElem(x, xs) { return !xs.includes(x); }
function maximum(xs) { return Math.max(...xs); }
function minimum(xs) { return Math.min(...xs); }
function replicate(n, x) { return Array.from({length: n}, () => x); }
function splitAt(n, xs) { return {__type:"Tuple",values:[xs.slice(0,n),xs.slice(n)]}; }
function span(f, xs) { const i = xs.findIndex(x => !f(x)); const p = i === -1 ? xs : xs.slice(0, i); const q = i === -1 ? [] : xs.slice(i); return {__type:"Tuple",values:[p,q]}; }
function words(s) { return s.trim().split(/\\s+/).filter(x => x.length > 0); }
function unwords(ws) { return ws.join(" "); }
function lines(s) { return s.split("\\n"); }
function unlines(ls) { return ls.map(l => l + "\\n").join(""); }
function div(a, b) { return Math.trunc(a / b); }
function mod(a, b) { return ((a % b) + b) % b; }
function quot(a, b) { return Math.trunc(a / b); }
function rem(a, b) { return a % b; }
function abs(x) { return Math.abs(x); }
function signum(x) { return x > 0 ? 1 : x < 0 ? -1 : 0; }
function negate(x) { return -x; }
function even(x) { return x % 2 === 0; }
function odd(x) { return x % 2 !== 0; }
function fromIntegral(x) { return x; }
function toInteger(x) { return Math.trunc(x); }
function floor(x) { return Math.floor(x); }
function ceiling(x) { return Math.ceil(x); }
function round(x) { return Math.round(x); }
function truncate(x) { return Math.trunc(x); }
function sqrt(x) { return Math.sqrt(x); }
function not(b) { return !b; }
function __and(a, b) { return a && b; }
function __or(a, b) { return a || b; }
function all(f, xs) { return xs.every(f); }
function any(f, xs) { return xs.some(f); }
function flip(f) { return (a, b) => f(b, a); }
function const_(a) { return (_) => a; }
function id(x) { return x; }
function __compose(f, g) { return (x) => f(g(x)); }
function Just(x) { return {__type:"Just", value:x}; }
const Nothing = {__type:"Nothing"};
function fromMaybe(def, m) { return m && m.__type === "Just" ? m.value : def; }
function isJust(m) { return m && m.__type === "Just"; }
function isNothing(m) { return !m || m.__type === "Nothing"; }
function fromJust(m) { if (!m || m.__type !== "Just") throw new Error("fromJust: Nothing"); return m.value; }
function maybe(def, f, m) { return m && m.__type === "Just" ? f(m.value) : def; }
function mapMaybe(f, xs) { return xs.map(f).filter(m => m && m.__type === "Just").map(m => m.value); }
function error(msg) { throw new Error(msg); }
function undefined_() { throw new Error("Prelude.undefined"); }
function __fst(t) { return t.values[0]; }
function __snd(t) { return t.values[1]; }
function fst(t) { return t.values[0]; }
function snd(t) { return t.values[1]; }
function curry(f) { return (a) => (b) => f({__type:"Tuple",values:[a,b]}); }
function uncurry(f) { return (t) => f(t.values[0])(t.values[1]); }
function __numToStr(n) { return String(n); }
function __strToNum(s) { return Number(s); }
function __chr(n) { return String.fromCharCode(n); }
function __ord(c) { return c.charCodeAt(0); }
function intersperse(sep, xs) {
  if (xs.length === 0) return [];
  const res = [xs[0]];
  for (let i = 1; i < xs.length; i++) { res.push(sep); res.push(xs[i]); }
  return res;
}
function intercalate(sep, xss) { return concat(intersperse(sep, xss)); }
function nub(xs) { return [...new Set(xs)]; }
function sort(xs) { return xs.slice().sort((a,b) => a < b ? -1 : a > b ? 1 : 0); }
function sortBy(f, xs) { return xs.slice().sort(f); }
function groupBy(f, xs) {
  if (!xs.length) return [];
  const res = [[xs[0]]];
  for (let i = 1; i < xs.length; i++) {
    if (f(res[res.length-1][0], xs[i])) res[res.length-1].push(xs[i]);
    else res.push([xs[i]]);
  }
  return res;
}
function transpose(xss) {
  if (!xss.length) return [];
  const len = Math.max(...xss.map(xs => xs.length));
  return Array.from({length: len}, (_, i) => xss.map(xs => xs[i]).filter(x => x !== undefined));
}
function lookup(k, pairs) {
  for (const p of pairs) {
    if (p.values[0] === k) return {__type:"Just", value:p.values[1]};
  }
  return Nothing;
}
function iterate(f, x) { const arr = [x]; for(let i=0;i<999;i++) arr.push(f(arr[arr.length-1])); return arr; }
function cycle(xs) { const arr=[]; for(let i=0;i<1000;i++) arr.push(xs[i%xs.length]); return arr; }
function and(xs) { return xs.every(Boolean); }
function or(xs) { return xs.some(Boolean); }
function succ(x) { return x + 1; }
function pred(x) { return x - 1; }
function __showList(xs) {
  // if list of chars (single-char strings), show as string
  if (xs.every(x => typeof x === "string" && x.length === 1)) return JSON.stringify(xs.join(""));
  return "[" + xs.map(__show).join(",") + "]";
}
function __printf(fmt, ...args) {
  let i = 0;
  return fmt.replace(/%[sdif.0-9]+/g, (m) => {
    const v = args[i++];
    if (m === "%d" || m === "%i") return String(Math.trunc(v));
    if (m === "%s") return String(v);
    if (m.includes("f")) {
      const places = parseInt((m.match(/\\.([0-9]+)f/) || [0,6])[1]);
      return v.toFixed(places);
    }
    return String(v);
  });
}
`;

// ---------------------------------------------------------------------------
// Transpiler: Haskell subset → JavaScript
// ---------------------------------------------------------------------------

function transpileHaskell(code: string): string {
  const lines = code.split("\n");
  const outputLines: string[] = [PRELUDE];

  // Remove type signatures, LANGUAGE pragmas, module declarations
  const filtered = lines.filter((line) => {
    const t = line.trim();
    if (t.startsWith("{-#") || t.startsWith("module ") || t.startsWith("import ")) return false;
    if (/^\w[\w']*\s*::/.test(t)) return false; // type signature
    return true;
  });

  // Join continuation lines: a logical line is one that begins with an identifier or 'main'
  // We group indented lines with the previous top-level line
  const logical: string[] = [];
  for (const line of filtered) {
    if (line === "" || /^\s+/.test(line) || line.startsWith("  ") || line.startsWith("\t")) {
      if (logical.length > 0) logical[logical.length - 1] += "\n" + line;
      else logical.push(line);
    } else {
      logical.push(line);
    }
  }

  // Parse top-level declarations
  // Each logical entry is either:
  //   main = do ...
  //   funcName arg1 arg2 ... = body
  //   funcName arg1 arg2
  //     | guard1 = expr1
  //     | guard2 = expr2
  const decls: { name: string; raw: string }[] = [];
  for (const block of logical) {
    const trimmed = block.trim();
    if (!trimmed) continue;
    const m = trimmed.match(/^([a-z_][a-zA-Z0-9_']*)\s*/);
    if (m) {
      decls.push({ name: m[1], raw: block });
    }
  }

  // Group multiple equations for the same function (pattern matching)
  const grouped = new Map<string, string[]>();
  const order: string[] = [];
  for (const d of decls) {
    if (!grouped.has(d.name)) {
      grouped.set(d.name, []);
      order.push(d.name);
    }
    grouped.get(d.name)!.push(d.raw);
  }

  for (const name of order) {
    const equations = grouped.get(name)!;
    if (name === "main") {
      outputLines.push(transpileMain(equations[0]));
    } else {
      outputLines.push(transpileFunction(name, equations));
    }
  }

  outputLines.push(`main();`);
  return outputLines.join("\n");
}

// Transpile the main function
function transpileMain(raw: string): string {
  // main = expr  OR  main = do { stmts }
  const body = raw.replace(/^main\s*=\s*/, "").trim();
  if (body.startsWith("do\n") || body === "do" || body.startsWith("do ")) {
    const doBody = body.replace(/^do\s*/, "");
    return `function main() {\n${transpileDoBlock(doBody)}\n}`;
  }
  // single expression (e.g. main = putStrLn "hello")
  return `function main() {\n  ${transpileExpr(body)};\n}`;
}

// Transpile a do-block body (without the leading 'do')
function transpileDoBlock(body: string): string {
  // Split into statements. Each line that is not indented relative to the first is a new statement.
  const lines = body.split("\n");
  const stmts: string[] = [];
  let current = "";
  const baseIndent = lines.find((l) => l.trim())?.match(/^(\s*)/)?.[1].length ?? 0;

  for (const line of lines) {
    if (!line.trim()) continue;
    const indent = line.match(/^(\s*)/)?.[1].length ?? 0;
    if (indent <= baseIndent && current) {
      stmts.push(current);
      current = line;
    } else if (!current) {
      current = line;
    } else {
      current += "\n" + line;
    }
  }
  if (current.trim()) stmts.push(current);

  return stmts.map((s) => transpileDoStmt(s.trim())).join("\n");
}

function transpileDoStmt(stmt: string): string {
  // let x = expr
  if (/^let\s+/.test(stmt)) {
    const inner = stmt.replace(/^let\s+/, "").trim();
    // Could be multi-line let block
    const letLines = inner.split("\n");
    return letLines.map((l) => {
      const lm = l.trim().match(/^([a-zA-Z_][a-zA-Z0-9_']*)\s*=\s*(.+)$/);
      if (lm) return `  let ${lm[1]} = ${transpileExpr(lm[2])};`;
      return `  // ${l}`;
    }).join("\n");
  }
  // var <- expr  (monadic bind — just evaluate the expr for our purposes)
  if (/^[a-zA-Z_][a-zA-Z0-9_']*\s*<-/.test(stmt)) {
    const [varPart, ...rest] = stmt.split("<-");
    return `  let ${varPart.trim()} = ${transpileExpr(rest.join("<-").trim())};`;
  }
  // return expr
  if (/^return\s+/.test(stmt)) {
    const expr = stmt.replace(/^return\s+/, "").trim();
    return `  ${transpileExpr(expr)};`;
  }
  // Plain expression (e.g. putStrLn ...)
  return `  ${transpileExpr(stmt)};`;
}

// Transpile a function definition (possibly with guards / pattern matching)
function transpileFunction(name: string, equations: string[]): string {
  // Parse each equation to get params and body
  const parsed = equations.map((eq) => parseEquation(name, eq));

  if (parsed.length === 1 && !parsed[0].guards) {
    // Simple single-equation function
    const { params, body } = parsed[0];
    const paramStr = params.join(", ");
    return `function ${name}(${paramStr}) {\n${transpileBody(body ?? "", params)}\n}`;
  }

  // Multiple equations or guards: generate with if/else
  const maxParams = Math.max(...parsed.map((p) => p.params.length));
  const paramNames = Array.from({ length: maxParams }, (_, i) => `__p${i}`);
  const paramStr = paramNames.join(", ");

  const branches: string[] = [];

  for (const eq of parsed) {
    const { params, body, guards } = eq;
    // Build pattern conditions
    const conditions: string[] = [];
    for (let i = 0; i < params.length; i++) {
      const p = params[i];
      if (p === "_") continue; // wildcard
      if (/^[A-Z]/.test(p)) {
        // Constructor pattern (e.g. True, False, Nothing, Just)
        if (p === "True") conditions.push(`${paramNames[i]} === true`);
        else if (p === "False") conditions.push(`${paramNames[i]} === false`);
        else if (p === "Nothing") conditions.push(`${paramNames[i]}.__type === "Nothing"`);
        else if (/^\d+/.test(p)) conditions.push(`${paramNames[i]} === ${p}`);
        else conditions.push(`${paramNames[i]}.__type === "${p}"`);
      } else if (/^-?\d+$/.test(p)) {
        conditions.push(`${paramNames[i]} === ${p}`);
      } else if (p.startsWith('"') || p.startsWith("'")) {
        conditions.push(`${paramNames[i]} === ${p}`);
      } else if (p === "[]") {
        conditions.push(`${paramNames[i]}.length === 0`);
      } else if (p.includes(":")) {
        // x:xs pattern
        conditions.push(`${paramNames[i]}.length > 0`);
      }
    }

    // Build variable bindings from patterns
    const bindings: string[] = [];
    for (let i = 0; i < params.length; i++) {
      const p = params[i];
      if (p === "_" || p === "[]") continue;
      if (/^[a-z_][a-zA-Z0-9_']*$/.test(p) && !/^(true|false)$/.test(p)) {
        bindings.push(`  let ${p} = ${paramNames[i]};`);
      } else if (p.includes(":")) {
        const parts = p.split(":").map((s) => s.trim());
        if (parts.length === 2) {
          bindings.push(`  let ${parts[0]} = ${paramNames[i]}[0];`);
          bindings.push(`  let ${parts[1]} = ${paramNames[i]}.slice(1);`);
        }
      } else if (p.startsWith("(Just ")) {
        const inner = p.replace(/^\(Just\s+/, "").replace(/\)$/, "").trim();
        bindings.push(`  let ${inner} = ${paramNames[i]}.value;`);
      }
    }

    const condStr = conditions.length > 0 ? conditions.join(" && ") : "";

    if (guards && guards.length > 0) {
      const guardCode = guards.map(({ cond, expr }, idx) => {
        const c = cond.trim() === "otherwise" ? "true" : transpileExpr(cond);
        const prefix = idx === 0 ? "if" : "else if";
        return `  ${prefix} (${c}) { return ${transpileExpr(expr)}; }`;
      }).join("\n");

      if (condStr) {
        branches.push(`if (${condStr}) {\n${bindings.join("\n")}\n${guardCode}\n}`);
      } else {
        branches.push(`{\n${bindings.join("\n")}\n${guardCode}\n  return undefined;\n}`);
      }
    } else {
      const bodyCode = transpileBody(body!, params, paramNames);
      if (condStr) {
        branches.push(`if (${condStr}) {\n${bindings.join("\n")}\n${bodyCode}\n}`);
      } else {
        branches.push(`{\n${bindings.join("\n")}\n${bodyCode}\n}`);
      }
    }
  }

  let fnBody: string;
  if (branches.length === 1 && !branches[0].startsWith("if")) {
    fnBody = branches[0].replace(/^\{|\}$/g, "").trim();
  } else {
    fnBody = branches.join(" else ");
  }

  return `function ${name}(${paramStr}) {\n${fnBody}\n}`;
}

interface ParsedEquation {
  params: string[];
  body?: string;
  guards?: Array<{ cond: string; expr: string }>;
}

function parseEquation(name: string, raw: string): ParsedEquation {
  const lines = raw.split("\n");
  const firstLine = lines[0].trim();

  // Remove function name prefix
  const afterName = firstLine.replace(new RegExp(`^${name}\\s*`), "").trim();

  // Check for guards (lines starting with |)
  const hasGuards = lines.slice(1).some((l) => /^\s*\|/.test(l)) || /^\s*\|/.test(afterName);

  if (hasGuards) {
    // Parse params from first line (before any =)
    const paramsPart = afterName.split("|")[0].trim();
    const params = parseParams(paramsPart);

    const guardLines = lines.filter((l) => /^\s*\|/.test(l));
    const guards = guardLines.map((l) => {
      const m = l.trim().match(/^\|\s*(.+?)\s*=\s*(.+)$/);
      if (m) return { cond: m[1], expr: m[2] };
      return { cond: "otherwise", expr: "undefined" };
    });

    return { params, guards };
  }

  // Simple equation: name params = body
  const eqIdx = afterName.indexOf("=");
  if (eqIdx === -1) return { params: [], body: "undefined" };

  const paramsPart = afterName.slice(0, eqIdx).trim();
  const bodyPart = afterName.slice(eqIdx + 1).trim();

  // Collect continuation lines
  const continuations = lines.slice(1).filter((l) => !(/^\s*where\s/.test(l)));
  const whereLines = lines.slice(1).filter((l) => /^\s*where\b/.test(l) || false);

  let fullBody = bodyPart;
  if (continuations.length > 0) {
    fullBody += "\n" + continuations.join("\n");
  }

  const params = parseParams(paramsPart);
  return { params, body: fullBody };
}

function parseParams(str: string): string[] {
  if (!str.trim()) return [];
  // Simple tokenization — handles: x, (x,y), (Just x), x:xs
  const params: string[] = [];
  let depth = 0;
  let current = "";
  for (const ch of str) {
    if (ch === "(" || ch === "[") { depth++; current += ch; }
    else if (ch === ")" || ch === "]") { depth--; current += ch; }
    else if (ch === " " && depth === 0) {
      if (current) { params.push(current); current = ""; }
    } else {
      current += ch;
    }
  }
  if (current) params.push(current);
  return params;
}

function transpileBody(body: string, params: string[], paramNames?: string[]): string {
  const lines = body.split("\n");

  // Check for where clause
  const whereIdx = lines.findIndex((l) => /^\s*where\b/.test(l));
  let mainBody = body;
  let whereCode = "";

  if (whereIdx !== -1) {
    mainBody = lines.slice(0, whereIdx).join("\n").trim();
    const wherePart = lines.slice(whereIdx + 1).join("\n");
    whereCode = transpileWhere(wherePart);
  }

  // Bind named params
  const bindings: string[] = [];
  if (paramNames) {
    for (let i = 0; i < params.length; i++) {
      const p = params[i];
      if (p === "_" || p === "[]") continue;
      if (/^[a-z_][a-zA-Z0-9_']*$/.test(p)) {
        bindings.push(`  let ${p} = ${paramNames[i]};`);
      } else if (p.includes(":")) {
        const parts = p.split(":").map((s) => s.trim());
        bindings.push(`  let ${parts[0]} = ${paramNames[i]}[0];`);
        bindings.push(`  let ${parts[1]} = ${paramNames[i]}.slice(1);`);
      }
    }
  }

  const expr = transpileExpr(mainBody.trim());
  return `${bindings.join("\n")}${whereCode}\n  return ${expr};`;
}

function transpileWhere(whereBody: string): string {
  const lines = whereBody.split("\n").map((l) => l.trim()).filter(Boolean);
  return lines.map((l) => {
    const m = l.match(/^([a-zA-Z_][a-zA-Z0-9_']*)\s*=\s*(.+)$/);
    if (m) return `  let ${m[1]} = ${transpileExpr(m[2])};`;
    return `  // ${l}`;
  }).join("\n");
}

// ---------------------------------------------------------------------------
// Expression transpiler
// ---------------------------------------------------------------------------

function transpileExpr(expr: string): string {
  expr = expr.trim();
  if (!expr) return "undefined";

  // Multi-line expressions: if first line is a do block
  if (expr.startsWith("do\n") || expr === "do") {
    const body = expr.replace(/^do\s*/, "");
    return `(() => { ${transpileDoBlock(body)} })()`;
  }

  // let...in expression
  const letInMatch = expr.match(/^let\s+([\s\S]+?)\s+in\s+([\s\S]+)$/);
  if (letInMatch) {
    const bindings = letInMatch[1];
    const inExpr = letInMatch[2];
    const bindLines = bindings.split(";").map((b) => b.trim());
    const bindCode = bindLines.map((b) => {
      const m = b.match(/^([a-zA-Z_][a-zA-Z0-9_']*)\s*=\s*(.+)$/);
      if (m) return `let ${m[1]} = ${transpileExpr(m[2])};`;
      return `// ${b}`;
    }).join(" ");
    return `(() => { ${bindCode} return ${transpileExpr(inExpr)}; })()`;
  }

  // if/then/else
  const ifMatch = matchIfThenElse(expr);
  if (ifMatch) {
    const { cond, then_, else_ } = ifMatch;
    return `(${transpileExpr(cond)} ? ${transpileExpr(then_)} : ${transpileExpr(else_)})`;
  }

  // case expression
  if (/^case\s+/.test(expr)) {
    return transpileCase(expr);
  }

  // Lambda: \x y -> expr
  const lambdaMatch = expr.match(/^\\([\w\s']+)\s*->\s*([\s\S]+)$/);
  if (lambdaMatch) {
    const paramStr = lambdaMatch[1].trim().split(/\s+/).join(", ");
    return `((${paramStr}) => ${transpileExpr(lambdaMatch[2])})`;
  }

  // Range [a..b] or [a,b..c] or [a..]
  const rangeMatch = expr.match(/^\[(-?[\w.]+)\s*,\s*(-?[\w.]+)\s*\.\.\s*(-?[\w.]+)?\]$/);
  if (rangeMatch) {
    const a = transpileExpr(rangeMatch[1]);
    const b = transpileExpr(rangeMatch[2]);
    const step = `(${b}) - (${a})`;
    if (rangeMatch[3]) return `__range(${a}, ${transpileExpr(rangeMatch[3])}, ${step})`;
    return `__rangeFrom(${a})`;
  }
  const range2Match = expr.match(/^\[(-?[\w.]+)\s*\.\.\s*(-?[\w.]+)\]$/);
  if (range2Match) {
    return `__range(${transpileExpr(range2Match[1])}, ${transpileExpr(range2Match[2])})`;
  }
  const rangeFromMatch = expr.match(/^\[(-?[\w.]+)\s*\.\.\]$/);
  if (rangeFromMatch) {
    return `__rangeFrom(${transpileExpr(rangeFromMatch[1])})`;
  }

  // List comprehension [expr | x <- list, pred]
  const lcMatch = matchListComp(expr);
  if (lcMatch) return transpileListComp(lcMatch);

  // List literal
  if (expr.startsWith("[") && expr.endsWith("]")) {
    const inner = expr.slice(1, -1).trim();
    if (!inner) return "[]";
    // check it's not a range (handled above)
    const items = splitCommas(inner);
    return `[${items.map(transpileExpr).join(", ")}]`;
  }

  // Tuple
  if (expr.startsWith("(") && expr.endsWith(")")) {
    const inner = expr.slice(1, -1).trim();
    const items = splitCommas(inner);
    if (items.length >= 2) {
      return `{__type:"Tuple",values:[${items.map(transpileExpr).join(", ")}]}`;
    }
    // Parenthesized expression
    return `(${transpileExpr(inner)})`;
  }

  // String literal
  if (expr.startsWith('"') || expr.startsWith("'")) return expr;

  // Number literal
  if (/^-?\d+(\.\d+)?$/.test(expr)) return expr;

  // Boolean literals
  if (expr === "True") return "true";
  if (expr === "False") return "false";
  if (expr === "Nothing") return "Nothing";

  // Operators: handle infix with backticks `func`
  if (/`[a-z][a-zA-Z0-9_']*`/.test(expr)) {
    return transpileInfix(expr);
  }

  // Operator expressions (binary)
  const opExpr = transpileBinaryOp(expr);
  if (opExpr) return opExpr;

  // Function application: f arg1 arg2 ...
  const appMatch = tryFunctionApplication(expr);
  if (appMatch) return appMatch;

  // Fallback: identifier
  return expr;
}

function matchIfThenElse(expr: string): { cond: string; then_: string; else_: string } | null {
  if (!/^if\s/.test(expr)) return null;
  // Find 'then' and 'else' at the same paren depth
  let depth = 0;
  let thenIdx = -1;
  let elseIdx = -1;
  let i = 0;
  const tokens = tokenize(expr);

  let pos = 0;
  for (let ti = 0; ti < tokens.length; ti++) {
    const t = tokens[ti];
    if (t === "(" || t === "[") depth++;
    else if (t === ")" || t === "]") depth--;
    else if (t === "then" && depth === 0 && thenIdx === -1) {
      thenIdx = pos;
    } else if (t === "else" && depth === 0 && thenIdx !== -1) {
      elseIdx = pos;
      break;
    }
    pos += t.length + (ti < tokens.length - 1 ? 0 : 0);
  }

  // Use string search approach
  const thenPos = findKeyword(expr, "then");
  const elsePos = findKeyword(expr, "else");
  if (thenPos === -1 || elsePos === -1) return null;

  const cond = expr.slice(3, thenPos).trim();
  const then_ = expr.slice(thenPos + 4, elsePos).trim();
  const else_ = expr.slice(elsePos + 4).trim();
  return { cond, then_, else_ };
}

function findKeyword(expr: string, kw: string): number {
  let depth = 0;
  for (let i = 0; i <= expr.length - kw.length; i++) {
    const ch = expr[i];
    if (ch === "(" || ch === "[") { depth++; continue; }
    if (ch === ")" || ch === "]") { depth--; continue; }
    if (ch === '"') {
      i++;
      while (i < expr.length && expr[i] !== '"') { if (expr[i] === "\\") i++; i++; }
      continue;
    }
    if (depth === 0 && expr.slice(i, i + kw.length) === kw) {
      const before = i === 0 ? " " : expr[i - 1];
      const after = i + kw.length >= expr.length ? " " : expr[i + kw.length];
      if (/\s/.test(before) && /\s/.test(after)) return i;
    }
  }
  return -1;
}

function tokenize(s: string): string[] {
  return s.match(/\S+/g) ?? [];
}

function transpileInfix(expr: string): string {
  return expr.replace(/(.+?)\s+`([a-z][a-zA-Z0-9_']*)`\s+(.+)/, (_m, a, fn, b) => {
    return `${fn}(${transpileExpr(a)}, ${transpileExpr(b)})`;
  });
}

const BINARY_OPS: [string, string][] = [
  ["++", "+"],
  ["&&", "&&"],
  ["||", "||"],
  ["==", "==="],
  ["/=", "!=="],
  [">=", ">="],
  ["<=", "<="],
  [">", ">"],
  ["<", "<"],
  ["^", "**"],
  ["+", "+"],
  ["-", "-"],
  ["*", "*"],
  ["/", "/"],
  [":", "__cons"],
  ["$", "__apply"],
  [".", "__compose"],
];

function transpileBinaryOp(expr: string): string | null {
  for (const [op, jsOp] of BINARY_OPS) {
    const idx = findOperator(expr, op);
    if (idx !== -1) {
      const left = expr.slice(0, idx).trim();
      const right = expr.slice(idx + op.length).trim();
      if (!left || !right) continue;
      if (jsOp === "__cons") {
        return `[${transpileExpr(left)}, ...${transpileExpr(right)}]`;
      }
      if (jsOp === "__apply") {
        return `${transpileExpr(left)}(${transpileExpr(right)})`;
      }
      if (jsOp === "__compose") {
        return `__compose(${transpileExpr(left)}, ${transpileExpr(right)})`;
      }
      return `(${transpileExpr(left)} ${jsOp} ${transpileExpr(right)})`;
    }
  }
  return null;
}

function findOperator(expr: string, op: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = expr.length - op.length; i >= 0; i--) {
    const ch = expr[i];
    if (ch === '"' && !inStr) { inStr = true; continue; }
    if (ch === '"' && inStr) { inStr = false; continue; }
    if (inStr) continue;
    if (ch === ")" || ch === "]") { depth++; continue; }
    if (ch === "(" || ch === "[") { depth--; continue; }
    if (depth === 0 && expr.slice(i, i + op.length) === op) {
      // Make sure it's actually this operator (not part of another)
      const before = i > 0 ? expr[i - 1] : " ";
      const after = i + op.length < expr.length ? expr[i + op.length] : " ";
      // For single-char operators, don't match multi-char
      if (op === ">" && (before === "=" || after === "=")) continue;
      if (op === "<" && (before === "=" || after === "=")) continue;
      if (op === "+" && before === "+") continue;
      if (op === "-" && before === "-") continue;
      if (op === "/" && after === "=") continue;
      if (op === "." && (after === "." || before === ".")) continue;
      if (op === "*" && before === "*") continue;
      return i;
    }
  }
  return -1;
}

function tryFunctionApplication(expr: string): string | null {
  // Match: identifier followed by arguments
  const m = expr.match(/^([a-zA-Z_][a-zA-Z0-9_']*)(\s+[\s\S]+)$/);
  if (!m) return null;
  const fn = m[1];
  const rest = m[2].trim();

  // Parse arguments (respecting parentheses)
  const args = parseArgs(rest);
  if (args.length === 0) return fn;

  return `${fn}(${args.map(transpileExpr).join(", ")})`;
}

function parseArgs(str: string): string[] {
  const args: string[] = [];
  let depth = 0;
  let current = "";
  let inStr = false;
  let i = 0;
  while (i < str.length) {
    const ch = str[i];
    if (ch === '"' && !inStr) { inStr = true; current += ch; }
    else if (ch === '"' && inStr) { inStr = false; current += ch; }
    else if (inStr) { if (ch === "\\") { current += ch + str[++i]; } else current += ch; }
    else if (ch === "(" || ch === "[") { depth++; current += ch; }
    else if (ch === ")" || ch === "]") { depth--; current += ch; }
    else if (ch === " " && depth === 0) {
      if (current.trim()) { args.push(current.trim()); current = ""; }
    } else {
      current += ch;
    }
    i++;
  }
  if (current.trim()) args.push(current.trim());
  return args;
}

function splitCommas(str: string): string[] {
  const items: string[] = [];
  let depth = 0;
  let current = "";
  let inStr = false;
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    if (ch === '"' && !inStr) { inStr = true; current += ch; }
    else if (ch === '"' && inStr) { inStr = false; current += ch; }
    else if (inStr) { current += ch; }
    else if (ch === "(" || ch === "[") { depth++; current += ch; }
    else if (ch === ")" || ch === "]") { depth--; current += ch; }
    else if (ch === "," && depth === 0) { items.push(current.trim()); current = ""; }
    else { current += ch; }
  }
  if (current.trim()) items.push(current.trim());
  return items;
}

interface ListCompMatch {
  expr: string;
  generators: Array<{ var_: string; list: string }>;
  guards: string[];
}

function matchListComp(expr: string): ListCompMatch | null {
  if (!expr.startsWith("[")) return null;
  if (!expr.endsWith("]")) return null;
  const inner = expr.slice(1, -1);
  const pipeIdx = findPipe(inner);
  if (pipeIdx === -1) return null;

  const outputExpr = inner.slice(0, pipeIdx).trim();
  const qualifiers = inner.slice(pipeIdx + 1).trim();
  const parts = splitCommas(qualifiers);

  const generators: Array<{ var_: string; list: string }> = [];
  const guards: string[] = [];

  for (const part of parts) {
    const genMatch = part.match(/^([a-zA-Z_][a-zA-Z0-9_']*)\s*<-\s*(.+)$/);
    if (genMatch) {
      generators.push({ var_: genMatch[1], list: genMatch[2].trim() });
    } else {
      guards.push(part.trim());
    }
  }

  return { expr: outputExpr, generators, guards };
}

function findPipe(str: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    if (ch === '"') { inStr = !inStr; continue; }
    if (inStr) continue;
    if (ch === "(" || ch === "[") depth++;
    else if (ch === ")" || ch === "]") depth--;
    else if (ch === "|" && depth === 0) return i;
  }
  return -1;
}

function transpileListComp(lc: ListCompMatch): string {
  // Build nested array comprehension
  let code = `((() => { const __result = []; `;
  for (const gen of lc.generators) {
    code += `for (const ${gen.var_} of ${transpileExpr(gen.list)}) { `;
  }
  if (lc.guards.length > 0) {
    code += `if (${lc.guards.map(transpileExpr).join(" && ")}) { `;
  }
  code += `__result.push(${transpileExpr(lc.expr)}); `;
  if (lc.guards.length > 0) code += "} ";
  for (const _gen of lc.generators) code += "} ";
  code += `return __result; })())`;
  return code;
}

function transpileCase(expr: string): string {
  const m = expr.match(/^case\s+([\s\S]+?)\s+of\s*([\s\S]*)$/);
  if (!m) return "undefined";
  const scrutinee = transpileExpr(m[1]);
  const altsStr = m[2];
  const alts = altsStr.split("\n").map((l) => l.trim()).filter(Boolean);

  const branches: string[] = [];
  for (const alt of alts) {
    const arrowIdx = alt.indexOf("->");
    if (arrowIdx === -1) continue;
    const pat = alt.slice(0, arrowIdx).trim();
    const rhs = alt.slice(arrowIdx + 2).trim();

    if (pat === "_") {
      branches.push(`else { return ${transpileExpr(rhs)}; }`);
    } else if (pat === "Nothing") {
      branches.push(`if (__s.__type === "Nothing") { return ${transpileExpr(rhs)}; }`);
    } else if (pat.startsWith("Just ")) {
      const inner = pat.slice(5).trim();
      branches.push(`if (__s.__type === "Just") { let ${inner} = __s.value; return ${transpileExpr(rhs)}; }`);
    } else if (pat === "True") {
      branches.push(`if (__s === true) { return ${transpileExpr(rhs)}; }`);
    } else if (pat === "False") {
      branches.push(`if (__s === false) { return ${transpileExpr(rhs)}; }`);
    } else if (/^[a-z_]/.test(pat)) {
      branches.push(`else { let ${pat} = __s; return ${transpileExpr(rhs)}; }`);
    } else {
      branches.push(`if (__s === ${transpileExpr(pat)}) { return ${transpileExpr(rhs)}; }`);
    }
  }

  return `((__s) => { ${branches.join(" ")} })(${scrutinee})`;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function extractHaskellFunctions(code: string): string {
  // Return only non-main top-level definitions
  const lines = code.split("\n");
  const result: string[] = [];
  let inMain = false;

  for (const line of lines) {
    const t = line.trim();
    if (/^main\s*=/.test(t) || /^main\s*$/.test(t)) {
      inMain = true;
      continue;
    }
    if (inMain && line.startsWith(" ")) continue;
    inMain = false;
    if (t && !/^\w[\w']*\s*::/.test(t)) result.push(line);
  }
  return result.join("\n").trim();
}

export async function runHaskell(code: string): Promise<RunResult> {
  try {
    const js = transpileHaskell(code);
    // eslint-disable-next-line no-new-func
    const fn = new Function(js + "\nreturn __output.join('');");
    const output = fn();
    return { stdout: output, stderr: "", error: "" };
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    return { stdout: "", stderr: msg, error: msg };
  }
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];

  for (const test of tests) {
    let codeToRun = code;
    if (test.code) {
      const fns = extractHaskellFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", fns);
    }

    const result = await runHaskell(codeToRun);
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
