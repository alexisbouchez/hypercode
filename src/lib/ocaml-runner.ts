import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let ocamlReady = false;
export function isOCamlReady(): boolean { return ocamlReady; }
export async function initOCamlRunner(): Promise<void> { ocamlReady = true; }

// ---------------------------------------------------------------------------
// OCaml -> JavaScript transpiler
// Handles the subset of OCaml used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function print_endline(s) { __output.push(String(s) + '\\n'); }
function print_string(s) { __output.push(String(s)); }
function print_int(n) { __output.push(String(n)); }
function print_float(f) { __output.push(String(f)); }
function print_newline() { __output.push('\\n'); }
function print_char(c) { __output.push(c); }
function string_of_int(n) { return String(n); }
function string_of_float(f) {
  let s = String(f);
  if (!s.includes('.')) s += '.';
  return s;
}
function string_of_bool(b) { return b ? 'true' : 'false'; }
function int_of_string(s) { return parseInt(s); }
function float_of_string(s) { return parseFloat(s); }
function int_of_float(f) { return Math.trunc(f); }
function float_of_int(n) { return n; }
function abs(x) { return Math.abs(x); }
function abs_float(x) { return Math.abs(x); }
function min(a, b) { return Math.min(a, b); }
function max(a, b) { return Math.max(a, b); }
function succ(n) { return n + 1; }
function pred(n) { return n - 1; }
function fst(t) { return t[0]; }
function snd(t) { return t[1]; }
function not(b) { return !b; }
function ignore(_v) { return undefined; }

const List = {
  length: (lst) => lst.length,
  hd: (lst) => lst[0],
  tl: (lst) => lst.slice(1),
  rev: (lst) => [...lst].reverse(),
  nth: (lst, n) => lst[n],
  map: (f, lst) => lst.map(f),
  mapi: (f, lst) => lst.map((x, i) => f(i, x)),
  filter: (f, lst) => lst.filter(f),
  fold_left: (f, init, lst) => lst.reduce((acc, x) => f(acc, x), init),
  fold_right: (f, lst, init) => lst.reduceRight((acc, x) => f(x, acc), init),
  iter: (f, lst) => { lst.forEach(f); },
  for_all: (f, lst) => lst.every(f),
  exists: (f, lst) => lst.some(f),
  mem: (x, lst) => lst.includes(x),
  find: (f, lst) => lst.find(f),
  sort: (cmp, lst) => [...lst].sort(cmp),
  append: (a, b) => [...a, ...b],
  concat: (lsts) => lsts.flat(),
  flatten: (lsts) => lsts.flat(),
  init: (n, f) => Array.from({length: n}, (_, i) => f(i)),
  assoc: (k, lst) => { const p = lst.find(t => t[0] === k); return p ? p[1] : undefined; },
};

const String_ = {
  length: (s) => s.length,
  sub: (s, start, len) => s.substring(start, start + len),
  uppercase_ascii: (s) => s.toUpperCase(),
  lowercase_ascii: (s) => s.toLowerCase(),
  concat: (sep, lst) => lst.join(sep),
  contains: (s, c) => s.includes(c),
  trim: (s) => s.trim(),
  make: (n, c) => c.repeat(n),
  get: (s, i) => s[i],
  split_on_char: (c, s) => s.split(c),
};

const Printf = {
  printf: (fmt, ...args) => {
    let i = 0;
    const result = fmt.replace(/%[sdifb.0-9]*/g, (m) => {
      const v = args[i++];
      if (m === '%d' || m === '%i') return String(Math.trunc(v));
      if (m === '%s') return String(v);
      if (m === '%b') return String(v);
      if (m.includes('f')) {
        const places = parseInt((m.match(/\\.([0-9]+)f/) || [0,6])[1]);
        return v.toFixed(places);
      }
      return String(v);
    });
    __output.push(result);
  },
};

function compare(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

const None = null;
function Some(v) { return v; }

const __ref = (v) => ({ contents: v });
const __deref = (r) => r.contents;
const __assign = (r, v) => { r.contents = v; };
`;

// ---------------------------------------------------------------------------
// Transpiler: OCaml subset -> JavaScript
// ---------------------------------------------------------------------------

export function transpileOCaml(code: string): string {
  // Remove OCaml comments (* ... *)
  code = removeComments(code);

  // Remove ;; separators
  code = code.replace(/;;\s*/g, "\n");

  // Remove type annotations after colons in let/function defs
  // but be careful not to break other uses of :
  // We handle this per-construct below

  const lines = code.split("\n");
  const out: string[] = [PRELUDE];

  let i = 0;
  while (i < lines.length) {
    const result = processLine(lines, i);
    if (result.output !== null) {
      out.push(result.output);
    }
    i = result.nextIndex;
  }

  out.push('\n__output.join("")');
  return out.join("\n");
}

function removeComments(code: string): string {
  let result = "";
  let i = 0;
  while (i < code.length) {
    if (code[i] === "(" && code[i + 1] === "*") {
      let depth = 1;
      i += 2;
      while (i < code.length && depth > 0) {
        if (code[i] === "(" && code[i + 1] === "*") { depth++; i += 2; }
        else if (code[i] === "*" && code[i + 1] === ")") { depth--; i += 2; }
        else i++;
      }
    } else if (code[i] === '"') {
      result += code[i++];
      while (i < code.length && code[i] !== '"') {
        if (code[i] === '\\') { result += code[i++]; }
        result += code[i++];
      }
      if (i < code.length) result += code[i++];
    } else {
      result += code[i++];
    }
  }
  return result;
}

interface ProcessResult {
  output: string | null;
  nextIndex: number;
}

function processLine(lines: string[], index: number): ProcessResult {
  const raw = lines[index];
  const trimmed = raw.trim();

  // Skip blank lines
  if (trimmed === "") {
    return { output: "", nextIndex: index + 1 };
  }

  // Skip type definitions
  if (/^type\s+/.test(trimmed)) {
    // Consume entire type definition (may span multiple lines)
    let j = index + 1;
    while (j < lines.length) {
      const t = lines[j].trim();
      if (t === "" || (!t.startsWith("|") && !t.startsWith("and ") && !t.startsWith("{") && !t.startsWith("}") && !t.includes(";"))) break;
      j++;
    }
    return { output: null, nextIndex: j };
  }

  // Skip open/module declarations
  if (/^open\s+/.test(trimmed) || /^module\s+/.test(trimmed)) {
    return { output: null, nextIndex: index + 1 };
  }

  // let rec function definition
  const letRecMatch = trimmed.match(/^let\s+rec\s+(\w+)\s*(.*)/);
  if (letRecMatch) {
    return processLetRec(lines, index, letRecMatch[1], letRecMatch[2]);
  }

  // let function/value definition (with parameters = function)
  const letMatch = trimmed.match(/^let\s+(\w+)\s*(.*)/);
  if (letMatch) {
    return processLet(lines, index, letMatch[1], letMatch[2]);
  }

  // Standalone expression (e.g., print_endline "hello")
  const js = transpileExpr(trimmed);
  return { output: js + ";", nextIndex: index + 1 };
}

function processLetRec(lines: string[], index: number, name: string, rest: string): ProcessResult {
  // Collect the full body (may span multiple lines)
  const { params, body } = parseLetBinding(lines, index, rest);

  if (params.length === 0) {
    // Recursive value (unusual but possible)
    const jsBody = transpileExpr(body);
    return { output: `let ${name} = ${jsBody};`, nextIndex: findEndIndex(lines, index, body) };
  }

  const jsBody = transpileBody(body);
  const endIdx = findEndIndex(lines, index, body);
  return {
    output: `function ${name}(${params.join(", ")}) { return ${jsBody}; }`,
    nextIndex: endIdx,
  };
}

function processLet(lines: string[], index: number, name: string, rest: string): ProcessResult {
  const { params, body, hasIn, inExpr } = parseLetBinding(lines, index, rest);

  const endIdx = findEndIndex(lines, index, body + (inExpr ? " in " + inExpr : ""));

  if (hasIn) {
    // let x = val in expr  ->  IIFE
    if (params.length === 0) {
      const jsVal = transpileExpr(body);
      const jsIn = transpileExpr(inExpr!);
      return {
        output: `(function() { let ${name} = ${jsVal}; return ${jsIn}; })()`,
        nextIndex: endIdx,
      };
    } else {
      const jsBody = transpileBody(body);
      const jsIn = transpileExpr(inExpr!);
      return {
        output: `(function() { function ${name}(${params.join(", ")}) { return ${jsBody}; } return ${jsIn}; })()`,
        nextIndex: endIdx,
      };
    }
  }

  if (params.length === 0) {
    // Simple value binding - use var for OCaml shadowing compatibility
    const jsBody = transpileExpr(body);
    return { output: `var ${name} = ${jsBody};`, nextIndex: endIdx };
  }

  // Function definition
  const jsBody = transpileBody(body);
  return {
    output: `function ${name}(${params.join(", ")}) { return ${jsBody}; }`,
    nextIndex: endIdx,
  };
}

interface LetBindingResult {
  params: string[];
  body: string;
  hasIn: boolean;
  inExpr?: string;
}

function parseLetBinding(lines: string[], index: number, rest: string): LetBindingResult {
  // rest is everything after "let name" or "let rec name"
  // Parse parameters until we find "="
  // Then the body is everything after "="

  // Collect full text (may be multi-line)
  let fullText = rest;
  let j = index + 1;

  // Check if there's an = on the first line
  if (!containsEquals(rest)) {
    // keep reading lines until we find =
    while (j < lines.length) {
      const t = lines[j].trim();
      if (t === "") { j++; continue; }
      fullText += " " + t;
      j++;
      if (containsEquals(fullText)) break;
    }
  }

  // Now continue collecting if the body spans multiple lines
  // We look at indentation: continuation lines are indented more
  const baseIndent = lines[index].length - lines[index].trimStart().length;
  while (j < lines.length) {
    const line = lines[j];
    if (line.trim() === "") { j++; continue; }
    const indent = line.length - line.trimStart().length;
    if (indent <= baseIndent) break;
    fullText += " " + line.trim();
    j++;
  }

  // Store the actual end index
  const actualEndIdx = j;

  // Parse params and body from fullText
  const eqIdx = findTopLevelEquals(fullText);
  if (eqIdx === -1) {
    return { params: [], body: fullText.trim(), hasIn: false };
  }

  const paramsPart = fullText.slice(0, eqIdx).trim();
  let bodyPart = fullText.slice(eqIdx + 1).trim();

  // Parse parameters (space-separated identifiers, skip type annotations)
  const params = parseOCamlParams(paramsPart);

  // Check for "in" keyword at top level
  const inIdx = findTopLevelIn(bodyPart);
  if (inIdx !== -1) {
    const body = bodyPart.slice(0, inIdx).trim();
    const inExpr = bodyPart.slice(inIdx + 2).trim();
    return { params, body, hasIn: true, inExpr };
  }

  // Store actualEndIdx on the lines array for findEndIndex to use
  (lines as unknown as Record<string, number>).__lastParsedEnd = actualEndIdx;

  return { params, body: bodyPart, hasIn: false };
}

function containsEquals(s: string): boolean {
  // Find = that is not part of ==, !=, <=, >=, =>
  let depth = 0;
  for (let i = 0; i < s.length; i++) {
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    else if (s[i] === '"') {
      i++;
      while (i < s.length && s[i] !== '"') { if (s[i] === '\\') i++; i++; }
    } else if (depth === 0 && s[i] === "=" && s[i - 1] !== "<" && s[i - 1] !== ">" && s[i - 1] !== "!" && s[i + 1] !== "=") {
      // Check it's not part of ->
      if (s[i - 1] !== "-" && s[i + 1] !== ">") return true;
    }
  }
  return false;
}

function findTopLevelEquals(s: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length; i++) {
    if (inStr) {
      if (s[i] === '\\') { i++; continue; }
      if (s[i] === '"') inStr = false;
      continue;
    }
    if (s[i] === '"') { inStr = true; continue; }
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    else if (depth === 0 && s[i] === "=") {
      // Not ==, !=, <=, >=, =>
      const prev = i > 0 ? s[i - 1] : " ";
      const next = i + 1 < s.length ? s[i + 1] : " ";
      if (prev !== "<" && prev !== ">" && prev !== "!" && prev !== "=" && next !== "=" && next !== ">") {
        return i;
      }
    }
  }
  return -1;
}

function findTopLevelIn(s: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length; i++) {
    if (inStr) {
      if (s[i] === '\\') { i++; continue; }
      if (s[i] === '"') inStr = false;
      continue;
    }
    if (s[i] === '"') { inStr = true; continue; }
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    else if (depth === 0 && s.slice(i, i + 2) === "in") {
      const before = i === 0 ? " " : s[i - 1];
      const after = i + 2 >= s.length ? " " : s[i + 2];
      if (/\s/.test(before) && /\s/.test(after)) {
        // Make sure it's not part of "print_int", "begin", "string", etc.
        // by checking previous non-whitespace char
        return i;
      }
    }
  }
  return -1;
}

function parseOCamlParams(paramsPart: string): string[] {
  const params: string[] = [];
  // Tokenize, skip type annotations
  const tokens = tokenizeOCaml(paramsPart);
  let i = 0;
  while (i < tokens.length) {
    const tok = tokens[i];
    if (tok === "(") {
      // Skip parenthesized type annotations like (x : int)
      let inner: string[] = [];
      i++; // skip (
      let depth = 1;
      while (i < tokens.length && depth > 0) {
        if (tokens[i] === "(") depth++;
        else if (tokens[i] === ")") { depth--; if (depth === 0) break; }
        inner.push(tokens[i]);
        i++;
      }
      i++; // skip )
      // Extract the name (first token before :)
      const colonIdx = inner.indexOf(":");
      if (colonIdx > 0) {
        params.push(inner[0]);
      } else if (inner.length === 1) {
        params.push(inner[0]);
      }
    } else if (tok === ":" || tok === "->") {
      // Skip type annotation: consume until next param or end
      i++;
      while (i < tokens.length && tokens[i] !== "(" && !/^[a-z_]\w*$/i.test(tokens[i])) {
        i++;
      }
    } else if (/^[a-z_]\w*$/.test(tok)) {
      params.push(tok);
      i++;
    } else {
      i++;
    }
  }
  return params;
}

function tokenizeOCaml(s: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < s.length) {
    while (i < s.length && /\s/.test(s[i])) i++;
    if (i >= s.length) break;
    if (s[i] === "(" || s[i] === ")" || s[i] === "[" || s[i] === "]") {
      tokens.push(s[i++]);
    } else if (s[i] === '"') {
      let tok = '"';
      i++;
      while (i < s.length && s[i] !== '"') {
        if (s[i] === '\\') { tok += s[i++]; }
        tok += s[i++];
      }
      tok += '"';
      i++;
      tokens.push(tok);
    } else if (s[i] === ':' && s[i + 1] === ':') {
      tokens.push("::");
      i += 2;
    } else if (s[i] === '-' && s[i + 1] === '>') {
      tokens.push("->");
      i += 2;
    } else if (/[=:;,|@<>!+\-*/^.]/.test(s[i])) {
      let tok = "";
      while (i < s.length && /[=:;,|@<>!+\-*/^.]/.test(s[i])) { tok += s[i++]; }
      tokens.push(tok);
    } else {
      let tok = "";
      while (i < s.length && !/[\s()\[\]"=:;,|@<>!+\-*/^.]/.test(s[i])) { tok += s[i++]; }
      if (tok) tokens.push(tok);
    }
  }
  return tokens;
}

function findEndIndex(lines: string[], startIndex: number, _body: string): number {
  // Use the cached end index if available
  const cached = (lines as unknown as Record<string, number>).__lastParsedEnd;
  if (cached !== undefined) {
    delete (lines as unknown as Record<string, number>).__lastParsedEnd;
    return cached;
  }

  // Fallback: skip lines that are continuations (indented more than start)
  const baseIndent = lines[startIndex].length - lines[startIndex].trimStart().length;
  let j = startIndex + 1;
  while (j < lines.length) {
    const line = lines[j];
    if (line.trim() === "") { j++; continue; }
    const indent = line.length - line.trimStart().length;
    if (indent <= baseIndent) break;
    j++;
  }
  return j;
}

// ---------------------------------------------------------------------------
// Transpile an OCaml expression to JavaScript
// ---------------------------------------------------------------------------

function transpileExpr(expr: string): string {
  expr = expr.trim();
  if (!expr) return "undefined";

  // begin ... end -> ( ... )
  expr = transformBeginEnd(expr);

  // ref creation: ref expr
  if (/^ref\s+/.test(expr)) {
    return `__ref(${transpileExpr(expr.slice(4))})`;
  }

  // Dereference: !varname
  if (/^!\w+$/.test(expr)) {
    return `__deref(${expr.slice(1)})`;
  }

  // if ... then ... else ...
  const ifMatch = matchIfThenElse(expr);
  if (ifMatch) {
    const cond = transpileExpr(ifMatch.cond);
    const then_ = transpileExpr(ifMatch.then_);
    const else_ = transpileExpr(ifMatch.else_);
    return `(${cond} ? ${then_} : ${else_})`;
  }

  // if ... then ... (no else, returns unit)
  const ifNoElseMatch = matchIfThen(expr);
  if (ifNoElseMatch) {
    const cond = transpileExpr(ifNoElseMatch.cond);
    const then_ = transpileExpr(ifNoElseMatch.then_);
    return `(${cond} ? ${then_} : undefined)`;
  }

  // match ... with | pat -> expr | ...
  const matchResult = matchExpression(expr);
  if (matchResult) {
    return transpileMatch(matchResult);
  }

  // fun x y -> expr (lambda)
  const funMatch = expr.match(/^fun\s+([\w\s]+)\s*->\s*([\s\S]+)$/);
  if (funMatch) {
    const params = funMatch[1].trim().split(/\s+/);
    const body = transpileExpr(funMatch[2]);
    return `((${params.join(", ")}) => ${body})`;
  }

  // function | pat -> expr (single-arg match lambda)
  if (/^function\s*\|/.test(expr)) {
    const cases = expr.replace(/^function\s*/, "");
    const matchResult2 = parseMatchCases(cases);
    if (matchResult2.length > 0) {
      return `((__match_arg) => ${buildMatchChain("__match_arg", matchResult2)})`;
    }
  }

  // let ... in ... (expression-level)
  const letInMatch = matchLetIn(expr);
  if (letInMatch) {
    return transpileLetIn(letInMatch);
  }

  // Sequencing with ;
  if (containsTopLevelSemicolon(expr)) {
    const parts = splitTopLevel(expr, ";");
    const stmts = parts.map((p) => p.trim()).filter(Boolean);
    if (stmts.length > 1) {
      const jsStmts = stmts.slice(0, -1).map((s) => transpileExpr(s) + ";");
      const last = transpileExpr(stmts[stmts.length - 1]);
      return `(function() { ${jsStmts.join(" ")} return ${last}; })()`;
    }
  }

  // Assignment: varname := expr
  const assignMatch = expr.match(/^(\w+)\s*:=\s*([\s\S]+)$/);
  if (assignMatch) {
    return `__assign(${assignMatch[1]}, ${transpileExpr(assignMatch[2])})`;
  }

  // Boolean operators
  if (containsTopLevelOp(expr, "||")) {
    const parts = splitTopLevel(expr, "||");
    return parts.map(transpileExpr).join(" || ");
  }
  if (containsTopLevelOp(expr, "&&")) {
    const parts = splitTopLevel(expr, "&&");
    return parts.map(transpileExpr).join(" && ");
  }

  // Comparison operators
  for (const op of ["<>", ">=", "<=", "=", "<", ">"]) {
    if (containsTopLevelOp(expr, op)) {
      const parts = splitTopLevel(expr, op);
      if (parts.length === 2) {
        const jsOp = op === "=" ? "===" : op === "<>" ? "!==" : op;
        return `(${transpileExpr(parts[0])} ${jsOp} ${transpileExpr(parts[1])})`;
      }
    }
  }

  // String concatenation with ^
  if (containsTopLevelOp(expr, "^")) {
    const parts = splitTopLevel(expr, "^");
    return parts.map(transpileExpr).join(" + ");
  }

  // List append with @
  if (containsTopLevelOp(expr, "@")) {
    const parts = splitTopLevel(expr, "@");
    if (parts.length === 2) {
      return `[...${transpileExpr(parts[0])}, ...${transpileExpr(parts[1])}]`;
    }
  }

  // Cons operator ::
  if (containsTopLevelOp(expr, "::")) {
    const parts = splitTopLevel(expr, "::");
    if (parts.length === 2) {
      return `[${transpileExpr(parts[0])}, ...${transpileExpr(parts[1])}]`;
    }
  }

  // Arithmetic: +. -. *. /. (float ops) are same as regular in JS
  // Addition and subtraction
  {
    const addParts = splitTopLevelAddSub(expr);
    if (addParts) return addParts;
  }

  // Multiplication * and /
  if (containsTopLevelOp(expr, "*") || containsTopLevelOp(expr, "*.")) {
    const op = containsTopLevelOp(expr, "*.") ? "*." : "*";
    const parts = splitTopLevel(expr, op);
    if (parts.length >= 2) {
      return `(${parts.map(transpileExpr).join(" * ")})`;
    }
  }
  if (containsTopLevelOp(expr, "/") || containsTopLevelOp(expr, "/.")) {
    const op = containsTopLevelOp(expr, "/.") ? "/." : "/";
    const parts = splitTopLevel(expr, op);
    if (parts.length >= 2) {
      return `(${parts.map(transpileExpr).join(" / ")})`;
    }
  }

  // Modulo
  if (containsTopLevelOp(expr, "mod")) {
    const parts = splitTopLevelKeyword(expr, "mod");
    if (parts.length === 2) {
      return `(${transpileExpr(parts[0])} % ${transpileExpr(parts[1])})`;
    }
  }

  // Pipe operator |>
  if (containsTopLevelOp(expr, "|>")) {
    const parts = splitTopLevel(expr, "|>");
    if (parts.length >= 2) {
      let result = transpileExpr(parts[0]);
      for (let k = 1; k < parts.length; k++) {
        const fn = transpileExpr(parts[k]);
        result = `${fn}(${result})`;
      }
      return result;
    }
  }

  // Negation: - expr (unary)
  if (/^-\s*\d/.test(expr) && !expr.includes(" ")) {
    return expr; // negative number literal
  }

  // List literal: [1; 2; 3]
  if (expr.startsWith("[") && expr.endsWith("]")) {
    const inner = expr.slice(1, -1).trim();
    if (inner === "") return "[]";
    const items = inner.split(";").map((s) => s.trim()).filter(Boolean);
    return `[${items.map(transpileExpr).join(", ")}]`;
  }

  // Tuple: (a, b, c)
  if (expr.startsWith("(") && expr.endsWith(")")) {
    const inner = expr.slice(1, -1).trim();
    const items = splitCommas(inner);
    if (items.length >= 2) {
      return `[${items.map(transpileExpr).join(", ")}]`;
    }
    // Parenthesized expression
    return `(${transpileExpr(inner)})`;
  }

  // String literal
  if (expr.startsWith('"')) return expr;

  // Char literal
  if (expr.startsWith("'") && expr.endsWith("'")) return expr;

  // Number literals
  if (/^-?\d+(\.\d+)?$/.test(expr)) return expr;

  // Boolean
  if (expr === "true") return "true";
  if (expr === "false") return "false";

  // Unit
  if (expr === "()") return "undefined";

  // Printf.printf
  if (expr.startsWith("Printf.printf ") || expr.startsWith("Printf.printf(")) {
    const args = extractFuncArgs(expr.slice(14));
    return `Printf.printf(${args.map(transpileExpr).join(", ")})`;
  }

  // String module calls -> String_
  if (expr.startsWith("String.")) {
    const afterDot = expr.slice(7);
    const spaceIdx = afterDot.search(/\s/);
    if (spaceIdx === -1) {
      return `String_.${afterDot}`;
    }
    const method = afterDot.slice(0, spaceIdx);
    const argsStr = afterDot.slice(spaceIdx + 1);
    const args = extractFuncArgs(argsStr);
    return `String_.${method}(${args.map(transpileExpr).join(", ")})`;
  }

  // List module calls
  if (expr.startsWith("List.")) {
    const afterDot = expr.slice(5);
    const spaceIdx = afterDot.search(/\s/);
    if (spaceIdx === -1) {
      return `List.${afterDot}`;
    }
    const method = afterDot.slice(0, spaceIdx);
    const argsStr = afterDot.slice(spaceIdx + 1);
    const args = extractFuncArgs(argsStr);
    return `List.${method}(${args.map(transpileExpr).join(", ")})`;
  }

  // Array.of_list, Array.to_list etc. (basic)
  if (expr.startsWith("Array.")) {
    return transpileExpr(expr.replace(/^Array\.\w+\s*/, ""));
  }

  // Function application: func arg1 arg2
  const appResult = tryFunctionApp(expr);
  if (appResult) return appResult;

  // Simple identifier
  return expr;
}

function transpileBody(body: string): string {
  return transpileExpr(body);
}

// ---------------------------------------------------------------------------
// if/then/else matching
// ---------------------------------------------------------------------------

interface IfResult { cond: string; then_: string; else_: string; }
interface IfNoElse { cond: string; then_: string; }

function matchIfThenElse(expr: string): IfResult | null {
  if (!/^if\s/.test(expr)) return null;

  const thenIdx = findKeyword(expr, "then");
  if (thenIdx === -1) return null;

  const cond = expr.slice(3, thenIdx).trim();
  const afterThen = expr.slice(thenIdx + 4).trim();

  const elseIdx = findKeyword(afterThen, "else");
  if (elseIdx === -1) return null;

  const then_ = afterThen.slice(0, elseIdx).trim();
  const else_ = afterThen.slice(elseIdx + 4).trim();

  return { cond, then_, else_ };
}

function matchIfThen(expr: string): IfNoElse | null {
  if (!/^if\s/.test(expr)) return null;

  const thenIdx = findKeyword(expr, "then");
  if (thenIdx === -1) return null;

  const cond = expr.slice(3, thenIdx).trim();
  const then_ = expr.slice(thenIdx + 4).trim();

  return { cond, then_ };
}

function findKeyword(expr: string, kw: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i <= expr.length - kw.length; i++) {
    if (inStr) {
      if (expr[i] === '\\') { i++; continue; }
      if (expr[i] === '"') inStr = false;
      continue;
    }
    if (expr[i] === '"') { inStr = true; continue; }
    if (expr[i] === "(") depth++;
    else if (expr[i] === ")") depth--;
    else if (expr[i] === "[") depth++;
    else if (expr[i] === "]") depth--;
    if (depth === 0 && expr.slice(i, i + kw.length) === kw) {
      const before = i === 0 ? " " : expr[i - 1];
      const after = i + kw.length >= expr.length ? " " : expr[i + kw.length];
      if (/[\s(]/.test(before) && /[\s)]/.test(after)) return i;
    }
  }
  return -1;
}

// ---------------------------------------------------------------------------
// match expression parsing
// ---------------------------------------------------------------------------

interface MatchResult { scrutinee: string; cases: MatchCase[]; }
interface MatchCase { pattern: string; guard?: string; body: string; }

function matchExpression(expr: string): MatchResult | null {
  const matchIdx = findKeywordFromStart(expr, "match");
  if (matchIdx !== 0) return null;

  const withIdx = findKeyword(expr, "with");
  if (withIdx === -1) return null;

  const scrutinee = expr.slice(5, withIdx).trim();
  const casesStr = expr.slice(withIdx + 4).trim();

  const cases = parseMatchCases(casesStr);
  if (cases.length === 0) return null;

  return { scrutinee, cases };
}

function findKeywordFromStart(expr: string, kw: string): number {
  if (expr.startsWith(kw) && (expr.length === kw.length || /\s/.test(expr[kw.length]))) return 0;
  return -1;
}

function parseMatchCases(casesStr: string): MatchCase[] {
  const cases: MatchCase[] = [];
  // Split on top-level |
  const parts = splitMatchCases(casesStr);

  for (const part of parts) {
    const trimmed = part.trim();
    if (!trimmed) continue;

    const arrowIdx = findArrow(trimmed);
    if (arrowIdx === -1) continue;

    let pattern = trimmed.slice(0, arrowIdx).trim();
    const body = trimmed.slice(arrowIdx + 2).trim();

    // Check for guard: "pattern when condition"
    let guard: string | undefined;
    const whenIdx = findKeyword(pattern, "when");
    if (whenIdx !== -1) {
      guard = pattern.slice(whenIdx + 4).trim();
      pattern = pattern.slice(0, whenIdx).trim();
    }

    cases.push({ pattern, guard, body });
  }

  return cases;
}

function splitMatchCases(s: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let current = "";
  let inStr = false;

  for (let i = 0; i < s.length; i++) {
    if (inStr) {
      if (s[i] === '\\') { current += s[i] + s[i + 1]; i++; continue; }
      if (s[i] === '"') inStr = false;
      current += s[i];
      continue;
    }
    if (s[i] === '"') { inStr = true; current += s[i]; continue; }
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    else if (s[i] === "[") depth++;
    else if (s[i] === "]") depth--;

    if (depth === 0 && s[i] === "|") {
      parts.push(current);
      current = "";
      continue;
    }
    current += s[i];
  }
  if (current.trim()) parts.push(current);
  return parts;
}

function findArrow(s: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length - 1; i++) {
    if (inStr) {
      if (s[i] === '\\') { i++; continue; }
      if (s[i] === '"') inStr = false;
      continue;
    }
    if (s[i] === '"') { inStr = true; continue; }
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    else if (s[i] === "[") depth++;
    else if (s[i] === "]") depth--;
    if (depth === 0 && s[i] === "-" && s[i + 1] === ">") return i;
  }
  return -1;
}

function transpileMatch(m: MatchResult): string {
  const scrutinee = transpileExpr(m.scrutinee);
  return `((__m) => { ${buildMatchChain("__m", m.cases)} })(${scrutinee})`;
}

function buildMatchChain(varName: string, cases: MatchCase[]): string {
  const branches: string[] = [];

  for (const c of cases) {
    const pat = c.pattern.trim();
    const body = transpileExpr(c.body);

    if (pat === "_") {
      branches.push(`return ${body};`);
      continue;
    }

    // Wildcard variable binding
    if (/^[a-z_]\w*$/.test(pat) && !["true", "false"].includes(pat)) {
      if (c.guard) {
        branches.push(`{ let ${pat} = ${varName}; if (${transpileExpr(c.guard.replace(new RegExp(`\\b${pat}\\b`, "g"), varName))}) return ${body}; }`);
      } else {
        branches.push(`{ let ${pat} = ${varName}; return ${body}; }`);
      }
      continue;
    }

    // Singleton list pattern: _ :: [] or x :: []
    const singletonMatch = pat.match(/^(\w+)\s*::\s*\[\]$/);
    if (singletonMatch) {
      const name = singletonMatch[1];
      if (name === "_") {
        branches.push(`if (${varName}.length === 1) return ${body};`);
      } else {
        branches.push(`if (${varName}.length === 1) { let ${name} = ${varName}[0]; return ${body}; }`);
      }
      continue;
    }

    // Cons pattern: x :: xs
    const consMatch = pat.match(/^(\w+)\s*::\s*(\w+)$/);
    if (consMatch) {
      const [, head, tail] = consMatch;
      const guard = c.guard ? ` && (${transpileExpr(c.guard)})` : "";
      branches.push(`if (${varName}.length > 0${guard}) { let ${head} = ${varName}[0]; let ${tail} = ${varName}.slice(1); return ${body}; }`);
      continue;
    }

    // Empty list pattern: []
    if (pat === "[]") {
      branches.push(`if (${varName}.length === 0) return ${body};`);
      continue;
    }

    // Tuple pattern: (a, b)
    const tupleMatch = pat.match(/^\(([^)]+)\)$/);
    if (tupleMatch) {
      const parts = tupleMatch[1].split(",").map((s) => s.trim());
      const bindings = parts.map((p, idx) => `let ${p} = ${varName}[${idx}];`).join(" ");
      const guard = c.guard ? `if (${transpileExpr(c.guard)})` : "";
      branches.push(`{ ${bindings} ${guard} return ${body}; }`);
      continue;
    }

    // Constructor pattern: Some x, None
    if (pat === "None") {
      branches.push(`if (${varName} === null || ${varName} === undefined) return ${body};`);
      continue;
    }
    const someMatch = pat.match(/^Some\s+(\w+)$/);
    if (someMatch) {
      const inner = someMatch[1];
      branches.push(`if (${varName} !== null && ${varName} !== undefined) { let ${inner} = ${varName}; return ${body}; }`);
      continue;
    }

    // Literal patterns (numbers, strings, booleans)
    if (/^-?\d+$/.test(pat) || /^".*"$/.test(pat)) {
      const guard = c.guard ? ` && (${transpileExpr(c.guard)})` : "";
      branches.push(`if (${varName} === ${pat}${guard}) return ${body};`);
      continue;
    }
    if (pat === "true") { branches.push(`if (${varName} === true) return ${body};`); continue; }
    if (pat === "false") { branches.push(`if (${varName} === false) return ${body};`); continue; }

    // OR pattern: pat1 | pat2 (already split by |, so this handles "0" etc.)
    // Default: treat as literal comparison
    const guard = c.guard ? ` && (${transpileExpr(c.guard)})` : "";
    branches.push(`if (${varName} === ${transpileExpr(pat)}${guard}) return ${body};`);
  }

  return branches.join(" ");
}

// ---------------------------------------------------------------------------
// let ... in ... at expression level
// ---------------------------------------------------------------------------

interface LetInExpr {
  name: string;
  isRec: boolean;
  params: string[];
  value: string;
  inExpr: string;
}

function matchLetIn(expr: string): LetInExpr | null {
  if (!/^let\s/.test(expr)) return null;

  const isRec = /^let\s+rec\s+/.test(expr);
  const afterLet = isRec ? expr.slice(8) : expr.slice(4);

  // Find =
  const eqIdx = findTopLevelEquals(afterLet);
  if (eqIdx === -1) return null;

  const beforeEq = afterLet.slice(0, eqIdx).trim();
  const afterEq = afterLet.slice(eqIdx + 1).trim();

  // Find "in" at top level in afterEq
  const inIdx = findTopLevelIn(afterEq);
  if (inIdx === -1) return null;

  const value = afterEq.slice(0, inIdx).trim();
  const inExpr = afterEq.slice(inIdx + 2).trim();

  const tokens = beforeEq.split(/\s+/);
  const name = tokens[0];
  const params = tokens.slice(1).filter((t) => t !== ":" && !/^[A-Z]/.test(t));

  return { name, isRec, params, value, inExpr };
}

function transpileLetIn(li: LetInExpr): string {
  if (li.params.length === 0) {
    return `(function() { let ${li.name} = ${transpileExpr(li.value)}; return ${transpileExpr(li.inExpr)}; })()`;
  }
  const fn = li.isRec ? "function" : "function";
  return `(function() { ${fn} ${li.name}(${li.params.join(", ")}) { return ${transpileExpr(li.value)}; } return ${transpileExpr(li.inExpr)}; })()`;
}

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

function containsTopLevelOp(expr: string, op: string): boolean {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < expr.length; i++) {
    if (inStr) {
      if (expr[i] === '\\') { i++; continue; }
      if (expr[i] === '"') inStr = false;
      continue;
    }
    if (expr[i] === '"') { inStr = true; continue; }
    if (expr[i] === "(" || expr[i] === "[") depth++;
    else if (expr[i] === ")" || expr[i] === "]") depth--;
    if (depth === 0 && expr.slice(i, i + op.length) === op) {
      // For alphabetic operators, check word boundaries
      if (/\w/.test(op[0])) {
        const before = i > 0 ? expr[i - 1] : " ";
        const after = i + op.length < expr.length ? expr[i + op.length] : " ";
        if (!/\w/.test(before) && !/\w/.test(after)) return true;
      } else {
        // For symbolic operators, ensure we match exact operator
        // e.g., don't match ">=" when looking for ">"
        if (op === ">" && i + 1 < expr.length && expr[i + 1] === "=") continue;
        if (op === "<" && i + 1 < expr.length && (expr[i + 1] === ">" || expr[i + 1] === "=")) continue;
        if (op === "=" && i > 0 && (expr[i - 1] === "<" || expr[i - 1] === ">" || expr[i - 1] === "!")) continue;
        if (op === "=" && i + 1 < expr.length && expr[i + 1] === "=") continue;
        if (op === "|" && expr[i + 1] === ">") continue;
        if (op === "|" && expr[i + 1] === "|") continue;
        if (op === "*" && expr[i + 1] === ".") continue;
        if (op === "/" && expr[i + 1] === ".") continue;
        return true;
      }
    }
  }
  return false;
}

function splitTopLevel(expr: string, sep: string): string[] {
  const parts: string[] = [];
  let depth = 0;
  let inStr = false;
  let current = "";

  for (let i = 0; i < expr.length; i++) {
    if (inStr) {
      if (expr[i] === '\\') { current += expr[i] + expr[i + 1]; i++; continue; }
      if (expr[i] === '"') inStr = false;
      current += expr[i];
      continue;
    }
    if (expr[i] === '"') { inStr = true; current += expr[i]; continue; }
    if (expr[i] === "(" || expr[i] === "[") { depth++; current += expr[i]; continue; }
    if (expr[i] === ")" || expr[i] === "]") { depth--; current += expr[i]; continue; }

    if (depth === 0 && expr.slice(i, i + sep.length) === sep) {
      if (/\w/.test(sep[0])) {
        const before = i > 0 ? expr[i - 1] : " ";
        const after = i + sep.length < expr.length ? expr[i + sep.length] : " ";
        if (!/\w/.test(before) && !/\w/.test(after)) {
          parts.push(current);
          current = "";
          i += sep.length - 1;
          continue;
        }
      } else {
        // Check it's the exact operator
        if (sep === ">" && i + 1 < expr.length && expr[i + 1] === "=") { current += expr[i]; continue; }
        if (sep === "<" && i + 1 < expr.length && (expr[i + 1] === ">" || expr[i + 1] === "=")) { current += expr[i]; continue; }
        if (sep === "=" && i > 0 && (expr[i - 1] === "<" || expr[i - 1] === ">" || expr[i - 1] === "!")) { current += expr[i]; continue; }
        if (sep === "=" && i + 1 < expr.length && expr[i + 1] === "=") { current += expr[i]; continue; }
        if (sep === "*" && expr[i + 1] === ".") { current += expr[i]; continue; }
        if (sep === "/" && expr[i + 1] === ".") { current += expr[i]; continue; }
        parts.push(current);
        current = "";
        i += sep.length - 1;
        continue;
      }
    }
    current += expr[i];
  }
  if (current) parts.push(current);
  return parts;
}

function splitTopLevelKeyword(expr: string, kw: string): string[] {
  const idx = findKeyword(expr, kw);
  if (idx === -1) return [expr];
  return [expr.slice(0, idx).trim(), expr.slice(idx + kw.length).trim()];
}

function splitTopLevelAddSub(expr: string): string | null {
  // Find rightmost + or - at top level (for left associativity)
  // but not inside parens/strings, and not unary minus
  let depth = 0;
  let inStr = false;
  let lastPlusIdx = -1;
  let lastMinusIdx = -1;
  let lastOp = "";

  for (let i = expr.length - 1; i >= 0; i--) {
    const ch = expr[i];
    // Quick backwards string detection (simplified)
    if (ch === '"' && !inStr) { inStr = true; continue; }
    if (ch === '"' && inStr) { inStr = false; continue; }
    if (inStr) continue;
    if (ch === ")" || ch === "]") depth++;
    else if (ch === "(" || ch === "[") depth--;

    if (depth === 0) {
      if ((ch === "+" || ch === "-") && i > 0) {
        const prev = expr[i - 1];
        const prevTrimmed = expr.slice(0, i).trimEnd();
        // Ensure it's not unary
        if (prevTrimmed.length > 0 && !/[=(<>+\-*/,;|]$/.test(prevTrimmed)) {
          // Ensure it's not +. or -.
          const next = i + 1 < expr.length ? expr[i + 1] : "";
          if (next === ".") continue; // float op, handle separately
          if (ch === "+" && lastPlusIdx === -1) { lastPlusIdx = i; lastOp = "+"; break; }
          if (ch === "-" && lastMinusIdx === -1) { lastMinusIdx = i; lastOp = "-"; break; }
        }
      }
    }
  }

  const idx = lastOp === "+" ? lastPlusIdx : lastMinusIdx;
  if (idx === -1) return null;

  const left = expr.slice(0, idx).trim();
  const right = expr.slice(idx + 1).trim();
  if (!left || !right) return null;

  return `(${transpileExpr(left)} ${lastOp} ${transpileExpr(right)})`;
}

function containsTopLevelSemicolon(expr: string): boolean {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < expr.length; i++) {
    if (inStr) {
      if (expr[i] === '\\') { i++; continue; }
      if (expr[i] === '"') inStr = false;
      continue;
    }
    if (expr[i] === '"') { inStr = true; continue; }
    if (expr[i] === "(" || expr[i] === "[") depth++;
    else if (expr[i] === ")" || expr[i] === "]") depth--;
    if (depth === 0 && expr[i] === ";") return true;
  }
  return false;
}

function splitCommas(s: string): string[] {
  const items: string[] = [];
  let depth = 0;
  let current = "";
  let inStr = false;
  for (let i = 0; i < s.length; i++) {
    if (inStr) {
      if (s[i] === '\\') { current += s[i] + s[i + 1]; i++; continue; }
      if (s[i] === '"') inStr = false;
      current += s[i];
      continue;
    }
    if (s[i] === '"') { inStr = true; current += s[i]; continue; }
    if (s[i] === "(" || s[i] === "[") { depth++; current += s[i]; continue; }
    if (s[i] === ")" || s[i] === "]") { depth--; current += s[i]; continue; }
    if (s[i] === "," && depth === 0) {
      items.push(current.trim());
      current = "";
      continue;
    }
    current += s[i];
  }
  if (current.trim()) items.push(current.trim());
  return items;
}

function transformBeginEnd(expr: string): string {
  // Replace begin ... end with ( ... )
  let result = expr;
  while (true) {
    const idx = findKeyword(result, "begin");
    if (idx === -1) break;
    const endIdx = findKeyword(result.slice(idx + 5), "end");
    if (endIdx === -1) break;
    const inner = result.slice(idx + 5, idx + 5 + endIdx).trim();
    result = result.slice(0, idx) + "(" + inner + ")" + result.slice(idx + 5 + endIdx + 3);
  }
  return result;
}

// ---------------------------------------------------------------------------
// Function application
// ---------------------------------------------------------------------------

function tryFunctionApp(expr: string): string | null {
  const tokens = extractFuncArgs(expr);
  if (tokens.length < 2) return null;

  const fn = tokens[0];
  const args = tokens.slice(1);

  // Check if it's a known function/identifier
  if (/^[a-z_]\w*$/.test(fn) || fn === "Some") {
    // Some(x) -> x (Option type)
    if (fn === "Some") {
      return transpileExpr(args[0]);
    }
    const jsArgs = args.map(transpileExpr);
    return `${fn}(${jsArgs.join(", ")})`;
  }

  // Not a function application
  return null;
}

function extractFuncArgs(expr: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  const s = expr.trim();

  while (i < s.length) {
    while (i < s.length && /\s/.test(s[i])) i++;
    if (i >= s.length) break;

    const ch = s[i];
    if (ch === "(") {
      const start = i;
      let depth = 1;
      i++;
      while (i < s.length && depth > 0) {
        if (s[i] === "(") depth++;
        else if (s[i] === ")") depth--;
        i++;
      }
      tokens.push(s.slice(start, i));
    } else if (ch === "[") {
      const start = i;
      let depth = 1;
      i++;
      while (i < s.length && depth > 0) {
        if (s[i] === "[") depth++;
        else if (s[i] === "]") depth--;
        i++;
      }
      tokens.push(s.slice(start, i));
    } else if (ch === '"') {
      const start = i;
      i++;
      while (i < s.length && s[i] !== '"') {
        if (s[i] === '\\') i++;
        i++;
      }
      i++;
      tokens.push(s.slice(start, i));
    } else {
      const start = i;
      while (i < s.length && !/[\s()\[\]"]/.test(s[i])) i++;
      tokens.push(s.slice(start, i));
    }
  }

  return tokens;
}

// ---------------------------------------------------------------------------
// Declaration extraction (for test code injection)
// ---------------------------------------------------------------------------

export function extractOCamlDeclarations(code: string): string {
  // Return non-side-effect top-level definitions
  const lines = code.split("\n");
  const result: string[] = [];
  let inDef = false;
  let baseIndent = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();

    if (!inDef) {
      if (/^let\s+(rec\s+)?\w+/.test(trimmed) && !isSideEffect(trimmed)) {
        inDef = true;
        baseIndent = line.length - line.trimStart().length;
        result.push(line);
      }
      // skip side-effect lines
    } else {
      const indent = line.length - line.trimStart().length;
      if (trimmed === "" || indent > baseIndent) {
        result.push(line);
      } else {
        inDef = false;
        // Re-check this line
        if (/^let\s+(rec\s+)?\w+/.test(trimmed) && !isSideEffect(trimmed)) {
          inDef = true;
          baseIndent = line.length - line.trimStart().length;
          result.push(line);
        }
      }
    }
  }

  return result.join("\n").trim();
}

function isSideEffect(line: string): boolean {
  return /^(print_|Printf\.)/.test(line) ||
    /^let\s+\(\)\s*=/.test(line) ||
    /^ignore\s/.test(line);
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export async function runOCaml(code: string): Promise<RunResult> {
  try {
    const js = transpileOCaml(code);
    // eslint-disable-next-line no-new-func
    const fn = new Function(js);
    const output = fn();
    return { stdout: output ?? "", stderr: "", error: "" };
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
      const decls = extractOCamlDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }

    const result = await runOCaml(codeToRun);
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
