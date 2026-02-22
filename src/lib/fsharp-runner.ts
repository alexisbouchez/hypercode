import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let fsharpReady = false;
export function isFSharpReady(): boolean { return fsharpReady; }
export async function initFSharpRunner(): Promise<void> { fsharpReady = true; }

// ---------------------------------------------------------------------------
// F# → JavaScript transpiler
// Handles the subset of F# used in the beginner course lessons.
// All user functions are fully curried to match F# semantics.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function println(s) {
  if (s === undefined || s === null) { __output.push('\\n'); return; }
  __output.push(String(s) + '\\n');
}
function print(s) {
  if (s === undefined || s === null) return;
  __output.push(String(s));
}
function fst(t) { return t[0]; }
function snd(t) { return t[1]; }
const List = {
  map: (f) => (lst) => lst.map(f),
  filter: (f) => (lst) => lst.filter(f),
  fold: (f) => (init) => (lst) => lst.reduce((acc, x) => f(acc, x), init),
  length: (lst) => lst.length,
  head: (lst) => lst[0],
  tail: (lst) => lst.slice(1),
  sum: (lst) => lst.reduce((a, b) => a + b, 0),
  isEmpty: (lst) => lst.length === 0,
  rev: (lst) => [...lst].reverse(),
  append: (xs) => (ys) => [...xs, ...ys],
  item: (i) => (lst) => lst[i],
  iter: (f) => (lst) => { lst.forEach(f); },
  exists: (f) => (lst) => lst.some(f),
  forall: (f) => (lst) => lst.every(f),
  max: (lst) => Math.max(...lst),
  min: (lst) => Math.min(...lst),
  sort: (lst) => [...lst].sort((a, b) => a - b),
  average: (lst) => lst.reduce((a, b) => a + b, 0) / lst.length,
  collect: (f) => (lst) => lst.flatMap(f),
  ofList: (lst) => lst,
  toList: (lst) => lst,
};
`;

// ---------------------------------------------------------------------------
// Utility: get indent of a line
// ---------------------------------------------------------------------------
function getIndent(line: string): number {
  return line.length - line.trimStart().length;
}

// ---------------------------------------------------------------------------
// PRELUDE functions that have known arities (used like List.* but simpler)
// ---------------------------------------------------------------------------
const PRELUDE_ARITIES: Record<string, number> = {
  "fst": 1, "snd": 1,
};

// ---------------------------------------------------------------------------
// Tokenize an F# expression into whitespace-separated tokens,
// respecting parens, brackets, and strings.
// ---------------------------------------------------------------------------
function tokenizeArgs(expr: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < expr.length) {
    while (i < expr.length && /\s/.test(expr[i])) i++;
    if (i >= expr.length) break;
    const start = i;
    const ch = expr[i];
    if (ch === "(" || ch === "[") {
      const close = ch === "(" ? ")" : "]";
      let depth = 1; i++;
      while (i < expr.length && depth > 0) {
        if (expr[i] === ch) depth++;
        else if (expr[i] === close) depth--;
        i++;
      }
    } else if (ch === '"') {
      i++;
      while (i < expr.length && expr[i] !== '"') { if (expr[i] === '\\') i++; i++; }
      i++;
    } else if (ch === '`') {
      i++;
      while (i < expr.length && expr[i] !== '`') { if (expr[i] === '\\') i++; i++; }
      i++;
    } else {
      while (i < expr.length && !/\s/.test(expr[i])) i++;
    }
    if (i > start) tokens.push(expr.slice(start, i));
  }
  return tokens;
}

// Find index of a substring outside parens/brackets/strings.
// Returns -1 if not found.
function findOutside(s: string, needle: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i <= s.length - needle.length; i++) {
    if (!inStr && s[i] === '"') { inStr = true; continue; }
    if (inStr) { if (s[i] === '\\') i++; else if (s[i] === '"') inStr = false; continue; }
    if (s[i] === '(' || s[i] === '[') { depth++; continue; }
    if (s[i] === ')' || s[i] === ']') { depth--; continue; }
    if (depth === 0 && s.slice(i, i + needle.length) === needle) return i;
  }
  return -1;
}

// Find index of keyword (whole-word, outside parens/strings)
function findKeyword(s: string, kw: string): number {
  const re = new RegExp(`\\b${kw}\\b`);
  let depth = 0, inStr = false;
  for (let i = 0; i <= s.length - kw.length; i++) {
    if (!inStr && s[i] === '"') { inStr = true; continue; }
    if (inStr) { if (s[i] === '\\') i++; else if (s[i] === '"') inStr = false; continue; }
    if (s[i] === '(' || s[i] === '[') { depth++; continue; }
    if (s[i] === ')' || s[i] === ']') { depth--; continue; }
    if (depth === 0 && re.test(s.slice(i, i + kw.length + 2))) {
      const m = s.slice(i).match(new RegExp(`^${kw}\\b`));
      if (m) return i;
    }
  }
  return -1;
}

// ---------------------------------------------------------------------------
// Phase 0: collect function arities and DU constructor info
// ---------------------------------------------------------------------------
const LIST_ARITIES: Record<string, number> = {
  "List.map": 2, "List.filter": 2, "List.fold": 3, "List.length": 1,
  "List.head": 1, "List.tail": 1, "List.sum": 1, "List.isEmpty": 1,
  "List.rev": 1, "List.append": 2, "List.item": 2, "List.iter": 2,
  "List.exists": 2, "List.forall": 2, "List.max": 1, "List.min": 1,
  "List.sort": 1, "List.average": 1, "List.collect": 2, "List.ofList": 1,
  "List.toList": 1,
};

function collectFuncArities(lines: string[]): Map<string, number> {
  const arities = new Map<string, number>();
  for (const line of lines) {
    const t = line.trim();
    // let [rec] name params... = body
    const m = t.match(/^let\s+(?:rec\s+)?(\w+)\s+((?:\(?\w[\w\s,:*]*\)?\s+)+)=/);
    if (!m) continue;
    const paramStr = m[2].trim();
    if (!paramStr) continue;
    // Count top-level param tokens
    const toks = tokenizeArgs(paramStr);
    if (toks.length > 0) arities.set(m[1], toks.length);
  }
  return arities;
}

interface DUType {
  cases: Map<string, number>; // caseName → tuple arity (0=single value, 1=tuple, -1=no value)
}

function collectDUTypes(lines: string[]): { duTypes: Map<string, DUType>; duConstructors: Set<string> } {
  const duTypes = new Map<string, DUType>();
  const duConstructors = new Set<string>();
  let inDU: string | null = null;
  let cases: Map<string, number> = new Map();

  for (const line of lines) {
    const t = line.trim();
    // type Expr = OR type Expr (multi-line DU starts)
    const typeM = t.match(/^type\s+(\w+)\s*=/);
    if (typeM) {
      if (inDU) { duTypes.set(inDU, { cases }); }
      inDU = typeM[1];
      cases = new Map();
      // Check for inline cases: type T = | A | B | C
      const inlineCases = t.slice(typeM[0].length).trim();
      if (inlineCases.startsWith('|') || inlineCases.match(/^\w/)) {
        // Record type or single-line - not a DU
        // Record: { ... } — skip
        if (inlineCases.includes('{')) { inDU = null; cases = new Map(); continue; }
        // Parse inline DU cases
        const casesParts = inlineCases.split('|').map(p => p.trim()).filter(Boolean);
        for (const cp of casesParts) parseDUCase(cp, cases, duConstructors);
        if (casesParts.length > 0) { duTypes.set(inDU, { cases }); inDU = null; }
      }
      continue;
    }
    // Continuation of DU definition
    if (inDU && t.startsWith('|')) {
      const cp = t.slice(1).trim();
      parseDUCase(cp, cases, duConstructors);
      continue;
    }
    // End of DU
    if (inDU && t && !t.startsWith('|') && !t.startsWith('//')) {
      duTypes.set(inDU, { cases });
      inDU = null; cases = new Map();
    }
  }
  if (inDU) duTypes.set(inDU, { cases });
  return { duTypes, duConstructors };
}

function parseDUCase(cp: string, cases: Map<string, number>, duConstructors: Set<string>) {
  // "Num of int" → name=Num, arity=0 (single value)
  // "Add of int * int" → name=Add, arity=1 (tuple)
  // "NoValue" → name=NoValue, arity=-1 (no value)
  const m = cp.match(/^(\w+)(?:\s+of\s+(.+))?$/);
  if (!m) return;
  const name = m[1];
  if (!m[2]) { cases.set(name, -1); }
  else if (m[2].includes('*')) { cases.set(name, 1); } // tuple
  else { cases.set(name, 0); } // single value
  duConstructors.add(name);
}

// ---------------------------------------------------------------------------
// Phase 1: Join continuation lines
// A "let ... =" ending with "=" and body on next lines
// Also joins lines starting with "|>" to previous line
// ---------------------------------------------------------------------------
function joinContinuations(lines: string[]): string[] {
  const result: string[] = [];
  let i = 0;
  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trim();
    if (trimmed === '') { result.push(''); i++; continue; }

    const indent = getIndent(line);
    // let binding with body on next line (ends with `=`)
    const isLetWithCont = /^\s*(?:let|let\s+rec)\s+[\w(].*=\s*$/.test(line);
    if (isLetWithCont) {
      let merged = line.trimEnd();
      i++;
      while (i < lines.length) {
        const nextLine = lines[i];
        const nextTrimmed = nextLine.trim();
        if (nextTrimmed === '') { i++; break; }
        const nextIndent = getIndent(nextLine);
        if (nextIndent <= indent) break;
        merged += ' ' + nextTrimmed;
        i++;
      }
      result.push(merged);
      continue;
    }
    result.push(line);
    i++;
  }
  return result;
}

// ---------------------------------------------------------------------------
// String interpolation: $"...{expr}..." → `...${expr}...`
// ---------------------------------------------------------------------------
function transformStringInterp(line: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  let result = '';
  let i = 0;
  while (i < line.length) {
    if (line[i] === '$' && line[i + 1] === '"') {
      i += 2;
      result += '`';
      while (i < line.length && line[i] !== '"') {
        if (line[i] === '\\') { result += line[i] + (line[i + 1] ?? ''); i += 2; }
        else if (line[i] === '{') {
          if (line[i + 1] === '{') { result += '{'; i += 2; }
          else {
            i++;
            let depth = 1, expr = '';
            while (i < line.length && depth > 0) {
              if (line[i] === '{') { depth++; expr += line[i]; }
              else if (line[i] === '}') { depth--; if (depth > 0) expr += line[i]; }
              else expr += line[i];
              i++;
            }
            const transformed = transformExpr(expr.trim(), funcArities, duConstructors);
            result += '${' + transformed + '}';
          }
        } else { result += line[i++]; }
      }
      i++; // closing "
      result += '`';
    } else if (line[i] === '"') {
      result += line[i++];
      while (i < line.length) {
        if (line[i] === '\\' && i + 1 < line.length) { result += line[i] + line[i + 1]; i += 2; }
        else if (line[i] === '"') { result += line[i++]; break; }
        else { result += line[i++]; }
      }
    } else { result += line[i++]; }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Expression transformer
// ---------------------------------------------------------------------------

function transformFun(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  // "fun x y -> body" or "(fun x y -> body)"
  const stripped = expr.trim().replace(/^\(|\)$/g, '').trim();
  const m = stripped.match(/^fun\s+(.*?)\s*->\s*(.*)$/s);
  if (!m) return expr;
  const rawParams = m[1].trim();
  const body = m[2].trim();
  // Handle tuple param: "(a, b)"
  const tupleParamM = rawParams.match(/^\(([^)]+)\)$/);
  let jsParams: string;
  if (tupleParamM) {
    const parts = tupleParamM[1].split(',').map(p => p.trim());
    jsParams = '[' + parts.join(', ') + ']';
  } else {
    const params = rawParams.split(/\s+/);
    // For multi-param `fun`, produce (a, b) not ((a, b)) — JS multi-arg call
    jsParams = params.join(', ');
  }
  const jsBody = transformExpr(body, funcArities, duConstructors);
  return `(${jsParams}) => ${jsBody}`;
}

function transformIfElse(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  const t = expr.trim();
  if (!t.startsWith('if ')) return expr;
  let rest = t.slice(3);

  // Find "then" at top level
  const thenIdx = findKeyword(rest, 'then');
  if (thenIdx === -1) return expr;
  const cond = transformExpr(rest.slice(0, thenIdx).trim(), funcArities, duConstructors);
  rest = rest.slice(thenIdx + 4).trim();

  // Find "elif" or "else" at top level
  const elifIdx = findKeyword(rest, 'elif');
  const elseIdx = findKeyword(rest, 'else');

  if (elifIdx !== -1 && (elseIdx === -1 || elifIdx < elseIdx)) {
    const thenBody = transformExpr(rest.slice(0, elifIdx).trim(), funcArities, duConstructors);
    const elseBody = transformIfElse('if ' + rest.slice(elifIdx + 4).trim(), funcArities, duConstructors);
    return `(${cond}) ? ${thenBody} : ${elseBody}`;
  }
  if (elseIdx !== -1) {
    const thenBody = transformExpr(rest.slice(0, elseIdx).trim(), funcArities, duConstructors);
    const elseBody = transformExpr(rest.slice(elseIdx + 4).trim(), funcArities, duConstructors);
    return `(${cond}) ? ${thenBody} : ${elseBody}`;
  }
  return expr;
}

function parseMatchCases(casesStr: string): Array<{ pattern: string; body: string }> {
  // "| pat -> body | pat2 -> body2"
  // Handles OR patterns like "| 12 | 1 | 2 -> body" by accumulating
  // parts without "->" as OR-pattern components for the next case.
  const cases: Array<{ pattern: string; body: string }> = [];
  const parts = casesStr.split(/\|(?=\s*[\w_(])/).filter(p => p.trim());
  let orParts: string[] = [];
  for (const part of parts) {
    const trimmed = part.trim();
    const arrowIdx = trimmed.indexOf('->');
    if (arrowIdx === -1) {
      // This part has no arrow — it's an OR-pattern component
      orParts.push(trimmed.trim());
    } else {
      // Combine accumulated OR parts with this pattern
      const localPattern = trimmed.slice(0, arrowIdx).trim();
      const body = trimmed.slice(arrowIdx + 2).trim();
      const fullPattern = [...orParts, localPattern].join(' | ');
      orParts = [];
      cases.push({ pattern: fullPattern, body });
    }
  }
  return cases;
}

function transformMatchCase(pattern: string, body: string, matchVar: string,
  funcArities: Map<string, number>, duConstructors: Set<string>): string {
  const jsBody = transformExpr(body, funcArities, duConstructors);

  // Wildcard
  if (pattern === '_') return `return ${jsBody};`;

  // OR patterns: "12 | 1 | 2"
  if (pattern.includes('|')) {
    const alts = pattern.split('|').map(p => p.trim());
    const cond = alts.map(a => `${matchVar} === ${a}`).join(' || ');
    return `if (${cond}) return ${jsBody};`;
  }

  // DU case with tuple value: "Add (a, b)"
  const duTupleM = pattern.match(/^(\w+)\s+\(([^)]+)\)$/);
  if (duTupleM && duConstructors.has(duTupleM[1])) {
    const caseName = duTupleM[1];
    const bindings = duTupleM[2].split(',').map(p => p.trim());
    const destructure = `const [${bindings.join(', ')}] = ${matchVar}.value;`;
    return `if (${matchVar}.tag === "${caseName}") { ${destructure} return ${jsBody}; }`;
  }

  // DU case with single value: "Num n" or "Circle r"
  const duSingleM = pattern.match(/^(\w+)\s+(\w+)$/);
  if (duSingleM && duConstructors.has(duSingleM[1])) {
    const caseName = duSingleM[1];
    const binding = duSingleM[2];
    return `if (${matchVar}.tag === "${caseName}") { const ${binding} = ${matchVar}.value; return ${jsBody}; }`;
  }

  // DU case with no value: "NoValue"
  if (duConstructors.has(pattern)) {
    return `if (${matchVar}.tag === "${pattern}") return ${jsBody};`;
  }

  // Literal: number, string, bool
  return `if (${matchVar} === ${pattern}) return ${jsBody};`;
}

function transformMatchExpr(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  // "match X with | pat -> body | _ -> default"
  const matchM = expr.trim().match(/^match\s+(.+?)\s+with\s+(\|.*)$/s);
  if (!matchM) return expr;
  const subject = transformExpr(matchM[1].trim(), funcArities, duConstructors);
  const casesStr = matchM[2].trim();
  const cases = parseMatchCases(casesStr);
  if (!cases.length) return expr;

  const matchVar = '__m';
  const body = cases.map(c => transformMatchCase(c.pattern, c.body, matchVar, funcArities, duConstructors)).join(' ');
  return `(() => { const ${matchVar} = ${subject}; ${body} })()`;
}

function transformPipe(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  // Split on |> at top level
  const parts: string[] = [];
  let current = '';
  let depth = 0, inStr = false;
  for (let i = 0; i < expr.length; i++) {
    if (!inStr && expr[i] === '"') { inStr = true; current += expr[i]; continue; }
    if (inStr) { if (expr[i] === '\\') { current += expr[i++]; } current += expr[i]; if (expr[i] === '"') inStr = false; continue; }
    if (expr[i] === '(' || expr[i] === '[') { depth++; current += expr[i]; continue; }
    if (expr[i] === ')' || expr[i] === ']') { depth--; current += expr[i]; continue; }
    if (depth === 0 && expr[i] === '|' && expr[i + 1] === '>') {
      parts.push(current.trim()); current = ''; i++;
    } else { current += expr[i]; }
  }
  parts.push(current.trim());

  if (parts.length === 1) return transformExprNoopipe(expr, funcArities, duConstructors);

  let acc = transformExprNoopipe(parts[0], funcArities, duConstructors);
  for (const part of parts.slice(1)) {
    const fn = transformPipeSegment(part.trim(), funcArities, duConstructors);
    acc = `(${fn})(${acc})`;
  }
  return acc;
}

function transformPipeSegment(segment: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  // segment is a partial application like "List.filter f" or "List.sum"
  const toks = tokenizeArgs(segment);
  if (!toks.length) return segment;
  const head = toks[0];

  // fun lambda as pipe segment (unusual but handle)
  if (head === 'fun') return transformFun(segment, funcArities, duConstructors);

  const listArity = LIST_ARITIES[head];
  if (listArity !== undefined) {
    // Curried: List.X(arg1)(arg2)... (all args except the last which is piped)
    let result = head;
    for (let i = 1; i < toks.length; i++) {
      result += '(' + transformExprNoopipe(toks[i], funcArities, duConstructors) + ')';
    }
    return result;
  }

  const userArity = funcArities.get(head);
  if (userArity !== undefined && toks.length > 1) {
    // User function with partial args (curried)
    let result = head;
    for (let i = 1; i < toks.length; i++) {
      result += '(' + transformExprNoopipe(toks[i], funcArities, duConstructors) + ')';
    }
    return result;
  }

  // Plain function reference
  return transformExprNoopipe(segment, funcArities, duConstructors);
}

function transformListLiteralOrRange(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  if (!expr.startsWith('[')) return expr;
  const inner = expr.slice(1, -1).trim();
  if (!inner) return '[]';

  // Range: [1..5] or [1..2..10] (step range)
  const rangeM = inner.match(/^(.+?)\.\.(.*?)$/);
  if (rangeM) {
    const start = rangeM[1].trim();
    const rest = rangeM[2].trim();
    const stepM = rest.match(/^(.+?)\.\.(.*?)$/);
    if (stepM) {
      // [start..step..end]
      const step = stepM[1].trim();
      const end = stepM[2].trim();
      return `Array.from({length: Math.floor((${end} - ${start}) / ${step}) + 1}, (_, i) => ${start} + i * ${step})`;
    }
    const end = rest;
    return `Array.from({length: ${end} - ${start} + 1}, (_, i) => i + (${start}))`;
  }

  // List literal: [a; b; c] — semicolons are separators
  const elems = inner.split(';').map(e => transformExprNoopipe(e.trim(), funcArities, duConstructors));
  return '[' + elems.join(', ') + ']';
}

function transformRecordLiteral(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  // { X = 3; Y = 4 } → { X: 3, Y: 4 }
  if (!expr.startsWith('{')) return expr;
  const inner = expr.slice(1, -1).trim();
  if (!inner) return '{}';
  const fields = inner.split(';').map(f => {
    const eqIdx = f.indexOf('=');
    if (eqIdx === -1) return f.trim();
    const key = f.slice(0, eqIdx).trim();
    const val = transformExprNoopipe(f.slice(eqIdx + 1).trim(), funcArities, duConstructors);
    return `${key}: ${val}`;
  });
  return '{' + fields.join(', ') + '}';
}

// Core expression transform (without pipe handling at top level)
function transformExprNoopipe(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  expr = expr.trim();
  if (!expr) return expr;

  // fun lambda
  if (/^(?:\()?fun\s+/.test(expr)) return transformFun(expr, funcArities, duConstructors);

  // if/elif/else
  if (/^if\s+/.test(expr)) return transformIfElse(expr, funcArities, duConstructors);

  // match ... with
  if (/^match\s+/.test(expr)) return transformMatchExpr(expr, funcArities, duConstructors);

  // not x → !x
  if (/^not\s+/.test(expr)) return '!' + transformExprNoopipe(expr.slice(4), funcArities, duConstructors);

  // List literal or range
  if (expr.startsWith('[')) return transformListLiteralOrRange(expr, funcArities, duConstructors);

  // Record literal
  if (expr.startsWith('{') && findOutside(expr, '=') !== -1) return transformRecordLiteral(expr, funcArities, duConstructors);

  // Template literal (already transformed string interp)
  if (expr.startsWith('`')) return expr;

  // String literal
  if (expr.startsWith('"')) return expr;

  // Tuple (a, b) → [a, b] — check for top-level comma
  if (expr.startsWith('(') && expr.endsWith(')')) {
    const inner = expr.slice(1, -1);
    // Check for top-level comma
    let depth = 0;
    for (const c of inner) {
      if (c === '(' || c === '[') depth++;
      else if (c === ')' || c === ']') depth--;
      else if (c === ',' && depth === 0) {
        const parts = splitTopLevel(inner, ',');
        return '[' + parts.map(p => transformExprNoopipe(p.trim(), funcArities, duConstructors)).join(', ') + ']';
      }
    }
    // Not a tuple, just parens
    return '(' + transformExprNoopipe(inner, funcArities, duConstructors) + ')';
  }

  // Tokenize
  const toks = tokenizeArgs(expr);
  if (!toks.length) return expr;
  const head = toks[0];

  // DU constructor (starts with uppercase, is a known DU case)
  if (/^[A-Z]/.test(head) && duConstructors.has(head)) {
    if (toks.length === 1) return `{tag: "${head}", value: null}`;
    // Single arg or tuple arg
    const argExpr = toks.slice(1).join(' ');
    const arg = transformExprNoopipe(argExpr, funcArities, duConstructors);
    return `{tag: "${head}", value: ${arg}}`;
  }

  // List.* function application (curried)
  const listArity = LIST_ARITIES[head];
  if (listArity !== undefined && toks.length > 1) {
    let result = head;
    for (let i = 1; i < toks.length; i++) {
      result += '(' + transformExprNoopipe(toks[i], funcArities, duConstructors) + ')';
    }
    return result;
  }

  // PRELUDE function application (fst, snd, etc.)
  const preludeArity = PRELUDE_ARITIES[head];
  if (preludeArity !== undefined && toks.length > 1) {
    let result = head;
    for (let i = 1; i <= Math.min(preludeArity, toks.length - 1); i++) {
      result += '(' + transformExprNoopipe(toks[i], funcArities, duConstructors) + ')';
    }
    return result;
  }

  // User function application (curried)
  const userArity = funcArities.get(head);
  if (userArity !== undefined && toks.length > 1) {
    let result = head;
    const argsToApply = toks.slice(1, 1 + userArity);
    for (const a of argsToApply) {
      result += '(' + transformExprNoopipe(a, funcArities, duConstructors) + ')';
    }
    const extra = toks.slice(1 + userArity);
    for (const a of extra) {
      result += '(' + transformExprNoopipe(a, funcArities, duConstructors) + ')';
    }
    return result;
  }

  // Binary/misc expression: recursively transform each token
  if (toks.length > 1) {
    return toks.map(t => {
      // Operators: leave as-is (with equality transform)
      if (/^[+\-*/%<>&|^!]+$/.test(t) || t === '==' || t === '!=' || t === '>=' || t === '<=' || t === '=>') return t;
      if (t === '=') return '===';
      if (t === '<>') return '!==';
      return transformExprNoopipe(t, funcArities, duConstructors);
    }).join(' ');
  }

  // Single token: apply equality/inequality transforms
  let result = expr;
  // These are handled at token level above; for single tokens just return
  return result;
}

function splitTopLevel(s: string, sep: string): string[] {
  const parts: string[] = [];
  let depth = 0, current = '';
  for (let i = 0; i < s.length; i++) {
    if (s[i] === '(' || s[i] === '[') depth++;
    else if (s[i] === ')' || s[i] === ']') depth--;
    else if (depth === 0 && s.slice(i, i + sep.length) === sep) {
      parts.push(current); current = ''; i += sep.length - 1; continue;
    }
    current += s[i];
  }
  parts.push(current);
  return parts;
}

function transformExpr(expr: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  expr = expr.trim();
  // Handle pipe at top level
  if (findOutside(expr, '|>') !== -1) return transformPipe(expr, funcArities, duConstructors);
  return transformExprNoopipe(expr, funcArities, duConstructors);
}

// ---------------------------------------------------------------------------
// Printfn transform
// ---------------------------------------------------------------------------
function transformPrintfn(line: string, funcArities: Map<string, number>, duConstructors: Set<string>): string {
  const indent = ' '.repeat(getIndent(line));
  const t = line.trim();

  // printfn `template` (already string-interp-transformed)
  let m = t.match(/^printfn\s+(`[^`]*`)$/);
  if (m) return `${indent}println(${m[1]});`;

  // printf `template`
  m = t.match(/^printf\s+(`[^`]*`)$/);
  if (m) return `${indent}print(${m[1]});`;

  // printfn "literal"
  m = t.match(/^printfn\s+"([^"]*)"$/);
  if (m) return `${indent}println("${m[1]}");`;

  // printf "literal"
  m = t.match(/^printf\s+"([^"]*)"$/);
  if (m) return `${indent}print("${m[1]}");`;

  // printfn "%.Xf" expr → expr.toFixed(X)
  m = t.match(/^printfn\s+"%\.(\d+)f"\s+(.+)$/);
  if (m) {
    const e = transformExpr(m[2].trim(), funcArities, duConstructors);
    return `${indent}println((${e}).toFixed(${m[1]}));`;
  }

  // printfn "%d"/"%s"/"%f"/"%A"/"%g"/"%b"/"%i" expr
  m = t.match(/^printfn\s+"%(d|s|f|A|g|b|i|e)"\s+(.+)$/);
  if (m) {
    const e = transformExpr(m[2].trim(), funcArities, duConstructors);
    return `${indent}println(${e});`;
  }

  // printf "%d" etc.
  m = t.match(/^printf\s+"%(d|s|f|A|g|b|i|e)"\s+(.+)$/);
  if (m) {
    const e = transformExpr(m[2].trim(), funcArities, duConstructors);
    return `${indent}print(${e});`;
  }

  return line;
}

// ---------------------------------------------------------------------------
// let binding transform
// ---------------------------------------------------------------------------
function transformLetLine(
  line: string,
  funcArities: Map<string, number>,
  duConstructors: Set<string>
): string {
  const indent = ' '.repeat(getIndent(line));
  const t = line.trim();

  // let mutable x = expr → let x = expr
  const mutM = t.match(/^let\s+mutable\s+(\w+)\s*(?::\s*[^=]+)?\s*=\s*(.+)$/);
  if (mutM) {
    const rhs = transformExpr(mutM[2].trim(), funcArities, duConstructors);
    return `${indent}let ${mutM[1]} = ${rhs};`;
  }

  // let (a, b) = expr → const [a, b] = expr  (tuple destructure)
  const tupDestructM = t.match(/^let\s+\(([^)]+)\)\s*=\s*(.+)$/);
  if (tupDestructM) {
    const vars = tupDestructM[1].split(',').map(v => v.trim());
    const rhs = transformExpr(tupDestructM[2].trim(), funcArities, duConstructors);
    return `${indent}const [${vars.join(', ')}] = ${rhs};`;
  }

  // let [rec] name params... = body (function or value)
  const letM = t.match(/^let\s+(rec\s+)?(\w+)\s*(.*?)\s*=\s*(.+)$/);
  if (!letM) return line;
  const isRec = !!letM[1];
  const name = letM[2];
  const paramStr = letM[3].trim();
  const body = letM[4].trim();

  if (!paramStr) {
    // Value binding: let x = expr
    const rhs = transformExpr(body, funcArities, duConstructors);
    return `${indent}const ${name} = ${rhs};`;
  }

  // Function: parse params
  const paramToks = tokenizeArgs(paramStr);
  const jsParams: string[] = [];
  for (const p of paramToks) {
    // Type-annotated param: (name: type)
    const typedM = p.match(/^\((\w+)\s*:\s*[^)]+\)$/);
    if (typedM) { jsParams.push(typedM[1]); continue; }
    // Tuple param: (a, b)
    const tupleM = p.match(/^\(([^)]+)\)$/);
    if (tupleM && tupleM[1].includes(',')) {
      const parts = tupleM[1].split(',').map(x => x.trim());
      jsParams.push('[' + parts.join(', ') + ']'); continue;
    }
    jsParams.push(p);
  }

  const jsBody = transformExpr(body, funcArities, duConstructors);

  // Build curried function
  if (isRec) {
    // Use function declaration for recursion (hoisted)
    if (jsParams.length === 1) {
      return `${indent}function ${name}(${jsParams[0]}) { return ${jsBody}; }`;
    }
    // Multi-param curried: function f(a) { return (b) => ... }
    const [first, ...rest] = jsParams;
    let inner = `return ${jsBody};`;
    for (const p of [...rest].reverse()) {
      inner = `return (${p}) => { ${inner} };`;
    }
    return `${indent}function ${name}(${first}) { ${inner} }`;
  } else {
    // const f = (a) => (b) => body  (curried)
    if (jsParams.length === 1) {
      return `${indent}const ${name} = (${jsParams[0]}) => ${jsBody};`;
    }
    const [last, ...init] = [...jsParams].reverse();
    let result = `(${last}) => ${jsBody}`;
    for (const p of init) {
      result = `(${p}) => ${result}`;
    }
    return `${indent}const ${name} = ${result};`;
  }
}

// ---------------------------------------------------------------------------
// Type declaration → generate DU factories or skip (records)
// ---------------------------------------------------------------------------
function handleTypeDecl(line: string, out: string[], duTypes: Map<string, DUType>): void {
  const t = line.trim();
  const typeM = t.match(/^type\s+(\w+)\s*=/);
  if (!typeM) return;
  const typeName = typeM[1];
  const duType = duTypes.get(typeName);
  if (!duType) return; // record type or simple alias — skip

  const indent = ' '.repeat(getIndent(line));
  for (const [caseName, arity] of duType.cases) {
    if (arity === -1) {
      // No value: const A = {tag: "A", value: null}
      out.push(`${indent}const ${caseName} = {tag: "${caseName}", value: null};`);
    } else {
      // Has value: const A = (value) => ({tag: "A", value})
      out.push(`${indent}const ${caseName} = (value) => ({tag: "${caseName}", value});`);
    }
  }
}

// ---------------------------------------------------------------------------
// Main transpiler
// ---------------------------------------------------------------------------
export function transpileFSharp(code: string): string {
  const rawLines = code.split('\n');

  // Phase 0: collect arities and DU info
  const funcArities = collectFuncArities(rawLines);
  const { duTypes, duConstructors } = collectDUTypes(rawLines);

  // Phase 1: string interpolation pass
  const interpLines = rawLines.map(l => transformStringInterp(l, funcArities, duConstructors));

  // Phase 2: join continuations
  const lines = joinContinuations(interpLines);

  const out: string[] = [PRELUDE];
  let i = 0;
  // Track which type declaration lines to skip (DU case lines)
  let skipUntilDedent = -1;

  while (i < lines.length) {
    const raw = lines[i];
    const t = raw.trim();
    i++;

    if (!t) { out.push(''); continue; }
    if (t.startsWith('//')) { out.push(raw); continue; }

    // Skip DU continuation lines (| Case -> ...) if already handled by type decl
    if (skipUntilDedent >= 0) {
      if (getIndent(raw) > skipUntilDedent || t.startsWith('|')) continue;
      skipUntilDedent = -1;
    }

    // Type declaration
    if (/^type\s+\w+/.test(t)) {
      const duType = duTypes.get(t.match(/^type\s+(\w+)/)?.[1] ?? '');
      if (duType) {
        handleTypeDecl(raw, out, duTypes);
        skipUntilDedent = getIndent(raw);
      }
      // else: record type — skip
      continue;
    }

    // Mutable assignment: x <- expr
    const mutableAssignM = raw.match(/^(\s*)(\w+)\s*<-\s*(.+)$/);
    if (mutableAssignM) {
      const rhs = transformExpr(mutableAssignM[3].trim(), funcArities, duConstructors);
      out.push(`${mutableAssignM[1]}${mutableAssignM[2]} = ${rhs};`);
      continue;
    }

    // printfn / printf
    if (/^\s*printfn\b/.test(raw) || /^\s*printf\b/.test(raw)) {
      out.push(transformPrintfn(raw, funcArities, duConstructors));
      continue;
    }

    // let binding
    if (/^\s*let\b/.test(raw)) {
      out.push(transformLetLine(raw, funcArities, duConstructors));
      continue;
    }

    // Bare expression (e.g., result of some computation printed inline)
    // Just transform and leave
    const transformed = transformExpr(t, funcArities, duConstructors);
    out.push(' '.repeat(getIndent(raw)) + transformed + ';');
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join('\n');
}

// ---------------------------------------------------------------------------
// Extract F# declarations for {{FUNC}} test pattern
// Extracts: let [rec] name params = body (function definitions with params)
//           type declarations
// Skips: simple value bindings, printfn calls, etc.
// ---------------------------------------------------------------------------
export function extractFSharpDeclarations(code: string): string {
  const rawLines = code.split('\n');
  const joined = joinContinuations(rawLines);
  const result: string[] = [];
  let inTypeDecl = false;

  for (const line of joined) {
    const t = line.trim();
    if (!t || t.startsWith('//')) { inTypeDecl = false; continue; }

    // Type declarations — emit all lines including DU case lines
    if (/^type\s+\w+/.test(t)) {
      result.push(line);
      inTypeDecl = true;
      continue;
    }

    // Collect DU case continuation lines (| Case of ...)
    if (inTypeDecl) {
      if (t.startsWith('|')) {
        result.push(line);
        continue;
      } else {
        inTypeDecl = false;
      }
    }

    // let [rec] name params = body — only if has params (is a function)
    if (/^let\s+(?:rec\s+)?\w+/.test(t)) {
      // Check for "let mutable" — skip
      if (/^let\s+mutable/.test(t)) continue;
      // Check for tuple destructure "let (a, b) = ..." — skip
      if (/^let\s+\(/.test(t)) continue;

      // Extract param string between name and =
      const m = t.match(/^let\s+(?:rec\s+)?(\w+)\s+(.*?)=(.*)$/);
      if (!m) continue;
      const paramStr = m[2].trim();
      // If paramStr is non-empty and looks like params (not empty), it's a function
      if (paramStr.length > 0 && !/^\s*$/.test(paramStr)) {
        result.push(line);
      }
      // else: value binding, skip
    }
  }

  return result.join('\n').trim();
}

// ---------------------------------------------------------------------------
// Browser runner API
// ---------------------------------------------------------------------------
export async function runFSharp(code: string): Promise<RunResult> {
  try {
    const js = transpileFSharp(code);
    const captured: string[] = [];
    const origLog = console.log;
    console.log = (...args: unknown[]) => captured.push(args.join(' '));
    try {
      // eslint-disable-next-line no-new-func
      new Function(js)();
    } finally {
      console.log = origLog;
    }
    const stdout = captured.join('\n');
    return { stdout, stderr: '', error: '' };
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    return { stdout: '', stderr: msg, error: msg };
  }
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const results: TestResult[] = [];
  for (const test of tests) {
    let codeToRun = code;
    if (test.code) {
      const decls = extractFSharpDeclarations(code);
      codeToRun = test.code.replace('{{FUNC}}', decls);
    }
    const result = await runFSharp(codeToRun);
    const actual = result.stdout || (result.error ? result.error + '\n' : '');
    results.push({
      name: test.name,
      passed: actual === test.expected,
      actual,
      expected: test.expected,
    });
  }
  return results;
}
