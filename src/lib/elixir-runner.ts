import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let elixirReady = false;
export function isElixirReady(): boolean { return elixirReady; }
export async function initElixirRunner(): Promise<void> { elixirReady = true; }

// ---------------------------------------------------------------------------
// Prelude
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
const __nativeStr = v => '' + v;

function __inspect(v) {
  if (v === null || v === undefined) return 'nil';
  if (v === true) return 'true';
  if (v === false) return 'false';
  if (typeof v === 'string') return JSON.stringify(v);
  if (Array.isArray(v)) {
    if (v.__tuple) return '{' + v.map(__inspect).join(', ') + '}';
    return '[' + v.map(__inspect).join(', ') + ']';
  }
  if (typeof v === 'object') {
    const entries = Object.entries(v).map(([k,vv]) => k + ': ' + __inspect(vv));
    return '%{' + entries.join(', ') + '}';
  }
  return __nativeStr(v);
}

function __puts(v) {
  if (v === null || v === undefined) { __output.push('nil\\n'); return; }
  if (typeof v === 'boolean') { __output.push(__nativeStr(v) + '\\n'); return; }
  if (typeof v === 'string') { __output.push(v + '\\n'); return; }
  if (Array.isArray(v)) {
    if (v.__tuple) { __output.push('(' + v.map(__inspect).join(', ') + ')\\n'); return; }
    __output.push('[' + v.map(__inspect).join(', ') + ']\\n'); return;
  }
  if (typeof v === 'object') {
    const entries = Object.entries(v).map(([k,vv]) => k + ': ' + __inspect(vv));
    __output.push('%{' + entries.join(', ') + '}\\n'); return;
  }
  __output.push(__nativeStr(v) + '\\n');
}

const IO = {
  puts: __puts,
  inspect: function(v) {
    __output.push(__inspect(v) + '\\n');
    return v;
  },
};

function __tuple(arr) {
  arr.__tuple = true;
  return arr;
}

function __floatRound(x, digits) {
  const factor = Math.pow(10, digits);
  return Math.round(x * factor) / factor;
}

function div(a, b) { return Math.trunc(a / b); }
function rem(a, b) { return a % b; }
function hd(list) { return list[0]; }
function tl(list) { return list.slice(1); }
function length(x) { return x.length; }
function abs(x) { return Math.abs(x); }

const Enum = {
  map: function(list, f) { return list.map(f); },
  filter: function(list, f) { return list.filter(f); },
  reduce: function(list, acc, f) { return list.reduce(f, acc); },
  join: function(list, sep) { return list.join(sep === undefined ? '' : sep); },
  sum: function(list) { return list.reduce(function(a,b){return a+b;}, 0); },
  any: function(list, f) { return list.some(f); },
  all: function(list, f) { return list.every(f); },
  count: function(list, f) { return f ? list.filter(f).length : list.length; },
  sort: function(list) { return list.slice().sort(function(a,b){ return a < b ? -1 : a > b ? 1 : 0; }); },
  sort_by: function(list, f) { return list.slice().sort(function(a,b){ const fa=f(a),fb=f(b); return fa<fb?-1:fa>fb?1:0; }); },
  reverse: function(list) { return list.slice().reverse(); },
  take: function(list, n) { return list.slice(0, n); },
  drop: function(list, n) { return list.slice(n); },
  uniq: function(list) { return [...new Set(list)]; },
  flat_map: function(list, f) { return list.flatMap(f); },
  min: function(list) { return Math.min.apply(null, list); },
  max: function(list) { return Math.max.apply(null, list); },
  each: function(list, f) { list.forEach(f); return list; },
  find: function(list, f) { return list.find(f) ?? null; },
  member: function(list, x) { return list.includes(x); },
  zip: function(a, b) { return a.map(function(v,i){ return __tuple([v, b[i]]); }); },
  with_index: function(list) { return list.map(function(v,i){ return __tuple([v, i]); }); },
  into: function(list) { return list; },
};

const String = {
  upcase: function(s) { return s.toUpperCase(); },
  downcase: function(s) { return s.toLowerCase(); },
  length: function(s) { return s.length; },
  reverse: function(s) { return s.split('').reverse().join(''); },
  split: function(s, sep) { return s.split(sep); },
  trim: function(s) { return s.trim(); },
  contains: function(s, sub) { return s.includes(sub); },
  replace: function(s, a, b) { return s.split(a).join(b); },
  starts_with: function(s, pre) { return s.startsWith(pre); },
  ends_with: function(s, suf) { return s.endsWith(suf); },
  to_integer: function(s) { return parseInt(s, 10); },
  to_float: function(s) { return parseFloat(s); },
  pad_leading: function(s, n, ch) { return s.padStart(n, ch || ' '); },
  pad_trailing: function(s, n, ch) { return s.padEnd(n, ch || ' '); },
};

const Float = {
  round: __floatRound,
  ceil: function(x, d) { const f=Math.pow(10,d||0); return Math.ceil(x*f)/f; },
  floor: function(x, d) { const f=Math.pow(10,d||0); return Math.floor(x*f)/f; },
};

const Integer = {
  to_string: function(n) { return __nativeStr(n); },
  digits: function(n) {
    return String(Math.abs(n)).split('').map(Number).reverse();
  },
};

const Map = {
  get: function(m, k) { return m[k] ?? null; },
  put: function(m, k, v) { const r = Object.assign({}, m); r[k] = v; return r; },
  delete: function(m, k) { const r = Object.assign({}, m); delete r[k]; return r; },
  keys: function(m) { return Object.keys(m); },
  values: function(m) { return Object.values(m); },
  has_key: function(m, k) { return k in m; },
  merge: function(a, b) { return Object.assign({}, a, b); },
};

const List = {
  first: function(l) { return l[0]; },
  last: function(l) { return l[l.length - 1]; },
  flatten: function(l) { return l.flat(Infinity); },
  uniq: function(l) { return [...new Set(l)]; },
};
`;

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

function stripElixirComments(code: string): string {
  const lines = code.split("\n");
  return lines
    .map((line) => {
      let inStr = false;
      for (let i = 0; i < line.length; i++) {
        if (line[i] === "\\" && inStr) { i++; continue; }
        if (line[i] === '"') { inStr = !inStr; continue; }
        if (!inStr && line[i] === "#") {
          return line.slice(0, i);
        }
      }
      return line;
    })
    .join("\n");
}

function splitByCommaEx(s: string): string[] {
  const items: string[] = [];
  let depth = 0;
  let inStr = false;
  let current = "";
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { current += ch + s[i + 1]; i++; continue; }
    if (ch === '"') { inStr = !inStr; current += ch; continue; }
    if (inStr) { current += ch; continue; }
    if ("([{".includes(ch)) { depth++; current += ch; continue; }
    if (")]}".includes(ch)) { depth--; current += ch; continue; }
    if (ch === "," && depth === 0) { items.push(current.trim()); current = ""; continue; }
    current += ch;
  }
  if (current.trim()) items.push(current.trim());
  return items;
}

function findMatchingCloseEx(s: string, startIdx: number, open: string, close: string): number {
  let depth = 1;
  let inStr = false;
  let i = startIdx + 1;
  while (i < s.length) {
    const ch = s[i];
    if (ch === "\\" && inStr) { i += 2; continue; }
    if (ch === '"') { inStr = !inStr; i++; continue; }
    if (inStr) { i++; continue; }
    if (ch === open) depth++;
    else if (ch === close) { depth--; if (depth === 0) return i; }
    i++;
  }
  return s.length - 1;
}

function scanStringEndEx(s: string, start: number): number {
  let i = start + 1;
  while (i < s.length) {
    if (s[i] === "\\") { i += 2; continue; }
    if (s[i] === '"') return i + 1;
    i++;
  }
  return s.length;
}

// ---------------------------------------------------------------------------
// String interpolation: "Hello #{name}" -> `Hello ${name}`
// ---------------------------------------------------------------------------

function transpileStringInterp(s: string): string {
  if (!s.startsWith('"')) return s;
  const inner = s.slice(1, -1);
  let result = "";
  let i = 0;
  let hasInterp = false;
  while (i < inner.length) {
    const ch = inner[i];
    if (ch === "\\") { result += ch + (inner[i + 1] || ""); i += 2; continue; }
    if (ch === "`") { result += "\\`"; i++; continue; }
    if (ch === "#" && inner[i + 1] === "{") {
      hasInterp = true;
      const closeIdx = findMatchingCloseEx(inner, i + 1, "{", "}");
      const expr = inner.slice(i + 2, closeIdx);
      result += "${" + transpileExpr(expr) + "}";
      i = closeIdx + 1;
    } else {
      result += ch;
      i++;
    }
  }
  if (!hasInterp) return s; // plain string, keep as-is
  return "`" + result + "`";
}

// ---------------------------------------------------------------------------
// Atom: :foo -> "foo"
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Pattern compiler for case/multi-head matching
// ---------------------------------------------------------------------------

interface PatternResult {
  test: string;
  bindings: string[];
}

function compilePat(pat: string, argVar: string): PatternResult {
  pat = pat.trim();

  // Wildcard
  if (pat === "_") return { test: "true", bindings: [] };

  // true / false / nil
  if (pat === "true") return { test: `${argVar} === true`, bindings: [] };
  if (pat === "false") return { test: `${argVar} === false`, bindings: [] };
  if (pat === "nil") return { test: `${argVar} === null`, bindings: [] };

  // Atom :foo
  if (/^:[a-zA-Z_][a-zA-Z0-9_?!]*$/.test(pat)) {
    const name = pat.slice(1);
    return { test: `${argVar} === ${JSON.stringify(name)}`, bindings: [] };
  }

  // Integer literal
  if (/^-?\d+$/.test(pat)) {
    return { test: `${argVar} === ${pat}`, bindings: [] };
  }

  // Float literal
  if (/^-?\d+\.\d+$/.test(pat)) {
    return { test: `${argVar} === ${pat}`, bindings: [] };
  }

  // String literal
  if (pat.startsWith('"') && pat.endsWith('"')) {
    return { test: `${argVar} === ${pat}`, bindings: [] };
  }

  // Empty list []
  if (pat === "[]") {
    return { test: `Array.isArray(${argVar}) && !${argVar}.__tuple && ${argVar}.length === 0`, bindings: [] };
  }

  // Cons [h | t]
  const consMatch = pat.match(/^\[(\w+)\s*\|\s*(\w+)\]$/);
  if (consMatch) {
    const [, h, t] = consMatch;
    return {
      test: `Array.isArray(${argVar}) && !${argVar}.__tuple && ${argVar}.length >= 1`,
      bindings: [`const ${h} = ${argVar}[0]; const ${t} = ${argVar}.slice(1);`],
    };
  }

  // List literal with patterns [a, b, c]
  if (pat.startsWith("[") && pat.endsWith("]")) {
    const items = splitByCommaEx(pat.slice(1, -1));
    const tests: string[] = [`Array.isArray(${argVar}) && !${argVar}.__tuple && ${argVar}.length === ${items.length}`];
    const bindings: string[] = [];
    for (let idx = 0; idx < items.length; idx++) {
      const sub = compilePat(items[idx].trim(), `${argVar}[${idx}]`);
      if (sub.test !== "true") tests.push(sub.test);
      bindings.push(...sub.bindings);
    }
    return { test: tests.join(" && "), bindings };
  }

  // Tuple {a, b, ...}
  if (pat.startsWith("{") && pat.endsWith("}")) {
    const items = splitByCommaEx(pat.slice(1, -1));
    const tests: string[] = [`Array.isArray(${argVar}) && ${argVar}.__tuple && ${argVar}.length === ${items.length}`];
    const bindings: string[] = [];
    for (let idx = 0; idx < items.length; idx++) {
      const sub = compilePat(items[idx].trim(), `${argVar}[${idx}]`);
      if (sub.test !== "true") tests.push(sub.test);
      bindings.push(...sub.bindings);
    }
    return { test: tests.join(" && "), bindings };
  }

  // Variable (identifier, lower-case or with underscore)
  if (/^[a-z_][a-zA-Z0-9_]*$/.test(pat)) {
    return { test: "true", bindings: [`const ${pat} = ${argVar};`] };
  }

  // Fallback: treat as expression
  return { test: `${argVar} === ${transpileExpr(pat)}`, bindings: [] };
}

// ---------------------------------------------------------------------------
// Expression transformer
// ---------------------------------------------------------------------------

export function transpileExpr(s: string): string {
  s = s.trim();
  if (!s) return "";

  // Handle |> pipe chains first
  if (hasPipeAtTop(s)) {
    return compilePipeChain(s);
  }

  // Handle ++ list concat at top level
  if (hasConcatAtTop(s)) {
    return compileConcat(s);
  }

  let result = "";
  let i = 0;

  while (i < s.length) {
    // String literal
    if (s[i] === '"') {
      const end = scanStringEndEx(s, i);
      const strLit = s.slice(i, end);
      result += transpileStringInterp(strLit);
      i = end;
      continue;
    }

    // Atom literal :foo
    if (s[i] === ":" && i < s.length - 1 && /[a-zA-Z_]/.test(s[i + 1])) {
      let j = i + 1;
      while (j < s.length && /[a-zA-Z0-9_?!]/.test(s[j])) j++;
      const atom = s.slice(i, j);
      // Only treat as atom if not preceded by a word char (not module accessor like :ok in %{key: val})
      if (i === 0 || /[\s,([{+\-*\/=<>!&|?:^~]/.test(s[i - 1])) {
        result += JSON.stringify(atom.slice(1));
        i = j;
        continue;
      }
    }

    // Map literal %{...}
    if (s[i] === "%" && s[i + 1] === "{") {
      const closeIdx = findMatchingCloseEx(s, i + 1, "{", "}");
      const inner = s.slice(i + 2, closeIdx);
      const pairs = splitByCommaEx(inner);
      const entries = pairs.map((pair) => {
        // key: val or "key" => val
        const arrowIdx = pair.indexOf("=>");
        if (arrowIdx !== -1) {
          const key = transpileExpr(pair.slice(0, arrowIdx).trim());
          const val = transpileExpr(pair.slice(arrowIdx + 2).trim());
          return `[${key}]: ${val}`;
        }
        const colonIdx = pair.lastIndexOf(":");
        if (colonIdx !== -1 && pair.slice(0, colonIdx).trim().match(/^[a-zA-Z_][a-zA-Z0-9_]*$/)) {
          const key = pair.slice(0, colonIdx).trim();
          const val = transpileExpr(pair.slice(colonIdx + 1).trim());
          return `${key}: ${val}`;
        }
        return transpileExpr(pair);
      });
      result += "{" + entries.join(", ") + "}";
      i = closeIdx + 1;
      continue;
    }

    // Tuple {a, b, ...}
    if (s[i] === "{") {
      // Check if this is a tuple (not a block)
      const closeIdx = findMatchingCloseEx(s, i, "{", "}");
      const inner = s.slice(i + 1, closeIdx).trim();
      // If inner has commas at top level or is empty, treat as tuple
      const items = splitByCommaEx(inner);
      if (items.length >= 2 || (items.length === 1 && inner.trim() !== "")) {
        const jsItems = items.map((item) => transpileExpr(item.trim()));
        result += `__tuple([${jsItems.join(", ")}])`;
        i = closeIdx + 1;
        continue;
      }
    }

    // List [...]
    if (s[i] === "[") {
      const closeIdx = findMatchingCloseEx(s, i, "[", "]");
      const inner = s.slice(i + 1, closeIdx).trim();
      if (!inner) {
        result += "[]";
        i = closeIdx + 1;
        continue;
      }
      // Check for cons [head | tail]
      const pipeIdx = findTopLevelPipe(inner);
      if (pipeIdx !== -1) {
        const headPart = inner.slice(0, pipeIdx).trim();
        const tailPart = inner.slice(pipeIdx + 1).trim();
        // Could be [a, b | rest] or [h | t]
        const headItems = splitByCommaEx(headPart).map((x) => transpileExpr(x.trim()));
        const tailExpr = transpileExpr(tailPart);
        result += `[${headItems.join(", ")}, ...${tailExpr}]`;
        i = closeIdx + 1;
        continue;
      }
      const items = splitByCommaEx(inner).map((x) => transpileExpr(x.trim()));
      result += "[" + items.join(", ") + "]";
      i = closeIdx + 1;
      continue;
    }

    // Anonymous function: fn args -> expr end
    if (s.slice(i, i + 3) === "fn ") {
      const endIdx = findKeywordEnd(s, i);
      const fnBody = s.slice(i + 3, endIdx).trim();
      const arrowIdx = fnBody.indexOf(" -> ");
      if (arrowIdx !== -1) {
        const params = fnBody.slice(0, arrowIdx).trim();
        const body = fnBody.slice(arrowIdx + 4).trim();
        const jsParams = params || "";
        result += `function(${jsParams}) { return ${transpileExpr(body)}; }`;
      } else {
        result += `function() {}`;
      }
      i = endIdx + 3; // skip "end"
      continue;
    }

    // Capture syntax: &(&1 + &2) or &func/arity or &Module.func/arity
    if (s[i] === "&") {
      if (s[i + 1] === "(") {
        const closeIdx = findMatchingCloseEx(s, i + 1, "(", ")");
        const inner = s.slice(i + 2, closeIdx);
        const body = inner.replace(/&(\d+)/g, (_, n) => `__a${n}`);
        // Count how many &N refs
        const refs = new Set<string>();
        inner.replace(/&(\d+)/g, (_, n) => { refs.add(n); return ""; });
        const maxN = refs.size > 0 ? Math.max(...[...refs].map(Number)) : 1;
        const params = Array.from({ length: maxN }, (_, k) => `__a${k + 1}`).join(", ");
        result += `function(${params}) { return ${transpileExpr(body)}; }`;
        i = closeIdx + 1;
        continue;
      }
      // &Module.func/arity or &func/arity
      let j = i + 1;
      while (j < s.length && /[a-zA-Z0-9_.?!\/]/.test(s[j])) j++;
      const capture = s.slice(i + 1, j);
      // Parse Module.func/n or func/n
      const slashIdx = capture.lastIndexOf("/");
      if (slashIdx !== -1) {
        const funcRef = capture.slice(0, slashIdx);
        const arity = parseInt(capture.slice(slashIdx + 1));
        const params = Array.from({ length: arity }, (_, k) => `__c${k}`).join(", ");
        const dotIdx = funcRef.lastIndexOf(".");
        if (dotIdx !== -1) {
          const mod = funcRef.slice(0, dotIdx);
          const fn = funcRef.slice(dotIdx + 1);
          result += `function(${params}) { return ${mod}.${fn}(${params}); }`;
        } else {
          result += `function(${params}) { return ${funcRef}(${params}); }`;
        }
        i = j;
        continue;
      }
    }

    // IO.puts / IO.inspect
    if (s.slice(i, i + 8) === "IO.puts(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__puts(";
      i += 8;
      continue;
    }
    if (s.slice(i, i + 11) === "IO.inspect(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "IO.inspect(";
      i += 11;
      continue;
    }

    // Float.round(x, n) -> __floatRound(x, n)
    if (s.slice(i, i + 12) === "Float.round(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__floatRound(";
      i += 12;
      continue;
    }

    // Enum.* method calls
    const enumMatch = s.slice(i).match(/^Enum\.(\w+)\(/);
    if (enumMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "Enum." + enumMatch[1] + "(";
      i += enumMatch[0].length;
      continue;
    }

    // String.* -> String.method(
    const stringMatch = s.slice(i).match(/^String\.(\w+)\(/);
    if (stringMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "String." + stringMatch[1] + "(";
      i += stringMatch[0].length;
      continue;
    }

    // Float.* -> Float.method(
    const floatMatch = s.slice(i).match(/^Float\.(\w+)\(/);
    if (floatMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "Float." + floatMatch[1] + "(";
      i += floatMatch[0].length;
      continue;
    }

    // Map.* -> Map.method(
    const mapMatch = s.slice(i).match(/^Map\.(\w+)\(/);
    if (mapMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "Map." + mapMatch[1] + "(";
      i += mapMatch[0].length;
      continue;
    }

    // List.* -> List.method(
    const listMatch = s.slice(i).match(/^List\.(\w+)\(/);
    if (listMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "List." + listMatch[1] + "(";
      i += listMatch[0].length;
      continue;
    }

    // Integer.* -> Integer.method(
    const intMatch = s.slice(i).match(/^Integer\.(\w+)\(/);
    if (intMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "Integer." + intMatch[1] + "(";
      i += intMatch[0].length;
      continue;
    }

    // Anonymous fn call: f.(args) -> f(args)
    if (s[i] === "." && s[i + 1] === "(") {
      // Replace .( with (
      result += "(";
      i += 2;
      continue;
    }

    // Map key access via dot: map.key
    if (s[i] === "." && i > 0) {
      const prev = s.slice(0, i).trimEnd();
      if (prev.length > 0 && /[a-zA-Z0-9_\])]/.test(prev[prev.length - 1])) {
        let j = i + 1;
        while (j < s.length && /[a-zA-Z0-9_]/.test(s[j])) j++;
        if (j < s.length && s[j] !== "(") {
          result += "." + s.slice(i + 1, j);
          i = j;
          continue;
        }
      }
    }

    // <> string concat operator
    if (s[i] === "<" && s[i + 1] === ">") {
      result += " + ";
      i += 2;
      continue;
    }

    // not operator
    if (s.slice(i, i + 4) === "not ") {
      result += "!";
      i += 4;
      continue;
    }

    // and operator
    if (s.slice(i, i + 5) === " and ") {
      result += " && ";
      i += 5;
      continue;
    }

    // or operator
    if (s.slice(i, i + 4) === " or ") {
      result += " || ";
      i += 4;
      continue;
    }

    // == -> ===
    if (s[i] === "=" && s[i + 1] === "=") {
      result += "===";
      i += 2;
      continue;
    }

    // != -> !==
    if (s[i] === "!" && s[i + 1] === "=") {
      result += "!==";
      i += 2;
      continue;
    }

    result += s[i];
    i++;
  }

  return result;
}

// Find pipe operator | not inside brackets/strings at top level in list
function findTopLevelPipe(s: string): number {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { i++; continue; }
    if (ch === '"') { inStr = !inStr; continue; }
    if (inStr) continue;
    if ("([{".includes(ch)) depth++;
    else if (")]}".includes(ch)) depth--;
    else if (ch === "|" && depth === 0 && s[i - 1] !== "|" && s[i + 1] !== ">") return i;
  }
  return -1;
}

function hasConcatAtTop(s: string): boolean {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length - 1; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { i++; continue; }
    if (ch === '"') { inStr = !inStr; continue; }
    if (inStr) continue;
    if ("([{".includes(ch)) depth++;
    else if (")]}".includes(ch)) depth--;
    else if (ch === "+" && s[i + 1] === "+" && depth === 0) return true;
  }
  return false;
}

function compileConcat(s: string): string {
  const parts: string[] = [];
  let depth = 0;
  let inStr = false;
  let current = "";
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { current += ch + s[i + 1]; i++; continue; }
    if (ch === '"') { inStr = !inStr; current += ch; continue; }
    if (inStr) { current += ch; continue; }
    if ("([{".includes(ch)) { depth++; current += ch; continue; }
    if (")]}".includes(ch)) { depth--; current += ch; continue; }
    if (ch === "+" && s[i + 1] === "+" && depth === 0) {
      parts.push(current.trim());
      current = "";
      i++; // skip second +
      continue;
    }
    current += ch;
  }
  if (current.trim()) parts.push(current.trim());
  // a ++ b ++ c → a.concat(b).concat(c)
  return parts.map((p) => transpileExpr(p)).reduce((acc, cur) => `${acc}.concat(${cur})`);
}

function hasPipeAtTop(s: string): boolean {
  let depth = 0;
  let inStr = false;
  for (let i = 0; i < s.length - 1; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { i++; continue; }
    if (ch === '"') { inStr = !inStr; continue; }
    if (inStr) continue;
    if ("([{".includes(ch)) depth++;
    else if (")]}".includes(ch)) depth--;
    else if (ch === "|" && s[i + 1] === ">" && depth === 0) return true;
  }
  return false;
}

function compilePipeChain(s: string): string {
  // Split by |> at top level
  const parts: string[] = [];
  let depth = 0;
  let inStr = false;
  let current = "";
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "\\" && inStr) { current += ch + s[i + 1]; i++; continue; }
    if (ch === '"') { inStr = !inStr; current += ch; continue; }
    if (inStr) { current += ch; continue; }
    if ("([{".includes(ch)) { depth++; current += ch; continue; }
    if (")]}".includes(ch)) { depth--; current += ch; continue; }
    if (ch === "|" && s[i + 1] === ">" && depth === 0) {
      parts.push(current.trim());
      current = "";
      i++; // skip >
      continue;
    }
    current += ch;
  }
  if (current.trim()) parts.push(current.trim());

  if (parts.length <= 1) return transpileExprNoPipe(s);

  // Build chained calls
  let result = transpileExprNoPipe(parts[0]);
  for (let i = 1; i < parts.length; i++) {
    const fn = parts[i].trim();
    // fn could be: "Enum.map(fn x -> x*2 end)" or "String.upcase" or "myFunc"
    const parenIdx = fn.indexOf("(");
    if (parenIdx !== -1) {
      const fnName = fn.slice(0, parenIdx);
      const argsInner = fn.slice(parenIdx + 1, findMatchingCloseEx(fn, parenIdx, "(", ")"));
      const argsList = argsInner.trim() ? splitByCommaEx(argsInner).map((a) => transpileExpr(a.trim())).join(", ") : "";
      result = `${transpileExprNoPipe(fnName)}(${result}${argsList ? ", " + argsList : ""})`;
    } else {
      // No parens: just call as function
      result = `${transpileExprNoPipe(fn)}(${result})`;
    }
  }
  return result;
}

function transpileExprNoPipe(s: string): string {
  s = s.trim();
  if (!s) return "";

  // Handle ++ list concat at top level
  if (hasConcatAtTop(s)) {
    return compileConcat(s);
  }

  let result = "";
  let i = 0;

  while (i < s.length) {
    if (s[i] === '"') {
      const end = scanStringEndEx(s, i);
      result += transpileStringInterp(s.slice(i, end));
      i = end;
      continue;
    }
    if (s[i] === ":" && i < s.length - 1 && /[a-zA-Z_]/.test(s[i + 1])) {
      if (i === 0 || /[\s,([{+\-*\/=<>!&|?:^~]/.test(s[i - 1])) {
        let j = i + 1;
        while (j < s.length && /[a-zA-Z0-9_?!]/.test(s[j])) j++;
        result += JSON.stringify(s.slice(i + 1, j));
        i = j;
        continue;
      }
    }
    if (s[i] === "%" && s[i + 1] === "{") {
      const closeIdx = findMatchingCloseEx(s, i + 1, "{", "}");
      const inner = s.slice(i + 2, closeIdx);
      const pairs = splitByCommaEx(inner);
      const entries = pairs.map((pair) => {
        const arrowIdx = pair.indexOf("=>");
        if (arrowIdx !== -1) {
          const key = transpileExpr(pair.slice(0, arrowIdx).trim());
          const val = transpileExpr(pair.slice(arrowIdx + 2).trim());
          return `[${key}]: ${val}`;
        }
        const colonIdx = pair.lastIndexOf(":");
        if (colonIdx !== -1 && pair.slice(0, colonIdx).trim().match(/^[a-zA-Z_][a-zA-Z0-9_]*$/)) {
          const key = pair.slice(0, colonIdx).trim();
          const val = transpileExpr(pair.slice(colonIdx + 1).trim());
          return `${key}: ${val}`;
        }
        return transpileExpr(pair);
      });
      result += "{" + entries.join(", ") + "}";
      i = closeIdx + 1;
      continue;
    }
    if (s[i] === "{") {
      const closeIdx = findMatchingCloseEx(s, i, "{", "}");
      const inner = s.slice(i + 1, closeIdx).trim();
      const items = splitByCommaEx(inner);
      if (items.length >= 2 || (items.length === 1 && inner !== "")) {
        result += `__tuple([${items.map((x) => transpileExpr(x.trim())).join(", ")}])`;
        i = closeIdx + 1;
        continue;
      }
    }
    if (s[i] === "[") {
      const closeIdx = findMatchingCloseEx(s, i, "[", "]");
      const inner = s.slice(i + 1, closeIdx).trim();
      if (!inner) { result += "[]"; i = closeIdx + 1; continue; }
      const pipeIdx = findTopLevelPipe(inner);
      if (pipeIdx !== -1) {
        const headItems = splitByCommaEx(inner.slice(0, pipeIdx).trim()).map((x) => transpileExpr(x.trim()));
        result += `[${headItems.join(", ")}, ...${transpileExpr(inner.slice(pipeIdx + 1).trim())}]`;
        i = closeIdx + 1;
        continue;
      }
      result += "[" + splitByCommaEx(inner).map((x) => transpileExpr(x.trim())).join(", ") + "]";
      i = closeIdx + 1;
      continue;
    }
    if (s.slice(i, i + 3) === "fn ") {
      const endIdx = findKeywordEnd(s, i);
      const fnBody = s.slice(i + 3, endIdx).trim();
      const arrowIdx = fnBody.indexOf(" -> ");
      if (arrowIdx !== -1) {
        const params = fnBody.slice(0, arrowIdx).trim();
        const body = fnBody.slice(arrowIdx + 4).trim();
        result += `function(${params}) { return ${transpileExpr(body)}; }`;
      } else {
        result += `function() {}`;
      }
      i = endIdx + 3;
      continue;
    }
    if (s[i] === "&") {
      if (s[i + 1] === "(") {
        const closeIdx = findMatchingCloseEx(s, i + 1, "(", ")");
        const inner = s.slice(i + 2, closeIdx);
        const refs = new Set<string>();
        inner.replace(/&(\d+)/g, (_, n) => { refs.add(n); return ""; });
        const maxN = refs.size > 0 ? Math.max(...[...refs].map(Number)) : 1;
        const params = Array.from({ length: maxN }, (_, k) => `__a${k + 1}`).join(", ");
        const body = inner.replace(/&(\d+)/g, (_, n) => `__a${n}`);
        result += `function(${params}) { return ${transpileExpr(body)}; }`;
        i = closeIdx + 1;
        continue;
      }
      let j = i + 1;
      while (j < s.length && /[a-zA-Z0-9_.?!\/]/.test(s[j])) j++;
      const capture = s.slice(i + 1, j);
      const slashIdx = capture.lastIndexOf("/");
      if (slashIdx !== -1) {
        const funcRef = capture.slice(0, slashIdx);
        const arity = parseInt(capture.slice(slashIdx + 1));
        const params = Array.from({ length: arity }, (_, k) => `__c${k}`).join(", ");
        const dotIdx = funcRef.lastIndexOf(".");
        if (dotIdx !== -1) {
          const mod = funcRef.slice(0, dotIdx);
          const fn = funcRef.slice(dotIdx + 1);
          result += `function(${params}) { return ${mod}.${fn}(${params}); }`;
        } else {
          result += `function(${params}) { return ${funcRef}(${params}); }`;
        }
        i = j;
        continue;
      }
    }
    if (s.slice(i, i + 8) === "IO.puts(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__puts("; i += 8; continue;
    }
    if (s.slice(i, i + 11) === "IO.inspect(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "IO.inspect("; i += 11; continue;
    }
    if (s.slice(i, i + 12) === "Float.round(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__floatRound("; i += 12; continue;
    }
    const em = s.slice(i).match(/^Enum\.(\w+)\(/);
    if (em && (i === 0 || /\W/.test(s[i - 1]))) { result += "Enum." + em[1] + "("; i += em[0].length; continue; }
    const sm = s.slice(i).match(/^String\.(\w+)\(/);
    if (sm && (i === 0 || /\W/.test(s[i - 1]))) { result += "String." + sm[1] + "("; i += sm[0].length; continue; }
    const fm = s.slice(i).match(/^Float\.(\w+)\(/);
    if (fm && (i === 0 || /\W/.test(s[i - 1]))) { result += "Float." + fm[1] + "("; i += fm[0].length; continue; }
    const mm = s.slice(i).match(/^Map\.(\w+)\(/);
    if (mm && (i === 0 || /\W/.test(s[i - 1]))) { result += "Map." + mm[1] + "("; i += mm[0].length; continue; }
    const lm = s.slice(i).match(/^List\.(\w+)\(/);
    if (lm && (i === 0 || /\W/.test(s[i - 1]))) { result += "List." + lm[1] + "("; i += lm[0].length; continue; }
    const im = s.slice(i).match(/^Integer\.(\w+)\(/);
    if (im && (i === 0 || /\W/.test(s[i - 1]))) { result += "Integer." + im[1] + "("; i += im[0].length; continue; }
    if (s[i] === "." && s[i + 1] === "(") { result += "("; i += 2; continue; }
    if (s[i] === "<" && s[i + 1] === ">") { result += " + "; i += 2; continue; }
    if (s.slice(i, i + 4) === "not ") { result += "!"; i += 4; continue; }
    if (s.slice(i, i + 5) === " and ") { result += " && "; i += 5; continue; }
    if (s.slice(i, i + 4) === " or ") { result += " || "; i += 4; continue; }
    if (s[i] === "=" && s[i + 1] === "=") { result += "==="; i += 2; continue; }
    if (s[i] === "!" && s[i + 1] === "=") { result += "!=="; i += 2; continue; }
    if (s[i] === "+" && s[i + 1] === "+") {
      // list concat: left ++ right -> [...left, ...right]
      // This is tricky inline; just use .concat for now
      result += "].concat([";
      i += 2;
      // We'll need to fix the bracket context - this requires re-thinking.
      // Actually for inline ++ let's just do a simple approach:
      // rewrite result to wrap in concat
      const prevResult = result.slice(0, result.length - "].concat([".length);
      const afterConcat = s.slice(i).trim();
      // find end of afterConcat at top level
      result = prevResult;
      result += "].concat(";
      i += 0;
      continue;
    }
    result += s[i];
    i++;
  }
  return result;
}

// Find "end" keyword that closes a "fn ... end" block
function findKeywordEnd(s: string, fnStart: number): number {
  // Simple: find "end" not inside strings after fnStart
  let depth = 0;
  let inStr = false;
  let i = fnStart + 3; // skip "fn "
  while (i < s.length - 2) {
    const ch = s[i];
    if (ch === '"' && !inStr) { inStr = true; i++; continue; }
    if (ch === '"' && inStr) { inStr = false; i++; continue; }
    if (inStr) { i++; continue; }
    if (s.slice(i, i + 4) === " do " || s.slice(i, i + 3) === " do") depth++;
    if (s.slice(i, i + 3) === "end") {
      if (depth === 0) return i;
      depth--;
    }
    i++;
  }
  return s.length - 3;
}

// ---------------------------------------------------------------------------
// case expression: case expr do pat -> body; ... end
// ---------------------------------------------------------------------------

function buildCaseIife(subj: string, arms: Array<{ pat: string; body: string }>): string {
  const lines: string[] = [];
  lines.push(`(() => { const __s = ${transpileExpr(subj)};`);
  for (const arm of arms) {
    const { test, bindings } = compilePat(arm.pat, "__s");
    const bindStr = bindings.join(" ");
    const bodyJs = transpileExpr(arm.body.trim());
    if (test === "true") {
      lines.push(`  { ${bindStr} return ${bodyJs}; }`);
    } else {
      lines.push(`  if (${test}) { ${bindStr} return ${bodyJs}; }`);
    }
  }
  lines.push(`  throw new Error("no case match"); })()`);
  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// cond expression
// ---------------------------------------------------------------------------

function buildCondIife(arms: Array<{ cond: string; body: string }>): string {
  const lines: string[] = ["(() => {"];
  for (const arm of arms) {
    if (arm.cond === "true") {
      lines.push(`  return ${transpileExpr(arm.body)};`);
    } else {
      lines.push(`  if (${transpileExpr(arm.cond)}) return ${transpileExpr(arm.body)};`);
    }
  }
  lines.push(`  throw new Error("no cond match"); })()`);
  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// collect block body (lines between do and matching end)
// ---------------------------------------------------------------------------

function collectBlockBody(lines: string[], startIdx: number): { bodyLines: string[]; next: number } {
  const bodyLines: string[] = [];
  let depth = 0;
  // Count do/end on startLine
  const startLine = lines[startIdx].trim();
  if (/ do$/.test(startLine) || startLine.endsWith(" do")) depth = 1;
  else {
    // inline: def name(x), do: expr — no block
    return { bodyLines: [], next: startIdx + 1 };
  }

  let i = startIdx + 1;
  while (i < lines.length) {
    const t = lines[i].trim();
    if (!t) { bodyLines.push(""); i++; continue; }

    const doCount = countDoKeywords(t);
    const endCount = countEndKeywords(t);
    depth += doCount - endCount;

    if (depth <= 0) {
      i++;
      break;
    }
    bodyLines.push(lines[i]);
    i++;
  }
  return { bodyLines, next: i };
}

function countDoKeywords(s: string): number {
  // Count `do` at end of line (function heads, if/case/cond)
  let count = 0;
  // standalone 'do' at end or ' do ' in middle
  if (/ do$/.test(s)) count++;
  // defmodule/def/if/case/cond that have block do
  return count;
}

function countEndKeywords(s: string): number {
  if (s === "end" || s.startsWith("end ")) return 1;
  return 0;
}

// ---------------------------------------------------------------------------
// defmodule collector
// ---------------------------------------------------------------------------

interface DefClause {
  params: string;
  body: string;
  isInline: boolean;
}

function collectDefModule(lines: string[], startIdx: number): { code: string; next: number } {
  const startLine = lines[startIdx].trim();
  const modMatch = startLine.match(/^defmodule\s+(\w+)\s+do$/);
  if (!modMatch) return { code: "// unhandled defmodule", next: startIdx + 1 };
  const modName = modMatch[1];

  // Collect all lines until matching end
  let depth = 1;
  let i = startIdx + 1;
  const modLines: string[] = [];
  while (i < lines.length && depth > 0) {
    const t = lines[i].trim();
    if (/ do$/.test(t) || t === "do") depth++;
    if (t === "end" || t.startsWith("end ")) depth--;
    if (depth > 0) modLines.push(lines[i]);
    i++;
  }

  // Group function clauses by name
  const funcClauses = new Map<string, DefClause[]>();
  const funcOrder: string[] = [];

  let j = 0;
  while (j < modLines.length) {
    const t = modLines[j].trim();

    // def name(params) do ... end (multi-line)
    const defBlockMatch = t.match(/^def\s+(\w+)\s*\(([^)]*)\)\s+do$/);
    if (defBlockMatch) {
      const [, fnName, params] = defBlockMatch;
      const { bodyLines, next } = collectBlockBody(modLines, j);
      const bodyCode = transpileBody(bodyLines);
      if (!funcClauses.has(fnName)) { funcClauses.set(fnName, []); funcOrder.push(fnName); }
      funcClauses.get(fnName)!.push({ params, body: bodyCode, isInline: false });
      j = next;
      continue;
    }

    // def name(params), do: expr
    const defInlineMatch = t.match(/^def\s+(\w+)\s*\(([^)]*)\),\s*do:\s*(.+)$/);
    if (defInlineMatch) {
      const [, fnName, params, bodyExpr] = defInlineMatch;
      if (!funcClauses.has(fnName)) { funcClauses.set(fnName, []); funcOrder.push(fnName); }
      funcClauses.get(fnName)!.push({ params, body: `return ${transpileExpr(bodyExpr)};`, isInline: true });
      j++;
      continue;
    }

    // def name(params) (no body on same line but 'do' is actually block)
    // This covers: def name(params) do with block on next lines
    j++;
  }

  // Generate JS
  const fnDefs: string[] = [];
  for (const fnName of funcOrder) {
    const clauses = funcClauses.get(fnName)!;
    if (clauses.length === 1 && !hasPatternParams(clauses[0].params)) {
      // Simple function
      const jsParams = transpileParamList(clauses[0].params);
      fnDefs.push(`function ${fnName}(${jsParams}) {\n${clauses[0].body}\n}`);
    } else {
      // Multi-head dispatch
      const argVars = Array.from({ length: maxArity(clauses) }, (_, k) => `__a${k}`).join(", ");
      const dispatchLines: string[] = [`function ${fnName}(...__args) {`];
      for (const clause of clauses) {
        const paramList = splitByCommaEx(clause.params);
        const testParts: string[] = [];
        const bindParts: string[] = [];
        for (let k = 0; k < paramList.length; k++) {
          const pr = compilePat(paramList[k].trim(), `__args[${k}]`);
          if (pr.test !== "true") testParts.push(pr.test);
          bindParts.push(...pr.bindings);
        }
        const testStr = testParts.length > 0 ? testParts.join(" && ") : "true";
        dispatchLines.push(`  if (${testStr}) { ${bindParts.join(" ")} ${clause.body} }`);
      }
      dispatchLines.push(`  throw new Error("no match for ${fnName}");`);
      dispatchLines.push(`}`);
      fnDefs.push(dispatchLines.join("\n"));
    }
  }

  const moduleCode = `const ${modName} = (() => {\n${fnDefs.join("\n")}\nreturn { ${funcOrder.join(", ")} };\n})();`;
  return { code: moduleCode, next: i };
}

function hasPatternParams(params: string): boolean {
  if (!params.trim()) return false;
  return splitByCommaEx(params).some((p) => {
    const t = p.trim();
    return (
      t.startsWith(":") ||
      t.startsWith("{") ||
      t.startsWith("[") ||
      /^-?\d/.test(t) ||
      t === "_" ||
      t === "true" ||
      t === "false" ||
      t === "nil"
    );
  });
}

function maxArity(clauses: DefClause[]): number {
  return Math.max(...clauses.map((c) => splitByCommaEx(c.params).length));
}

function transpileParamList(params: string): string {
  if (!params.trim()) return "";
  return splitByCommaEx(params)
    .map((p) => {
      const t = p.trim();
      // default value: param \\ default
      const defaultIdx = t.indexOf(" \\\\ ");
      if (defaultIdx !== -1) {
        const name = t.slice(0, defaultIdx).trim();
        const def = transpileExpr(t.slice(defaultIdx + 4).trim());
        return `${name} = ${def}`;
      }
      return t;
    })
    .join(", ");
}

// ---------------------------------------------------------------------------
// Top-level body transpiler (handles if/case/cond blocks)
// ---------------------------------------------------------------------------

function transpileBody(lines: string[]): string {
  const result: string[] = [];
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const s = line.trim();
    const indent = line.match(/^(\s*)/)?.[1] ?? "";

    if (!s) { i++; continue; }

    // case expr do
    const caseMatch = s.match(/^case\s+(.+)\s+do$/);
    if (caseMatch) {
      const subj = caseMatch[1];
      const arms = collectCaseArms(lines, i + 1);
      // find next after end
      const afterEnd = findMatchingEnd(lines, i);
      result.push(indent + buildCaseIife(subj, arms));
      i = afterEnd;
      continue;
    }

    // x = case expr do
    const caseAssignMatch = s.match(/^(\w+)\s*=\s*case\s+(.+)\s+do$/);
    if (caseAssignMatch) {
      const [, varName, subj] = caseAssignMatch;
      const arms = collectCaseArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      result.push(indent + `const ${varName} = ${buildCaseIife(subj, arms)};`);
      i = afterEnd;
      continue;
    }

    // cond do
    if (s === "cond do") {
      const arms = collectCondArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      result.push(indent + buildCondIife(arms));
      i = afterEnd;
      continue;
    }

    // x = cond do
    const condAssignMatch = s.match(/^(\w+)\s*=\s*cond\s+do$/);
    if (condAssignMatch) {
      const arms = collectCondArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      result.push(indent + `const ${condAssignMatch[1]} = ${buildCondIife(arms)};`);
      i = afterEnd;
      continue;
    }

    // if expr do ... else ... end (multi-line)
    const ifDoMatch = s.match(/^if\s+(.+)\s+do$/);
    if (ifDoMatch) {
      const cond = ifDoMatch[1];
      const { thenLines, elseLines, afterEnd } = collectIfElse(lines, i);
      const thenCode = transpileBody(thenLines);
      const elseCode = elseLines.length > 0 ? transpileBody(elseLines) : "";
      result.push(indent + `if (${transpileExpr(cond)}) {\n${thenCode}\n${indent}}`);
      if (elseCode) result.push(` else {\n${elseCode}\n${indent}}`);
      i = afterEnd;
      continue;
    }

    // if expr, do: a, else: b (single line)
    const ifInlineMatch = s.match(/^if\s+(.+),\s*do:\s*(.+?)(?:,\s*else:\s*(.+))?$/);
    if (ifInlineMatch) {
      const [, cond, thenExpr, elseExpr] = ifInlineMatch;
      if (elseExpr) {
        result.push(indent + `(${transpileExpr(cond)} ? ${transpileExpr(thenExpr)} : ${transpileExpr(elseExpr)})`);
      } else {
        result.push(indent + `if (${transpileExpr(cond)}) { ${transpileExpr(thenExpr)}; }`);
      }
      i++;
      continue;
    }

    // Pattern match assignment: {a, b} = expr or [h | t] = expr
    const patAssignMatch = s.match(/^([{[].+?)\s*=\s*(.+)$/);
    if (patAssignMatch && (s[0] === "{" || s[0] === "[")) {
      const [, pat, rhs] = patAssignMatch;
      const rval = transpileExpr(rhs);
      const pr = compilePat(pat, "__pm");
      result.push(indent + `const __pm = ${rval}; ${pr.bindings.join(" ")}`);
      i++;
      continue;
    }

    // Simple variable assignment: x = expr
    const assignMatch = s.match(/^([a-z_][a-zA-Z0-9_]*)\s*=\s*(.+)$/);
    if (assignMatch && !["if", "case", "cond", "def", "defmodule", "do", "end"].includes(assignMatch[1])) {
      result.push(indent + `const ${assignMatch[1]} = ${transpileExpr(assignMatch[2])};`);
      i++;
      continue;
    }

    // return expr
    if (s.startsWith("return ")) {
      result.push(indent + `return ${transpileExpr(s.slice(7))};`);
      i++;
      continue;
    }

    // IO.puts / IO.inspect
    if (s.startsWith("IO.puts(") || s.startsWith("IO.inspect(")) {
      result.push(indent + transpileExpr(s) + ";");
      i++;
      continue;
    }

    // Bare expression (function call or expression statement)
    result.push(indent + `return ${transpileExpr(s)};`);
    i++;
    continue;
  }

  return result.join("\n");
}

function collectCaseArms(lines: string[], startIdx: number): Array<{ pat: string; body: string }> {
  const arms: Array<{ pat: string; body: string }> = [];
  let i = startIdx;
  while (i < lines.length) {
    const t = lines[i].trim();
    if (t === "end" || t.startsWith("end ")) break;
    if (!t) { i++; continue; }
    const arrowIdx = t.indexOf(" -> ");
    if (arrowIdx !== -1) {
      const pat = t.slice(0, arrowIdx).trim();
      const body = t.slice(arrowIdx + 4).trim();
      arms.push({ pat, body });
    }
    i++;
  }
  return arms;
}

function collectCondArms(lines: string[], startIdx: number): Array<{ cond: string; body: string }> {
  const arms: Array<{ cond: string; body: string }> = [];
  let i = startIdx;
  while (i < lines.length) {
    const t = lines[i].trim();
    if (t === "end" || t.startsWith("end ")) break;
    if (!t) { i++; continue; }
    const arrowIdx = t.indexOf(" -> ");
    if (arrowIdx !== -1) {
      arms.push({ cond: t.slice(0, arrowIdx).trim(), body: t.slice(arrowIdx + 4).trim() });
    }
    i++;
  }
  return arms;
}

function collectIfElse(lines: string[], startIdx: number): { thenLines: string[]; elseLines: string[]; afterEnd: number } {
  const thenLines: string[] = [];
  const elseLines: string[] = [];
  let depth = 1;
  let inElse = false;
  let i = startIdx + 1;

  while (i < lines.length) {
    const t = lines[i].trim();
    if (/ do$/.test(t) || t === "do") depth++;
    if (t === "end" || t.startsWith("end ")) {
      depth--;
      if (depth === 0) { i++; break; }
    }
    if (depth === 1 && t === "else") { inElse = true; i++; continue; }
    if (!inElse) thenLines.push(lines[i]);
    else elseLines.push(lines[i]);
    i++;
  }
  return { thenLines, elseLines, afterEnd: i };
}

function findMatchingEnd(lines: string[], startIdx: number): number {
  let depth = 0;
  for (let i = startIdx; i < lines.length; i++) {
    const t = lines[i].trim();
    if (/ do$/.test(t) || t === "do") depth++;
    if (t === "end" || t.startsWith("end ")) {
      depth--;
      if (depth === 0) return i + 1;
    }
  }
  return lines.length;
}

// ---------------------------------------------------------------------------
// Main transpiler
// ---------------------------------------------------------------------------

let _pmCounter = 0;

export function transpileElixir(code: string): string {
  _pmCounter = 0;
  code = stripElixirComments(code);

  // Join multi-line pipe chains
  code = joinPipeLines(code);

  const lines = code.split("\n");
  const output: string[] = [PRELUDE];

  let i = 0;
  while (i < lines.length) {
    const line = lines[i];
    const s = line.trim();

    if (!s) { i++; continue; }

    // defmodule
    if (/^defmodule\s+\w+\s+do$/.test(s)) {
      const { code: modCode, next } = collectDefModule(lines, i);
      output.push(modCode);
      i = next;
      continue;
    }

    // def at top-level (outside module)
    const defBlockMatch = s.match(/^def\s+(\w+)\s*\(([^)]*)\)\s+do$/);
    if (defBlockMatch) {
      const [, fnName, params] = defBlockMatch;
      const { bodyLines, next } = collectBlockBody(lines, i);
      const bodyCode = transpileBody(bodyLines);
      const jsParams = transpileParamList(params);
      output.push(`function ${fnName}(${jsParams}) {\n${bodyCode}\n}`);
      i = next;
      continue;
    }

    const defInlineMatch = s.match(/^def\s+(\w+)\s*\(([^)]*)\),\s*do:\s*(.+)$/);
    if (defInlineMatch) {
      const [, fnName, params, bodyExpr] = defInlineMatch;
      const jsParams = transpileParamList(params);
      output.push(`function ${fnName}(${jsParams}) { return ${transpileExpr(bodyExpr)}; }`);
      i++;
      continue;
    }

    // case expr do (top-level)
    const caseMatch = s.match(/^case\s+(.+)\s+do$/);
    if (caseMatch) {
      const subj = caseMatch[1];
      const arms = collectCaseArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      output.push(buildCaseIife(subj, arms) + ";");
      i = afterEnd;
      continue;
    }

    // cond do (top-level)
    if (s === "cond do") {
      const arms = collectCondArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      output.push(buildCondIife(arms) + ";");
      i = afterEnd;
      continue;
    }

    // if expr do (top-level)
    const ifDoMatch = s.match(/^if\s+(.+)\s+do$/);
    if (ifDoMatch) {
      const cond = ifDoMatch[1];
      const { thenLines, elseLines, afterEnd } = collectIfElse(lines, i);
      const thenCode = transpileBody(thenLines);
      const elseCode = elseLines.length > 0 ? transpileBody(elseLines) : "";
      output.push(`if (${transpileExpr(cond)}) {\n${thenCode}\n}`);
      if (elseCode) output.push(`else {\n${elseCode}\n}`);
      i = afterEnd;
      continue;
    }

    // if inline
    const ifInlineMatch = s.match(/^if\s+(.+),\s*do:\s*(.+?)(?:,\s*else:\s*(.+))?$/);
    if (ifInlineMatch) {
      const [, cond, thenExpr, elseExpr] = ifInlineMatch;
      if (elseExpr) {
        output.push(`(${transpileExpr(cond)} ? ${transpileExpr(thenExpr)} : ${transpileExpr(elseExpr)});`);
      } else {
        output.push(`if (${transpileExpr(cond)}) { ${transpileExpr(thenExpr)}; }`);
      }
      i++;
      continue;
    }

    // var = case expr do
    const caseAssignMatch = s.match(/^([a-z_]\w*)\s*=\s*case\s+(.+)\s+do$/);
    if (caseAssignMatch) {
      const [, varName, subj] = caseAssignMatch;
      const arms = collectCaseArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      output.push(`const ${varName} = ${buildCaseIife(subj, arms)};`);
      i = afterEnd;
      continue;
    }

    // var = cond do
    const condAssignMatch = s.match(/^([a-z_]\w*)\s*=\s*cond\s+do$/);
    if (condAssignMatch) {
      const varName = condAssignMatch[1];
      const arms = collectCondArms(lines, i + 1);
      const afterEnd = findMatchingEnd(lines, i);
      output.push(`const ${varName} = ${buildCondIife(arms)};`);
      i = afterEnd;
      continue;
    }

    // var = if expr do
    const ifAssignMatch = s.match(/^([a-z_]\w*)\s*=\s*if\s+(.+)\s+do$/);
    if (ifAssignMatch) {
      const [, varName, cond] = ifAssignMatch;
      const { thenLines, elseLines, afterEnd } = collectIfElse(lines, i);
      const thenCode = transpileBody(thenLines);
      const elseCode = elseLines.length > 0 ? transpileBody(elseLines) : "return null;";
      output.push(`const ${varName} = (() => { if (${transpileExpr(cond)}) {\n${thenCode}\n} else {\n${elseCode}\n} })();`);
      i = afterEnd;
      continue;
    }

    // Pattern match: {a,b} = expr  or  [h|t] = expr
    const patAssignMatch = s.match(/^([{[].+?)\s*=\s*(.+)$/);
    if (patAssignMatch && (s[0] === "{" || s[0] === "[")) {
      const [, pat, rhs] = patAssignMatch;
      const rval = transpileExpr(rhs);
      const pmVar = `__pm${_pmCounter++}`;
      const pr = compilePat(pat, pmVar);
      output.push(`const ${pmVar} = ${rval}; ${pr.bindings.join(" ")}`);
      i++;
      continue;
    }

    // Variable assignment: x = expr
    const assignMatch = s.match(/^([a-z_][a-zA-Z0-9_]*)\s*=\s*(.+)$/);
    if (assignMatch && !["if", "case", "cond", "def", "defmodule"].includes(assignMatch[1])) {
      output.push(`const ${assignMatch[1]} = ${transpileExpr(assignMatch[2])};`);
      i++;
      continue;
    }

    // IO.puts / IO.inspect (expression statement)
    if (s.startsWith("IO.puts(") || s.startsWith("IO.inspect(")) {
      output.push(transpileExpr(s) + ";");
      i++;
      continue;
    }

    // Generic expression statement
    output.push(transpileExpr(s) + ";");
    i++;
  }

  return output.join("\n");
}

function joinPipeLines(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let i = 0;
  while (i < lines.length) {
    let line = lines[i];
    // If next line starts with |>, join
    while (i + 1 < lines.length && lines[i + 1].trim().startsWith("|>")) {
      line = line.trimEnd() + " " + lines[i + 1].trim();
      i++;
    }
    result.push(line);
    i++;
  }
  return result.join("\n");
}

// ---------------------------------------------------------------------------
// extractElixirDefs - returns raw Elixir source of defmodule/def blocks
// ---------------------------------------------------------------------------

export function extractElixirDefs(code: string): string {
  code = stripElixirComments(code);
  const lines = code.split("\n");
  const result: string[] = [];
  let i = 0;

  while (i < lines.length) {
    const s = lines[i].trim();

    if (/^defmodule\s+\w+\s+do$/.test(s)) {
      result.push(lines[i]);
      let depth = 1;
      i++;
      while (i < lines.length && depth > 0) {
        const t = lines[i].trim();
        if (/ do$/.test(t) || t === "do") depth++;
        if (t === "end" || t.startsWith("end ")) depth--;
        result.push(lines[i]);
        i++;
      }
      continue;
    }

    if (/^def\s+\w+/.test(s)) {
      result.push(lines[i]);
      if (/ do$/.test(s)) {
        let depth = 1;
        i++;
        while (i < lines.length && depth > 0) {
          const t = lines[i].trim();
          if (/ do$/.test(t) || t === "do") depth++;
          if (t === "end" || t.startsWith("end ")) depth--;
          result.push(lines[i]);
          i++;
        }
      } else {
        i++;
      }
      continue;
    }

    i++;
  }

  return result.join("\n");
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export async function runElixir(code: string): Promise<RunResult> {
  try {
    const js = transpileElixir(code);
    const fn = new Function(js + "\nreturn __output.join('');");
    const output = fn() as string;
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
      const defs = extractElixirDefs(code);
      codeToRun = test.code.replace("{{FUNC}}", defs);
    }

    const result = await runElixir(codeToRun);
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
