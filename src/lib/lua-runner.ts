import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let luaReady = false;

export function isLuaReady(): boolean {
  return luaReady;
}

export async function initLuaRunner(): Promise<void> {
  luaReady = true;
}

// ---------------------------------------------------------------------------
// Lua → JavaScript transpiler
// Handles the subset of Lua used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];

function __luaType(v) {
  if (v === null || v === undefined) return 'nil';
  if (typeof v === 'number') return 'number';
  if (typeof v === 'string') return 'string';
  if (typeof v === 'boolean') return 'boolean';
  if (typeof v === 'function') return 'function';
  if (Array.isArray(v)) return 'table';
  if (typeof v === 'object') return 'table';
  return 'userdata';
}

function __luaPrint(...args) {
  const parts = args.map(v => {
    if (v === null || v === undefined) return 'nil';
    if (v === true) return 'true';
    if (v === false) return 'false';
    return String(v);
  });
  __output.push(parts.join('\\t') + '\\n');
}

function tostring(v) {
  if (v === null || v === undefined) return 'nil';
  if (v === true) return 'true';
  if (v === false) return 'false';
  return String(v);
}

function tonumber(v) {
  const n = Number(v);
  return isNaN(n) ? null : n;
}

function type(v) {
  return __luaType(v);
}

const string = {
  len: function(s) { return s.length; },
  sub: function(s, i, j) {
    if (j === undefined) j = s.length;
    return s.slice(i - 1, j);
  },
  upper: function(s) { return s.toUpperCase(); },
  lower: function(s) { return s.toLowerCase(); },
  rep: function(s, n) { return s.repeat(n); },
  find: function(s, pat) {
    const idx = s.indexOf(pat);
    if (idx === -1) return null;
    return idx + 1;
  },
  reverse: function(s) { return s.split('').reverse().join(''); },
  format: function(fmt, ...args) {
    let i = 0;
    return fmt.replace(/%%|%([+-]?\\d*\\.?\\d*)([diouxXeEfgGcs%])/g, function(match, flags, spec) {
      if (match === '%%') return '%';
      const arg = args[i++];
      switch (spec) {
        case 'd': case 'i': return Math.floor(Number(arg)).toString();
        case 'f': {
          const dotIdx = (flags || '').indexOf('.');
          if (dotIdx >= 0) {
            const prec = parseInt((flags || '').slice(dotIdx + 1)) || 6;
            return Number(arg).toFixed(prec);
          }
          return Number(arg).toFixed(6);
        }
        case 's': return String(arg);
        case 'x': return Math.floor(Number(arg)).toString(16);
        case 'X': return Math.floor(Number(arg)).toString(16).toUpperCase();
        case 'o': return Math.floor(Number(arg)).toString(8);
        default: return String(arg);
      }
    });
  },
  byte: function(s, i) { return s.charCodeAt((i || 1) - 1); },
  char: function(...args) { return String.fromCharCode(...args); },
};

const math = {
  floor: Math.floor,
  ceil: Math.ceil,
  abs: Math.abs,
  sqrt: Math.sqrt,
  max: Math.max,
  min: Math.min,
  pi: Math.PI,
  huge: Infinity,
  random: function(m, n) {
    if (m === undefined) return Math.random();
    if (n === undefined) return Math.floor(Math.random() * m) + 1;
    return Math.floor(Math.random() * (n - m + 1)) + m;
  },
  sin: Math.sin,
  cos: Math.cos,
  tan: Math.tan,
  log: Math.log,
  exp: Math.exp,
};

const table = {
  insert: function(t, pos, val) {
    if (val === undefined) { t.push(pos); }
    else { t.splice(pos - 1, 0, val); }
  },
  remove: function(t, pos) {
    if (pos === undefined) return t.pop();
    return t.splice(pos - 1, 1)[0];
  },
  concat: function(t, sep) { return t.join(sep || ''); },
  sort: function(t, comp) {
    if (comp) t.sort(comp);
    else t.sort(function(a, b) { return a < b ? -1 : a > b ? 1 : 0; });
  },
};

const io = {
  write: function(...args) {
    __output.push(args.map(v => String(v)).join(''));
  },
};

function ipairs(t) {
  let i = 0;
  return function() {
    i++;
    if (i <= t.length) return [i, t[i - 1]];
    return null;
  };
}

function pairs(t) {
  if (Array.isArray(t)) {
    let i = 0;
    return function() {
      i++;
      if (i <= t.length) return [i, t[i - 1]];
      return null;
    };
  }
  const keys = Object.keys(t);
  let i = 0;
  return function() {
    if (i < keys.length) {
      const k = keys[i++];
      return [k, t[k]];
    }
    return null;
  };
}

function unpack(t) { return t; }
function select(idx, ...args) {
  if (idx === '#') return args.length;
  return args[idx - 1];
}
function pcall(fn, ...args) {
  try { const r = fn(...args); return [true, r]; }
  catch (e) { return [false, String(e)]; }
}
function error(msg) { throw new Error(String(msg)); }
function assert(v, msg) { if (!v && v !== 0) throw new Error(msg || 'assertion failed!'); return v; }
function setmetatable(t, mt) {
  if (mt && mt.__index) {
    const handler = mt.__index;
    return new Proxy(t, {
      get(target, prop) {
        if (prop in target) return target[prop];
        if (typeof handler === 'function') return handler(target, prop);
        if (typeof handler === 'object' && handler !== null) return handler[prop];
        return undefined;
      }
    });
  }
  return t;
}
function getmetatable(t) { return null; }
`;

// ---------------------------------------------------------------------------
// Transpile Lua → JavaScript
// ---------------------------------------------------------------------------

function isInsideString(line: string, pos: number): boolean {
  let inSingle = false;
  let inDouble = false;
  for (let i = 0; i < pos; i++) {
    if (line[i] === "'" && !inDouble && (i === 0 || line[i - 1] !== "\\")) inSingle = !inSingle;
    if (line[i] === '"' && !inSingle && (i === 0 || line[i - 1] !== "\\")) inDouble = !inDouble;
  }
  return inSingle || inDouble;
}

function transformOperators(line: string): string {
  // Replace ~= with !== and .. with + (outside strings)
  let result = "";
  let i = 0;
  while (i < line.length) {
    if ((line[i] === '"' || line[i] === "'") && (i === 0 || line[i - 1] !== "\\")) {
      const quote = line[i];
      result += line[i++];
      while (i < line.length && !(line[i] === quote && line[i - 1] !== "\\")) {
        result += line[i++];
      }
      if (i < line.length) result += line[i++];
      continue;
    }
    if (line[i] === "~" && line[i + 1] === "=") {
      result += "!==";
      i += 2;
      continue;
    }
    // .. string concatenation → +
    if (line[i] === "." && line[i + 1] === "." && line[i + 2] !== ".") {
      result += " + ";
      i += 2;
      continue;
    }
    result += line[i++];
  }

  // Replace logical operators (word boundaries, outside strings)
  result = result.replace(/\bnot\s+/g, "!");
  result = result.replace(/\band\b/g, "&&");
  result = result.replace(/\bor\b/g, "||");

  // Replace nil with null
  result = result.replace(/\bnil\b/g, "null");

  return result;
}

function transformLengthOp(line: string): string {
  // #identifier or #(expr) → identifier.length or (expr).length
  return line.replace(/#(\w+)/g, "$1.length")
    .replace(/#\(([^)]+)\)/g, "($1).length");
}

function transformPrint(line: string): string {
  return line.replace(/\bprint\s*\(/g, "__luaPrint(");
}

// Transform Lua function parameters: handle ... → ...args
function transformParams(params: string): string {
  const p = params.trim();
  if (p === "...") return "...args";
  if (p.endsWith(", ...")) return p.slice(0, -4) + "...args";
  return p;
}

// Transform table constructor: {1, 2, 3} → [1, 2, 3], {key=val} → {key: val}
function transformTableLiteral(val: string): string {
  const trimmed = val.trim();
  if (!trimmed.startsWith("{") || !trimmed.endsWith("}")) return val;

  const inner = trimmed.slice(1, -1).trim();
  if (inner === "") return "{}";

  // Check if it's a list-style table (no key=val patterns)
  // Parse carefully to handle nested braces and strings
  const entries: string[] = [];
  let depth = 0;
  let current = "";
  let inStr: string | null = null;

  for (let i = 0; i < inner.length; i++) {
    const ch = inner[i];
    if (inStr) {
      current += ch;
      if (ch === inStr && inner[i - 1] !== "\\") inStr = null;
      continue;
    }
    if (ch === '"' || ch === "'") {
      inStr = ch;
      current += ch;
      continue;
    }
    if (ch === "{" || ch === "(") { depth++; current += ch; continue; }
    if (ch === "}" || ch === ")") { depth--; current += ch; continue; }
    if (ch === "," && depth === 0) {
      entries.push(current.trim());
      current = "";
      continue;
    }
    current += ch;
  }
  if (current.trim()) entries.push(current.trim());

  // Check if any entry has key = val pattern
  const hasKeys = entries.some(e => /^\w+\s*=/.test(e));

  if (hasKeys) {
    // Object-style: {key = val, ...} → {key: val, ...}
    const transformed = entries.map(e => {
      const m = e.match(/^(\w+)\s*=\s*(.+)$/);
      if (m) {
        return m[1] + ": " + transformTableLiteral(m[2].trim());
      }
      return e;
    });
    return "{" + transformed.join(", ") + "}";
  }

  // Array-style: {1, 2, 3} → [1, 2, 3]
  const transformed = entries.map(e => transformTableLiteral(e));
  return "[" + transformed.join(", ") + "]";
}

// Apply table literal transform to an expression containing table constructors
function transformTablesInExpr(expr: string): string {
  // Find top-level { ... } constructs outside strings
  let result = "";
  let i = 0;
  while (i < expr.length) {
    if ((expr[i] === '"' || expr[i] === "'") && (i === 0 || expr[i - 1] !== "\\")) {
      const quote = expr[i];
      result += expr[i++];
      while (i < expr.length && !(expr[i] === quote && expr[i - 1] !== "\\")) {
        result += expr[i++];
      }
      if (i < expr.length) result += expr[i++];
      continue;
    }
    if (expr[i] === "{") {
      // Find matching }
      let depth = 1;
      let j = i + 1;
      while (j < expr.length && depth > 0) {
        if (expr[j] === "{") depth++;
        else if (expr[j] === "}") depth--;
        j++;
      }
      const tableLit = expr.slice(i, j);
      result += transformTableLiteral(tableLit);
      i = j;
      continue;
    }
    result += expr[i++];
  }
  return result;
}

// Transform {...} (varargs to table) → [...args]
function transformVarargTable(expr: string): string {
  return expr.replace(/\{\s*\.\.\.\s*\}/g, "[...args]");
}

export function transpileLua(code: string): string {
  const lines = code.split("\n");
  const out: string[] = [PRELUDE];

  // Track block depth for end → }
  const blockStack: string[] = []; // "function", "if", "for", "while", "repeat", "do"
  let i = 0;

  while (i < lines.length) {
    const raw = lines[i];
    const trimmed = raw.trim();
    const indent = raw.match(/^(\s*)/)?.[1] ?? "";
    i++;

    // Blank lines
    if (trimmed === "") {
      out.push("");
      continue;
    }

    // Comments: -- → //
    if (trimmed.startsWith("--")) {
      out.push(indent + "//" + trimmed.slice(2));
      continue;
    }

    // Strip inline comments (but not inside strings)
    let codePart = trimmed;
    let commentPart = "";
    const dashIdx = trimmed.indexOf("--");
    if (dashIdx > 0 && !isInsideString(trimmed, dashIdx)) {
      codePart = trimmed.slice(0, dashIdx).trimEnd();
      commentPart = " //" + trimmed.slice(dashIdx + 2);
    }

    // --- repeat ... until ---
    if (/^repeat\b/.test(codePart)) {
      blockStack.push("repeat");
      out.push(indent + "do {" + commentPart);
      continue;
    }

    if (/^until\s+(.+)$/.test(codePart)) {
      const m = codePart.match(/^until\s+(.+)$/);
      if (m) {
        blockStack.pop();
        let cond = transformOperators(m[1]);
        cond = transformLengthOp(cond);
        cond = transformPrint(cond);
        out.push(indent + "} while (!(" + cond + "));" + commentPart);
      }
      continue;
    }

    // --- end ---
    if (codePart === "end") {
      const block = blockStack.pop();
      if (block === "for-pairs") {
        out.push(indent + "} }" + commentPart);
      } else {
        out.push(indent + "}" + commentPart);
      }
      continue;
    }

    // --- return function(params) --- (anonymous function in return)
    const returnFuncM = codePart.match(/^return\s+function\s*\(([^)]*)\)$/);
    if (returnFuncM) {
      const params = transformParams(returnFuncM[1]);
      blockStack.push("function");
      out.push(indent + "return function(" + params + ") {" + commentPart);
      continue;
    }

    // --- function declaration ---
    // function name(params)
    const funcM = codePart.match(/^function\s+(\w+)\s*\(([^)]*)\)$/);
    if (funcM) {
      const [, name, rawParams] = funcM;
      const params = transformParams(rawParams);
      blockStack.push("function");
      out.push(indent + "function " + name + "(" + params + ") {" + commentPart);
      continue;
    }

    // local function name(params)
    const localFuncM = codePart.match(/^local\s+function\s+(\w+)\s*\(([^)]*)\)$/);
    if (localFuncM) {
      const [, name, rawParams] = localFuncM;
      const params = transformParams(rawParams);
      blockStack.push("function");
      out.push(indent + "function " + name + "(" + params + ") {" + commentPart);
      continue;
    }

    // method syntax: function module.name(params) — treat as assignment
    const methodFuncM = codePart.match(/^function\s+(\w+)\.(\w+)\s*\(([^)]*)\)$/);
    if (methodFuncM) {
      const [, obj, name, rawParams] = methodFuncM;
      const params = transformParams(rawParams);
      blockStack.push("function");
      out.push(indent + obj + "." + name + " = function(" + params + ") {" + commentPart);
      continue;
    }

    // --- local variable with function value ---
    const localFuncValM = codePart.match(/^local\s+(\w+)\s*=\s*function\s*\(([^)]*)\)$/);
    if (localFuncValM) {
      const [, name, rawParams] = localFuncValM;
      const params = transformParams(rawParams);
      blockStack.push("function");
      out.push(indent + "let " + name + " = function(" + params + ") {" + commentPart);
      continue;
    }

    // --- if/elseif/else ---
    const ifM = codePart.match(/^if\s+(.+)\s+then$/);
    if (ifM) {
      let cond = transformOperators(ifM[1]);
      cond = transformLengthOp(cond);
      cond = transformPrint(cond);
      blockStack.push("if");
      out.push(indent + "if (" + cond + ") {" + commentPart);
      continue;
    }

    // single-line if: if cond then statement end
    const ifSingleM = codePart.match(/^if\s+(.+?)\s+then\s+(.+?)\s+end$/);
    if (ifSingleM) {
      let cond = transformOperators(ifSingleM[1]);
      cond = transformLengthOp(cond);
      cond = transformPrint(cond);
      let body = transformOperators(ifSingleM[2]);
      body = transformLengthOp(body);
      body = transformPrint(body);
      body = body.replace(/^return\s+/, "return ");
      out.push(indent + "if (" + cond + ") { " + body + "; }" + commentPart);
      continue;
    }

    const elseifM = codePart.match(/^elseif\s+(.+)\s+then$/);
    if (elseifM) {
      let cond = transformOperators(elseifM[1]);
      cond = transformLengthOp(cond);
      cond = transformPrint(cond);
      out.push(indent + "} else if (" + cond + ") {" + commentPart);
      continue;
    }

    if (codePart === "else") {
      out.push(indent + "} else {" + commentPart);
      continue;
    }

    // --- for i = start, stop[, step] do ---
    const forNumM = codePart.match(/^for\s+(\w+)\s*=\s*(.+?),\s*(.+?)(?:,\s*(.+?))?\s+do$/);
    if (forNumM) {
      const [, v, start, stop, step] = forNumM;
      blockStack.push("for");
      let startE = transformLengthOp(transformOperators(start.trim()));
      let stopE = transformLengthOp(transformOperators(stop.trim()));
      if (step) {
        let stepE = transformLengthOp(transformOperators(step.trim()));
        out.push(indent + "for (let " + v + " = " + startE + "; (" + stepE + " > 0 ? " + v + " <= " + stopE + " : " + v + " >= " + stopE + "); " + v + " += " + stepE + ") {" + commentPart);
      } else {
        out.push(indent + "for (let " + v + " = " + startE + "; " + v + " <= " + stopE + "; " + v + "++) {" + commentPart);
      }
      continue;
    }

    // --- for k, v in pairs(t) do ---
    const forPairsM = codePart.match(/^for\s+(\w+)\s*,\s*(\w+)\s+in\s+pairs\((\w+)\)\s+do$/);
    if (forPairsM) {
      const [, k, v, t] = forPairsM;
      blockStack.push("for-pairs");
      out.push(indent + "{ let __iter = pairs(" + t + "); let __kv; while ((__kv = __iter()) !== null) { let " + k + " = __kv[0]; let " + v + " = __kv[1];" + commentPart);
      continue;
    }

    // --- for i, v in ipairs(t) do ---
    const forIpairsM = codePart.match(/^for\s+(\w+)\s*,\s*(\w+)\s+in\s+ipairs\((\w+)\)\s+do$/);
    if (forIpairsM) {
      const [, idx, v, t] = forIpairsM;
      blockStack.push("for");
      out.push(indent + "for (let __i = 0; __i < " + t + ".length; __i++) { let " + idx + " = __i + 1; let " + v + " = " + t + "[__i];" + commentPart);
      continue;
    }

    // --- generic for with single var: for v in iterator do ---
    const forGenericM = codePart.match(/^for\s+(\w+)\s+in\s+(.+)\s+do$/);
    if (forGenericM) {
      const [, v, iterExpr] = forGenericM;
      blockStack.push("for");
      let expr = transformOperators(iterExpr);
      expr = transformLengthOp(expr);
      out.push(indent + "for (const " + v + " of " + expr + ") {" + commentPart);
      continue;
    }

    // --- while cond do ---
    const whileM = codePart.match(/^while\s+(.+)\s+do$/);
    if (whileM) {
      let cond = transformOperators(whileM[1]);
      cond = transformLengthOp(cond);
      cond = transformPrint(cond);
      blockStack.push("while");
      out.push(indent + "while (" + cond + ") {" + commentPart);
      continue;
    }

    // --- do block ---
    if (codePart === "do") {
      blockStack.push("do");
      out.push(indent + "{" + commentPart);
      continue;
    }

    // --- return with anonymous function ---
    // Already handled above (returnFuncM)

    // --- return ---
    const returnM = codePart.match(/^return\s+(.*)$/);
    if (returnM) {
      let expr = returnM[1];
      expr = transformVarargTable(expr);
      expr = transformTablesInExpr(expr);
      expr = transformOperators(expr);
      expr = transformLengthOp(expr);
      expr = transformPrint(expr);
      out.push(indent + "return " + expr + ";" + commentPart);
      continue;
    }

    if (codePart === "return") {
      out.push(indent + "return;" + commentPart);
      continue;
    }

    // --- local x = {...} (vararg capture) ---
    const localVarargM = codePart.match(/^local\s+(\w+)\s*=\s*\{\s*\.\.\.\s*\}$/);
    if (localVarargM) {
      const name = localVarargM[1];
      // If the param was already named ...args, avoid redeclaration
      if (name === "args") {
        // args is already available from ...args parameter
        out.push(indent + "// args = [...args] (already available)" + commentPart);
      } else {
        out.push(indent + "let " + name + " = [...args];" + commentPart);
      }
      continue;
    }

    // --- local x = val ---
    const localM = codePart.match(/^local\s+(\w+)\s*=\s*(.+)$/);
    if (localM) {
      let [, name, val] = localM;
      // Transform table literals BEFORE operators (to avoid .. → + in {..})
      val = transformVarargTable(val);
      val = transformTablesInExpr(val);
      val = transformOperators(val);
      val = transformLengthOp(val);
      val = transformPrint(val);
      out.push(indent + "let " + name + " = " + val + ";" + commentPart);
      continue;
    }

    // --- local x (no assignment) ---
    const localDeclM = codePart.match(/^local\s+(\w+)\s*$/);
    if (localDeclM) {
      out.push(indent + "let " + localDeclM[1] + " = null;" + commentPart);
      continue;
    }

    // --- local with multiple vars: local a, b = val1, val2 ---
    const localMultiM = codePart.match(/^local\s+(\w+(?:\s*,\s*\w+)+)\s*=\s*(.+)$/);
    if (localMultiM) {
      const names = localMultiM[1].split(",").map(n => n.trim());
      let vals = transformOperators(localMultiM[2]);
      vals = transformLengthOp(vals);
      vals = transformPrint(vals);
      const valParts = vals.split(",").map(v => v.trim());
      for (let j = 0; j < names.length; j++) {
        out.push(indent + "let " + names[j] + " = " + (valParts[j] || "null") + ";");
      }
      continue;
    }

    // --- General line: apply transforms ---
    let line = codePart;
    // Table literals must be transformed before operators (to avoid .. → + inside {..})
    line = transformVarargTable(line);
    line = transformTablesInExpr(line);
    line = transformOperators(line);
    line = transformLengthOp(line);
    line = transformPrint(line);

    // Assignment: x = val (no local)
    const assignM = line.match(/^(\w+(?:\.\w+)*(?:\[.+?\])?)\s*=\s*(.+)$/);
    if (assignM && !line.startsWith("if") && !line.startsWith("while") && !line.startsWith("for") && !line.startsWith("return") && !line.startsWith("function") && !line.startsWith("let ") && !line.includes("===") && !line.includes("!==")) {
      out.push(indent + line + ";" + commentPart);
      continue;
    }

    // Function calls as statements (no assignment)
    if (/^[\w.]+\s*\(/.test(line) || /^[\w.]+:\w+\s*\(/.test(line)) {
      out.push(indent + line + ";" + commentPart);
      continue;
    }

    out.push(indent + line + commentPart);
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Extract declarations from solution code for test isolation.
// Extracts: function blocks, local function blocks, module patterns
// (local X = {} followed by function X.method(...) ... end)
// ---------------------------------------------------------------------------

export function extractLuaDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inDecl = false;

  // First pass: collect module table names (local X = {})
  const moduleNames = new Set<string>();
  for (const line of lines) {
    const m = line.trim().match(/^local\s+(\w+)\s*=\s*\{\s*\}$/);
    if (m) moduleNames.add(m[1]);
  }

  for (let li = 0; li < lines.length; li++) {
    const line = lines[li];
    const trimmed = line.trim();

    if (!inDecl) {
      // function declarations (named, local, method)
      if (/^(?:local\s+)?function\s+/.test(trimmed) || /^function\s+\w+\.\w+/.test(trimmed)) {
        inDecl = true;
        depth = 1;
        result.push(line);
        continue;
      }
      // Module table declarations: local X = {}
      if (moduleNames.has(trimmed.match(/^local\s+(\w+)\s*=\s*\{\s*\}$/)?.[1] ?? "")) {
        result.push(line);
        continue;
      }
      // Module field assignments: X.field = value (single line, not a function)
      const modFieldM = trimmed.match(/^(\w+)\.(\w+)\s*=\s*(.+)$/);
      if (modFieldM && moduleNames.has(modFieldM[1]) && !modFieldM[3].match(/^function\b/)) {
        result.push(line);
        continue;
      }
      // Skip top-level executable statements
    } else {
      result.push(line);
      // Count block openers on this line
      const openers = (trimmed.match(/\b(?:function|if|for|while|repeat)\b/g) ?? []).length;
      // "do" as standalone can open a block, but "for...do" already counted
      if (trimmed === "do") depth++;
      else depth += openers;

      if (trimmed === "end") {
        depth--;
        if (depth <= 0) {
          inDecl = false;
          depth = 0;
        }
      }
    }
  }
  return result.join("\n").trim();
}

// ---------------------------------------------------------------------------
// Browser runner API
// ---------------------------------------------------------------------------

export async function runLua(code: string): Promise<RunResult> {
  try {
    const js = transpileLua(code);
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
      const decls = extractLuaDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }
    const result = await runLua(codeToRun);
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
