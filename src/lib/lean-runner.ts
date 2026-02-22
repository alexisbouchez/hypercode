import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let leanReady = false;

export function isLeanReady(): boolean {
  return leanReady;
}

export async function initLeanRunner(): Promise<void> {
  leanReady = true;
}

// ---------------------------------------------------------------------------
// Lean 4 Prelude — JavaScript implementations of Lean builtins
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];

const IO = {
  println: (s) => {
    __output.push(String(s === null || s === undefined ? "" : s) + "\\n");
    return { __isIO: true };
  },
  print: (s) => {
    __output.push(String(s === null || s === undefined ? "" : s));
    return { __isIO: true };
  },
};

function __show(v) {
  if (v === null || v === undefined) return "none";
  if (typeof v === "boolean") return v ? "true" : "false";
  if (typeof v === "number") return String(v);
  if (typeof v === "string") return JSON.stringify(v);
  if (Array.isArray(v)) return "[" + v.map(__show).join(", ") + "]";
  if (v.__isIO) return "";
  if (v.__type === "some") return "some " + __show(v.value);
  if (v.__type === "none") return "none";
  if (v.__type === "Tuple") return "(" + v.values.map(__show).join(", ") + ")";
  return String(v);
}

function __eval(fn) {
  try {
    const result = fn();
    if (result !== null && result !== undefined && !(result && result.__isIO)) {
      __output.push(__show(result) + "\\n");
    }
  } catch(e) {
    __output.push("error: " + (e.message || String(e)) + "\\n");
  }
}

function some(x) { return { __type: "some", value: x }; }
const none = { __type: "none" };

function not(b) { return !b; }
function id(x) { return x; }

function __append(a, b) {
  if (typeof a === "string" && typeof b === "string") return a + b;
  if (Array.isArray(a) && Array.isArray(b)) return [...a, ...b];
  return String(a) + String(b);
}

function __natDiv(a, b) { return b === 0 ? 0 : Math.trunc(a / b); }
function __natMod(a, b) { return b === 0 ? 0 : ((a % b) + Math.abs(b)) % Math.abs(b); }
function __pow(a, b) { return Math.pow(a, b); }
`;

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

function stripComments(code: string): string {
  code = code.replace(/\/\-[\s\S]*?\-\//g, "");
  code = code.replace(/--[^\n]*/g, "");
  return code;
}

function sanitizeIdent(name: string): string {
  return name.replace(/'/g, "_");
}

// Strip type annotation from param groups like "(x : T)" or "(x y : T)"
function stripParamGroup(param: string): string[] {
  param = param.trim();
  if (param.startsWith("(") && param.endsWith(")")) {
    param = param.slice(1, -1).trim();
  }
  const colonIdx = param.indexOf(":");
  if (colonIdx !== -1) {
    return param
      .slice(0, colonIdx)
      .trim()
      .split(/\s+/)
      .filter(Boolean)
      .map(sanitizeIdent);
  }
  return param.split(/\s+/).filter(Boolean).map(sanitizeIdent);
}

// Parse "(x : T) (y : T)" style parameter list into JS param names
function parseParams(paramStr: string): string[] {
  const params: string[] = [];
  let depth = 0;
  let current = "";

  for (let i = 0; i < paramStr.length; i++) {
    const ch = paramStr[i];
    if (ch === "(") {
      if (depth > 0) current += ch;
      depth++;
    } else if (ch === ")") {
      depth--;
      if (depth > 0) {
        current += ch;
      } else {
        params.push(...stripParamGroup(current.trim()));
        current = "";
      }
    } else if (ch === " " && depth === 0) {
      if (current.trim()) {
        // Plain identifier outside parens
        const t = current.trim();
        // Skip if it looks like a type name (starts uppercase) or is ':'
        if (!/^[A-Z]/.test(t) && t !== ":" && t !== "→") {
          params.push(sanitizeIdent(t));
        }
        current = "";
      }
    } else if (depth > 0) {
      current += ch;
    } else {
      current += ch;
    }
  }

  if (current.trim() && depth === 0) {
    const t = current.trim();
    if (!/^[A-Z]/.test(t) && t !== ":" && t !== "→") {
      params.push(sanitizeIdent(t));
    }
  }

  return params;
}

// ---------------------------------------------------------------------------
// Def block parser
// ---------------------------------------------------------------------------

interface DefParsed {
  name: string;
  params: string[];
  body: string;
  patterns: Array<{ pat: string; body: string }> | null;
}

function parseDef(block: string): DefParsed | null {
  const lines = block.split("\n").filter((l) => l.trim());
  if (!lines.length || !lines[0].trim().startsWith("def ")) return null;

  const firstLine = lines[0].trim();
  const afterDef = firstLine.slice(4).trim();

  const colonEqIdx = afterDef.indexOf(":=");
  const patternLines = lines.slice(1).filter((l) => l.trim().startsWith("|"));
  const hasMatchInCont = lines.slice(1).some((l) => l.trim().startsWith("match "));
  const hasPatterns = colonEqIdx === -1 && patternLines.length > 0;

  let headerPart: string;
  let bodyPart: string;

  if (colonEqIdx !== -1) {
    headerPart = afterDef.slice(0, colonEqIdx).trim();
    bodyPart = afterDef.slice(colonEqIdx + 2).trim();
    // Collect all continuation lines (including match/pipe lines)
    const contLines = lines.slice(1);
    if (contLines.length > 0) {
      bodyPart += " " + contLines.map((l) => l.trim()).join(" ");
    }
    void hasMatchInCont; // used above to distinguish from patterns
  } else {
    headerPart = afterDef.trim();
    bodyPart = "";
  }

  // Strip return type annotation from header (find last ':' at depth 0)
  let headerClean = headerPart;
  {
    let d = 0;
    let lastColon = -1;
    for (let i = 0; i < headerClean.length; i++) {
      const ch = headerClean[i];
      if (ch === "(") d++;
      else if (ch === ")") d--;
      else if (ch === ":" && d === 0) lastColon = i;
    }
    if (lastColon !== -1) {
      headerClean = headerClean.slice(0, lastColon).trim();
    }
  }

  const parts = headerClean.split(/\s+/);
  const name = sanitizeIdent(parts[0]);
  const paramStr = headerClean.slice(parts[0].length).trim();
  const params = paramStr ? parseParams(paramStr) : [];

  if (hasPatterns) {
    const patterns: Array<{ pat: string; body: string }> = [];
    for (const pLine of patternLines) {
      const t = pLine.trim().slice(1).trim();
      const arrowIdx = t.indexOf("=>");
      if (arrowIdx !== -1) {
        patterns.push({
          pat: t.slice(0, arrowIdx).trim(),
          body: t.slice(arrowIdx + 2).trim(),
        });
      }
    }
    return { name, params, body: bodyPart, patterns };
  }

  return { name, params, body: bodyPart, patterns: null };
}

// ---------------------------------------------------------------------------
// Expression transpiler
// ---------------------------------------------------------------------------

function transpileExpr(expr: string): string {
  expr = expr.trim();
  if (!expr) return "undefined";

  // Unary operators
  if (expr.startsWith("!") && !expr.startsWith("!=")) {
    return `!(${transpileExpr(expr.slice(1).trim())})`;
  }
  if (expr.startsWith("not ")) {
    return `!(${transpileExpr(expr.slice(4).trim())})`;
  }
  if (expr.startsWith("-") && /^-\d/.test(expr)) {
    return expr; // negative number literal
  }

  // if-then-else
  if (/^if\s/.test(expr)) {
    return transpileIfThenElse(expr);
  }

  // match expression
  if (/^match\s/.test(expr)) {
    return transpileMatch(expr);
  }

  // fun lambda
  if (/^fun\s/.test(expr)) {
    return transpileLambda(expr);
  }

  // let binding
  if (/^let\s/.test(expr)) {
    return transpileLet(expr);
  }

  // do block
  if (/^do(\s|$)/.test(expr)) {
    return transpileDoBlock(expr);
  }

  // List literal — only if opening [ closes at the very last character
  if (expr.startsWith("[") && expr.endsWith("]")) {
    let d = 0, s = false, matchEnd = -1;
    for (let i = 0; i < expr.length; i++) {
      const c = expr[i];
      if (c === "\\" && s) { i++; continue; }
      if (c === '"') { s = !s; continue; }
      if (s) continue;
      if (c === "[") d++;
      else if (c === "]") { d--; if (d === 0) { matchEnd = i; break; } }
    }
    if (matchEnd === expr.length - 1) {
      const inner = expr.slice(1, -1).trim();
      if (!inner) return "[]";
      const items = splitCommas(inner);
      return "[" + items.map(transpileExpr).join(", ") + "]";
    }
  }

  // Parenthesized / tuple — only if opening ( closes at the very last character
  if (expr.startsWith("(") && expr.endsWith(")")) {
    let d = 0, s = false, matchEnd = -1;
    for (let i = 0; i < expr.length; i++) {
      const c = expr[i];
      if (c === "\\" && s) { i++; continue; }
      if (c === '"') { s = !s; continue; }
      if (s) continue;
      if (c === "(") d++;
      else if (c === ")") { d--; if (d === 0) { matchEnd = i; break; } }
    }
    if (matchEnd === expr.length - 1) {
      const inner = expr.slice(1, -1).trim();
      if (!inner) return "null";
      const items = splitCommas(inner);
      if (items.length >= 2) {
        return (
          "{__type:\"Tuple\",values:[" + items.map(transpileExpr).join(", ") + "]}"
        );
      }
      return "(" + transpileExpr(inner) + ")";
    }
  }

  // String literal — only if the opening " closes at the very last character
  if (expr.startsWith('"')) {
    let i = 1;
    while (i < expr.length) {
      if (expr[i] === "\\") { i += 2; continue; }
      if (expr[i] === '"') break;
      i++;
    }
    if (i === expr.length - 1) return expr;
  }
  if (expr.startsWith("'")) {
    let i = 1;
    while (i < expr.length) {
      if (expr[i] === "\\") { i += 2; continue; }
      if (expr[i] === "'") break;
      i++;
    }
    if (i === expr.length - 1) return expr;
  }

  // Number literal
  if (/^-?\d+(\.\d+)?$/.test(expr)) return expr;

  // Boolean / Option literals
  if (expr === "true") return "true";
  if (expr === "false") return "false";
  if (expr === "none") return "none";

  // Binary operators
  const binOp = tryBinaryOp(expr);
  if (binOp) return binOp;

  // Dot notation (must come after binary ops to avoid catching :: or ++)
  const dotExpr = tryDotNotation(expr);
  if (dotExpr) return dotExpr;

  // Function application
  const appExpr = tryApplication(expr);
  if (appExpr) return appExpr;

  // Plain identifier
  return sanitizeIdent(expr);
}

function transpileIfThenElse(expr: string): string {
  const thenPos = findKeyword(expr, "then", 2);
  if (thenPos === -1) return `/* invalid if */ undefined`;
  const elsePos = findKeyword(expr, "else", thenPos + 4);
  if (elsePos === -1) return `/* invalid if */ undefined`;

  const cond = expr.slice(3, thenPos).trim();
  const thenExpr = expr.slice(thenPos + 4, elsePos).trim();
  const elseExpr = expr.slice(elsePos + 4).trim();

  return `(${transpileExpr(cond)} ? ${transpileExpr(thenExpr)} : ${transpileExpr(elseExpr)})`;
}

function findKeyword(str: string, kw: string, startFrom = 0): number {
  let depth = 0;
  let inStr = false;
  for (let i = startFrom; i <= str.length - kw.length; i++) {
    const ch = str[i];
    if (ch === '"' && !inStr) {
      inStr = true;
      continue;
    }
    if (ch === '"' && inStr) {
      inStr = false;
      continue;
    }
    if (inStr) continue;
    if (ch === "(" || ch === "[") {
      depth++;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth--;
      continue;
    }
    if (depth === 0 && str.slice(i, i + kw.length) === kw) {
      const before = i === 0 ? " " : str[i - 1];
      const after =
        i + kw.length >= str.length ? " " : str[i + kw.length];
      if (/\s/.test(before) && /[\s(]/.test(after)) return i;
    }
  }
  return -1;
}

function transpileLambda(expr: string): string {
  const afterFun = expr.slice(4).trim();
  const arrowIdx = afterFun.indexOf("=>");
  if (arrowIdx === -1) return `/* invalid lambda */ undefined`;

  const paramStr = afterFun.slice(0, arrowIdx).trim();
  const bodyStr = afterFun.slice(arrowIdx + 2).trim();
  const params = parseParams(paramStr);

  return `((${params.join(", ")}) => ${transpileExpr(bodyStr)})`;
}

function transpileLet(expr: string): string {
  const afterLet = expr.slice(4).trim();
  const colonEq = afterLet.indexOf(":=");
  if (colonEq === -1) return `/* invalid let */ undefined`;

  const namePart = afterLet.slice(0, colonEq).split(":")[0].trim();
  const name = sanitizeIdent(namePart);
  const afterAssign = afterLet.slice(colonEq + 2).trim();

  const semiIdx = afterAssign.indexOf(";");
  const nlIdx = afterAssign.indexOf("\n");
  let valueEnd = afterAssign.length;
  if (semiIdx !== -1) valueEnd = semiIdx;
  else if (nlIdx !== -1) valueEnd = nlIdx;

  const value = afterAssign.slice(0, valueEnd).trim();
  const rest = afterAssign.slice(valueEnd + 1).trim();

  if (rest) {
    return `(() => { const ${name} = ${transpileExpr(value)}; return ${transpileExpr(rest)}; })()`;
  }
  return `const ${name} = ${transpileExpr(value)}`;
}

function transpileDoBlock(expr: string): string {
  const body = expr.startsWith("do") ? expr.slice(2).trim() : expr;
  const lines = body.split("\n").map((l) => l.trim()).filter(Boolean);

  const stmts = lines.map((line) => {
    if (/^[a-zA-Z_][a-zA-Z0-9_]*\s*←/.test(line)) {
      const arrowIdx = line.indexOf("←");
      const varName = sanitizeIdent(line.slice(0, arrowIdx).trim());
      const rhs = line.slice(arrowIdx + 1).trim();
      return `let ${varName} = ${transpileExpr(rhs)};`;
    }
    if (line.startsWith("let ")) {
      const afterLet = line.slice(4).trim();
      const colonEq = afterLet.indexOf(":=");
      if (colonEq !== -1) {
        const namePart = afterLet.slice(0, colonEq).split(":")[0].trim();
        const valStr = afterLet.slice(colonEq + 2).trim();
        return `let ${sanitizeIdent(namePart)} = ${transpileExpr(valStr)};`;
      }
    }
    if (line.startsWith("return ")) {
      return `return ${transpileExpr(line.slice(7).trim())};`;
    }
    return `${transpileExpr(line)};`;
  });

  return `(() => { ${stmts.join(" ")} return { __isIO: true }; })()`;
}

function transpileMatch(expr: string): string {
  const withIdx = expr.indexOf(" with");
  if (withIdx === -1) return `/* invalid match */ undefined`;

  const scrutinee = transpileExpr(expr.slice(6, withIdx).trim());
  const afterWith = expr.slice(withIdx + 5).trim();
  const arms = splitPatternArms(afterWith);

  const branches = arms.map(({ pat, body }) =>
    transpilePatternBranch(pat, "__s", transpileExpr(body))
  );

  return `((__s) => { ${branches.join(" ")} return undefined; })(${scrutinee})`;
}

function splitPatternArms(str: string): Array<{ pat: string; body: string }> {
  const arms: Array<{ pat: string; body: string }> = [];
  const parts = str.split(/\s*\|\s*/).filter(Boolean);

  for (const part of parts) {
    const arrowIdx = part.indexOf("=>");
    if (arrowIdx !== -1) {
      arms.push({
        pat: part.slice(0, arrowIdx).trim(),
        body: part.slice(arrowIdx + 2).trim(),
      });
    }
  }
  return arms;
}

function transpilePatternBranch(
  pat: string,
  subject: string,
  bodyExpr: string
): string {
  if (pat === "_") return `else { return ${bodyExpr}; }`;
  if (pat === "[]") return `if (${subject}.length === 0) { return ${bodyExpr}; }`;
  if (/^\d+$/.test(pat)) return `if (${subject} === ${pat}) { return ${bodyExpr}; }`;
  if (pat === "true") return `if (${subject} === true) { return ${bodyExpr}; }`;
  if (pat === "false") return `if (${subject} === false) { return ${bodyExpr}; }`;
  if (pat === "none") {
    return `if (!${subject} || ${subject}.__type === "none") { return ${bodyExpr}; }`;
  }
  if (pat.startsWith("some ")) {
    const inner = sanitizeIdent(pat.slice(5).trim());
    return `if (${subject} && ${subject}.__type === "some") { const ${inner} = ${subject}.value; return ${bodyExpr}; }`;
  }
  if (pat.includes(" :: ")) {
    const colonIdx = pat.indexOf(" :: ");
    const head = sanitizeIdent(pat.slice(0, colonIdx).trim());
    const tail = sanitizeIdent(pat.slice(colonIdx + 4).trim());
    return `if (${subject}.length > 0) { const ${head} = ${subject}[0]; const ${tail} = ${subject}.slice(1); return ${bodyExpr}; }`;
  }
  if (pat.startsWith('"') || pat.startsWith("'")) {
    return `if (${subject} === ${pat}) { return ${bodyExpr}; }`;
  }
  // Nat successor: "n + 1"
  const succMatch = pat.match(/^([a-z_][a-zA-Z0-9_]*)\s*\+\s*1$/);
  if (succMatch) {
    const n = sanitizeIdent(succMatch[1]);
    return `if (${subject} > 0) { const ${n} = ${subject} - 1; return ${bodyExpr}; }`;
  }
  // Variable pattern (binding)
  if (/^[a-z_][a-zA-Z0-9_']*$/.test(pat)) {
    const vname = sanitizeIdent(pat);
    return `else { const ${vname} = ${subject}; return ${bodyExpr}; }`;
  }

  return `if (${subject} === ${transpileExpr(pat)}) { return ${bodyExpr}; }`;
}

// ---------------------------------------------------------------------------
// Binary operator parsing
// ---------------------------------------------------------------------------

const BINARY_OPS: Array<[string, string]> = [
  ["||", "||"],
  ["&&", "&&"],
  ["==", "==="],
  ["!=", "!=="],
  [">=", ">="],
  ["<=", "<="],
  [">", ">"],
  ["<", "<"],
  ["^", "__pow"],
  ["++", "__append"],
  ["::", "__cons"],
  ["+", "+"],
  ["-", "-"],
  ["*", "*"],
  ["/", "__natDiv"],
  ["%", "__natMod"],
];

function tryBinaryOp(expr: string): string | null {
  for (const [op, jsOp] of BINARY_OPS) {
    const idx = findOperator(expr, op);
    if (idx !== -1) {
      const left = expr.slice(0, idx).trim();
      const right = expr.slice(idx + op.length).trim();
      if (!left || !right) continue;
      if (jsOp === "__append") {
        return `__append(${transpileExpr(left)}, ${transpileExpr(right)})`;
      }
      if (jsOp === "__cons") {
        return `[${transpileExpr(left)}, ...${transpileExpr(right)}]`;
      }
      if (jsOp === "__pow") {
        return `__pow(${transpileExpr(left)}, ${transpileExpr(right)})`;
      }
      if (jsOp === "__natDiv") {
        return `__natDiv(${transpileExpr(left)}, ${transpileExpr(right)})`;
      }
      if (jsOp === "__natMod") {
        return `__natMod(${transpileExpr(left)}, ${transpileExpr(right)})`;
      }
      return `(${transpileExpr(left)} ${jsOp} ${transpileExpr(right)})`;
    }
  }
  return null;
}

function findOperator(expr: string, op: string): number {
  // Scan left-to-right, store last match (gives left-associativity)
  let depth = 0;
  let inStr = false;
  let result = -1;

  for (let i = 0; i <= expr.length - op.length; i++) {
    const ch = expr[i];
    if (ch === "\\" && inStr) {
      i++; // skip escaped char
      continue;
    }
    if (ch === '"' && !inStr) {
      inStr = true;
      continue;
    }
    if (ch === '"' && inStr) {
      inStr = false;
      continue;
    }
    if (inStr) continue;
    if (ch === "(" || ch === "[") {
      depth++;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth--;
      continue;
    }
    if (depth === 0 && expr.slice(i, i + op.length) === op) {
      const before = i > 0 ? expr[i - 1] : " ";
      const after =
        i + op.length < expr.length ? expr[i + op.length] : " ";
      if (op === ">" && (before === "=" || after === "=")) continue;
      if (op === "<" && (before === "=" || after === "=")) continue;
      if (op === "+" && (before === "+" || after === "+")) continue;
      if (op === "-" && before === "-") continue;
      if (op === "/" && after === "/") continue;
      if (op === "!" && after === "=") continue;
      if (op === ":" && (expr[i + 1] === ":" || (i > 0 && expr[i - 1] === ":"))) continue;
      if (
        op === "=" &&
        (before === "=" ||
          before === "!" ||
          before === "<" ||
          before === ">")
      )
        continue;
      if (op === "=" && after === ">") continue;
      if (op === "*" && (before === "*" || after === "*")) continue;
      if (op === "^" && (before === "^" || after === "^")) continue;
      result = i; // store last match for left-associativity
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Dot notation
// ---------------------------------------------------------------------------

function tryDotNotation(expr: string): string | null {
  let depth = 0;
  let inStr = false;

  for (let i = 0; i < expr.length; i++) {
    const ch = expr[i];
    if (ch === '"' && !inStr) {
      inStr = true;
      continue;
    }
    if (ch === '"' && inStr) {
      inStr = false;
      continue;
    }
    if (inStr) continue;
    if (ch === "(" || ch === "[") {
      depth++;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth--;
      continue;
    }

    if (i > 0 && ch === "." && depth === 0) {
      const obj = expr.slice(0, i).trim();
      const rest = expr.slice(i + 1).trim();

      const methodMatch = rest.match(/^([a-zA-Z_][a-zA-Z0-9_]*)(.*)$/);
      if (!methodMatch) continue;

      const methodName = methodMatch[1];
      const argsStr = methodMatch[2].trim();
      const transObj = transpileExpr(obj);

      // Namespace shortcuts: IO.println, List.X etc. fall through to application
      if (transObj === "IO" || transObj === "List" || transObj === "Array" || transObj === "Nat" || transObj === "String") {
        return null;
      }

      if (methodName === "length") return `${transObj}.length`;

      if (methodName === "map") {
        return `${transObj}.map(${transpileExpr(argsStr)})`;
      }
      if (methodName === "filter") {
        return `${transObj}.filter(${transpileExpr(argsStr)})`;
      }
      if (methodName === "foldl") {
        const args = parseArgs(argsStr);
        if (args.length >= 2) {
          return `${transObj}.reduce((acc, x) => (${transpileExpr(args[0])})(acc, x), ${transpileExpr(args[1])})`;
        }
        return `${transObj}.reduce(${transpileExpr(argsStr)})`;
      }
      if (methodName === "foldr") {
        const args = parseArgs(argsStr);
        if (args.length >= 2) {
          return `${transObj}.reduceRight((acc, x) => (${transpileExpr(args[0])})(x, acc), ${transpileExpr(args[1])})`;
        }
      }
      if (methodName === "reverse") return `[...${transObj}].reverse()`;
      if (methodName === "tail") return `${transObj}.slice(1)`;
      if (methodName === "append") {
        return `[...${transObj}, ...${transpileExpr(argsStr)}]`;
      }
      if (methodName === "contains") {
        return `${transObj}.includes(${transpileExpr(argsStr)})`;
      }
      if (methodName === "any") {
        return `${transObj}.some(${transpileExpr(argsStr)})`;
      }
      if (methodName === "all") {
        return `${transObj}.every(${transpileExpr(argsStr)})`;
      }

      if (argsStr) {
        const args = parseArgs(argsStr);
        return `${transObj}.${methodName}(${args.map(transpileExpr).join(", ")})`;
      }
      return `${transObj}.${methodName}`;
    }
  }
  return null;
}

// ---------------------------------------------------------------------------
// Function application
// ---------------------------------------------------------------------------

function tryApplication(expr: string): string | null {
  const m = expr.match(/^([a-zA-Z_][a-zA-Z0-9_.']*)((\s+)[\s\S]+)?$/);
  if (!m || !m[2]) return null;

  const fn = sanitizeIdent(m[1]);
  const rest = m[2].trim();
  if (!rest) return fn;

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
    if (ch === '"' && !inStr) {
      inStr = true;
      current += ch;
    } else if (ch === '"' && inStr) {
      inStr = false;
      current += ch;
    } else if (inStr) {
      if (ch === "\\") {
        current += ch + (str[i + 1] || "");
        i++;
      } else {
        current += ch;
      }
    } else if (ch === "(" || ch === "[") {
      depth++;
      current += ch;
    } else if (ch === ")" || ch === "]") {
      depth--;
      current += ch;
    } else if (ch === " " && depth === 0) {
      if (current.trim()) {
        args.push(current.trim());
        current = "";
      }
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
    if (ch === '"' && !inStr) {
      inStr = true;
      current += ch;
    } else if (ch === '"' && inStr) {
      inStr = false;
      current += ch;
    } else if (inStr) {
      current += ch;
    } else if (ch === "(" || ch === "[") {
      depth++;
      current += ch;
    } else if (ch === ")" || ch === "]") {
      depth--;
      current += ch;
    } else if (ch === "," && depth === 0) {
      items.push(current.trim());
      current = "";
    } else {
      current += ch;
    }
  }
  if (current.trim()) items.push(current.trim());
  return items;
}

// ---------------------------------------------------------------------------
// Top-level transpiler
// ---------------------------------------------------------------------------

function splitIntoBlocks(code: string): string[] {
  const lines = code.split("\n");
  const blocks: string[] = [];
  let current: string[] = [];

  for (const line of lines) {
    const trimmed = line.trim();
    if (/^(def |#eval |#check )/.test(trimmed) && current.length > 0) {
      blocks.push(current.join("\n"));
      current = [];
    }
    current.push(line);
  }
  if (current.length > 0) blocks.push(current.join("\n"));
  return blocks;
}

function transpileDef(def: DefParsed): string {
  const { name, params, body, patterns } = def;

  if (patterns && patterns.length > 0) {
    return transpilePatternFunc(name, params, patterns);
  }

  if (params.length === 0) {
    return `const ${name} = ${transpileExpr(body)};`;
  }

  const bodyStr = transpileExpr(body.replace(/\n\s*/g, " "));
  return `function ${name}(${params.join(", ")}) { return ${bodyStr}; }`;
}

function transpilePatternFunc(
  name: string,
  extraParams: string[],
  patterns: Array<{ pat: string; body: string }>
): string {
  const mainParam = "__p0";
  const allParams = [mainParam, ...extraParams];

  const branches = patterns.map(({ pat, body }) =>
    transpilePatternBranch(pat, mainParam, transpileExpr(body))
  );

  return `function ${name}(${allParams.join(", ")}) { ${branches.join(" ")} }`;
}

export function transpileLean(code: string): string {
  code = stripComments(code);
  const blocks = splitIntoBlocks(code);
  const output: string[] = [PRELUDE];

  for (const block of blocks) {
    const trimmed = block.trim();
    if (!trimmed) continue;

    if (trimmed.startsWith("#eval ")) {
      const exprStr = trimmed.slice(6).trim();
      output.push(`__eval(() => ${transpileExpr(exprStr)});`);
    } else if (trimmed.startsWith("#check ")) {
      // ignore type checks
    } else if (trimmed.startsWith("def ")) {
      const def = parseDef(block);
      if (def) {
        output.push(transpileDef(def));
      }
    }
  }

  return output.join("\n");
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function extractLeanDefs(code: string): string {
  code = stripComments(code);
  const blocks = splitIntoBlocks(code);
  return blocks
    .filter((b) => b.trim().startsWith("def "))
    .join("\n");
}

export async function runLean(code: string): Promise<RunResult> {
  try {
    const js = transpileLean(code);
    // eslint-disable-next-line no-new-func
    const fn = new Function(js + "\nreturn __output.join('');");
    const output = fn() as string;
    return { stdout: output, stderr: "", error: "" };
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
      const defs = extractLeanDefs(code);
      codeToRun = test.code.replace("{{FUNC}}", defs);
    }

    const result = await runLean(codeToRun);
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
