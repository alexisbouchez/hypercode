import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let kotlinReady = false;
export function isKotlinReady(): boolean { return kotlinReady; }
export async function initKotlinRunner(): Promise<void> { kotlinReady = true; }

// ---------------------------------------------------------------------------
// Prelude
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];

function __toString(v) {
  if (v === null || v === undefined) return 'null';
  if (Array.isArray(v)) return '[' + v.map(__toString).join(', ') + ']';
  if (typeof v === 'object' && v !== null && typeof v.__str === 'function') return v.__str();
  if (typeof v === 'boolean') return String(v);
  return String(v);
}

function __println() {
  if (arguments.length === 0) { __output.push('\\n'); return; }
  __output.push(__toString(arguments[0]) + '\\n');
}

function __print(v) {
  __output.push(__toString(v));
}

function minOf() { return Math.min.apply(null, arguments); }
function maxOf() { return Math.max.apply(null, arguments); }

const kotlin = {
  math: {
    sqrt: function(x) { return Math.sqrt(x); },
    abs: function(x) { return Math.abs(x); },
    pow: function(a, b) { return Math.pow(a, b); },
    floor: function(x) { return Math.floor(x); },
    ceil: function(x) { return Math.ceil(x); },
    round: function(x) { return Math.round(x); },
    PI: Math.PI,
    E: Math.E,
  }
};

if (!('size' in Array.prototype)) {
  Object.defineProperty(Array.prototype, 'size', { get() { return this.length; }, configurable: true });
}
if (!('contains' in Array.prototype)) {
  Object.defineProperty(Array.prototype, 'contains', { value(x) { return this.includes(x); }, configurable: true });
}

function __mapOf(obj) {
  return new Proxy(obj, {
    get(target, prop) {
      if (prop === 'size') return Object.keys(target).length;
      if (prop === 'contains' || prop === 'containsKey') return function(k) { return k in target; };
      if (prop === 'containsValue') return function(v) { return Object.values(target).includes(v); };
      if (prop === 'keys') return Object.keys(target);
      if (prop === 'values') return Object.values(target);
      if (prop === 'entries') return Object.entries(target).map(function(e) { return {first: e[0], second: e[1]}; });
      if (prop === '__str') return function() { return '{' + Object.entries(target).map(function(e) { return e[0] + '=' + e[1]; }).join(', ') + '}'; };
      return target[prop];
    }
  });
}
`;

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

function stripComments(code: string): string {
  code = code.replace(/\/\*[\s\S]*?\*\//g, "");
  const lines = code.split("\n");
  return lines
    .map((line) => {
      let inStr = false;
      for (let i = 0; i < line.length - 1; i++) {
        if (line[i] === "\\" && inStr) { i++; continue; }
        if (line[i] === '"') { inStr = !inStr; continue; }
        if (!inStr && line[i] === "/" && line[i + 1] === "/") {
          return line.slice(0, i);
        }
      }
      return line;
    })
    .join("\n");
}

function splitByComma(s: string): string[] {
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

function findMatchingClose(s: string, startIdx: number, open: string, close: string): number {
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

function scanStringEnd(s: string, start: number): number {
  let i = start + 1;
  while (i < s.length) {
    if (s[i] === "\\" ) { i += 2; continue; }
    if (s[i] === '"') return i + 1;
    i++;
  }
  return s.length;
}

function stripParamTypes(params: string): string {
  if (!params.trim()) return "";
  return splitByComma(params)
    .map((p) => {
      p = p.trim();
      if (!p) return p;
      const colonIdx = p.indexOf(":");
      if (colonIdx === -1) return p;
      const name = p.slice(0, colonIdx).trim();
      const afterColon = p.slice(colonIdx + 1).trim();
      // Find default value = after the type
      let depth = 0;
      for (let i = 0; i < afterColon.length; i++) {
        const ch = afterColon[i];
        if ("<([{".includes(ch)) depth++;
        else if (">)]}".includes(ch)) depth--;
        else if (ch === "=" && depth === 0 && afterColon[i - 1] !== "!" && afterColon[i - 1] !== "<" && afterColon[i - 1] !== ">" && afterColon[i + 1] !== "=") {
          const defaultVal = afterColon.slice(i + 1).trim();
          return `${name} = ${transformExpr(defaultVal)}`;
        }
      }
      return name;
    })
    .join(", ");
}

// ---------------------------------------------------------------------------
// String interpolation
// ---------------------------------------------------------------------------

function convertStringInterp(s: string): string {
  if (!s.startsWith('"') || !s.includes("$")) return s;
  const inner = s.slice(1, -1);
  let result = "";
  let i = 0;
  while (i < inner.length) {
    const ch = inner[i];
    if (ch === "\\") { result += ch + inner[i + 1]; i += 2; continue; }
    if (ch === "`") { result += "\\`"; i++; continue; }
    if (ch === "$") {
      if (inner[i + 1] === "{") {
        const closeIdx = findMatchingClose(inner, i + 1, "{", "}");
        const expr = inner.slice(i + 2, closeIdx);
        result += "${" + transformExpr(expr) + "}";
        i = closeIdx + 1;
      } else if (i + 1 < inner.length && /[a-zA-Z_]/.test(inner[i + 1])) {
        let j = i + 1;
        while (j < inner.length && /[a-zA-Z0-9_]/.test(inner[j])) j++;
        result += "${" + inner.slice(i + 1, j) + "}";
        i = j;
      } else { result += ch; i++; }
    } else { result += ch; i++; }
  }
  return "`" + result + "`";
}

// ---------------------------------------------------------------------------
// Lambda transformation
// ---------------------------------------------------------------------------

function transformLambdaInner(inner: string): string {
  inner = inner.trim();
  const arrowIdx = inner.indexOf(" -> ");
  if (arrowIdx !== -1) {
    const paramStr = inner.slice(0, arrowIdx).trim();
    const body = inner.slice(arrowIdx + 4).trim();
    const params = splitByComma(paramStr)
      .map((p) => {
        p = p.trim();
        const ci = p.indexOf(":");
        return ci !== -1 ? p.slice(0, ci).trim() : p.trim();
      })
      .join(", ");
    const jsBody = transformExpr(body);
    if (params.includes(",")) return `(${params}) => ${jsBody}`;
    return `${params} => ${jsBody}`;
  }
  // implicit `it`
  return `it => ${transformExpr(inner)}`;
}

// ---------------------------------------------------------------------------
// Expression transformer
// ---------------------------------------------------------------------------

function transformExpr(s: string): string {
  s = s.trim();
  if (!s) return "";

  let result = "";
  let i = 0;

  while (i < s.length) {
    // String literal
    if (s[i] === '"') {
      const end = scanStringEnd(s, i);
      const strLit = s.slice(i, end);
      result += convertStringInterp(strLit);
      i = end;
      continue;
    }

    // listOf( or mutableListOf(
    const listMatch = s.slice(i).match(/^(mutableListOf|listOf)\s*\(/);
    if (listMatch && (i === 0 || /\W/.test(s[i - 1]))) {
      const kLen = listMatch[0].length;
      const parenStart = i + kLen - 1;
      const parenEnd = findMatchingClose(s, parenStart, "(", ")");
      const inner = s.slice(parenStart + 1, parenEnd);
      const items = splitByComma(inner).map((x) => transformExpr(x.trim()));
      result += "[" + items.join(", ") + "]";
      i = parenEnd + 1;
      continue;
    }

    // mapOf(
    if (s.slice(i, i + 6) === "mapOf(" && (i === 0 || /\W/.test(s[i - 1]))) {
      const parenStart = i + 5;
      const parenEnd = findMatchingClose(s, parenStart, "(", ")");
      const inner = s.slice(parenStart + 1, parenEnd);
      const pairs = splitByComma(inner);
      const entries = pairs.map((pair) => {
        const toIdx = pair.indexOf(" to ");
        if (toIdx !== -1) {
          const key = transformExpr(pair.slice(0, toIdx).trim());
          const val = transformExpr(pair.slice(toIdx + 4).trim());
          return `${key}: ${val}`;
        }
        return transformExpr(pair.trim());
      });
      result += `__mapOf({${entries.join(", ")}})`;
      i = parenEnd + 1;
      continue;
    }

    // println( and print(
    if (s.slice(i, i + 8) === "println(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__println(";
      i += 8;
      continue;
    }
    if (s.slice(i).match(/^println\s*\(\s*\)/) && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__println()";
      i += s.slice(i).match(/^println\s*\(\s*\)/)![0].length;
      continue;
    }
    if (s.slice(i, i + 6) === "print(" && (i === 0 || /\W/.test(s[i - 1]))) {
      result += "__print(";
      i += 6;
      continue;
    }

    // Method chains
    if (s[i] === "." && i > 0 && s[i - 1] !== ".") {
      let j = i + 1;
      while (j < s.length && /[a-zA-Z0-9_]/.test(s[j])) j++;
      const methodName = s.slice(i + 1, j);

      // .size → .size (handled by Array.prototype.size getter and __mapOf proxy)
      if (methodName === "size") {
        result += ".size";
        i = j;
        continue;
      }

      // Method with () args
      if (j < s.length && s[j] === "(") {
        const parenEnd = findMatchingClose(s, j, "(", ")");
        const args = s.slice(j + 1, parenEnd);
        const jsArgs = splitByComma(args).map((a) => transformExpr(a.trim())).join(", ");

        // fold(init) { lambda }
        if (methodName === "fold" || methodName === "reduce") {
          let k = parenEnd + 1;
          while (k < s.length && s[k] === " ") k++;
          if (k < s.length && s[k] === "{") {
            const lambdaEnd = findMatchingClose(s, k, "{", "}");
            const lambdaInner = s.slice(k + 1, lambdaEnd).trim();
            const lambda = transformLambdaInner(lambdaInner);
            result += `.reduce(${lambda}, ${transformExpr(args)})`;
            i = lambdaEnd + 1;
            continue;
          }
        }

        // sortedBy(fn)
        if (methodName === "sortedBy") {
          result += `.sort((a, b) => { const fa = (${jsArgs})(a), fb = (${jsArgs})(b); return fa < fb ? -1 : fa > fb ? 1 : 0; })`;
          i = parenEnd + 1;
          continue;
        }

        // Renamed methods
        const renames: Record<string, string> = {
          uppercase: "toUpperCase",
          lowercase: "toLowerCase",
          contains: "includes",
          startsWith: "startsWith",
          endsWith: "endsWith",
          split: "split",
          trim: "trim",
          trimIndent: "trim",
          trimStart: "trimStart",
          trimEnd: "trimEnd",
          reversed: "reverse",
          toList: "slice",
          toMutableList: "slice",
          joinToString: "join",
        };

        if (methodName === "toInt" || methodName === "toIntOrNull") {
          result += `/* .${methodName}() */`;
          i = parenEnd + 1;
          continue;
        }
        if (methodName === "toDouble" || methodName === "toFloat") {
          result += `/* .${methodName}() */`;
          i = parenEnd + 1;
          continue;
        }
        if (methodName === "toString") {
          result += ".toString()";
          i = parenEnd + 1;
          continue;
        }
        if (methodName === "copy") {
          // copy(x = val, y = val) → copy({x: val, y: val})
          const namedArgs = splitByComma(args)
            .map((a) => {
              const eqIdx = a.indexOf("=");
              if (eqIdx !== -1 && !/[!<>=]/.test(a[eqIdx - 1]) && a[eqIdx + 1] !== "=") {
                const k = a.slice(0, eqIdx).trim();
                const v = transformExpr(a.slice(eqIdx + 1).trim());
                return `${k}: ${v}`;
              }
              return transformExpr(a.trim());
            })
            .join(", ");
          result += `.copy({${namedArgs}})`;
          i = parenEnd + 1;
          continue;
        }
        if (methodName === "subList" || methodName === "sublist") {
          result += `.slice(${jsArgs})`;
          i = parenEnd + 1;
          continue;
        }
        if (methodName === "getOrDefault") {
          const argList = splitByComma(args);
          const key = transformExpr(argList[0]);
          const def = transformExpr(argList[1] || "null");
          result += `[${key}] ?? ${def}`;
          i = parenEnd + 1;
          continue;
        }

        const jsMethod = renames[methodName] ?? methodName;
        result += `.${jsMethod}(${jsArgs})`;
        i = parenEnd + 1;
        continue;
      }

      // Method with lambda block (no parens)
      const afterMethod = s.slice(j);
      const spaceAndBrace = afterMethod.match(/^\s*\{/);
      if (spaceAndBrace) {
        const braceOffset = j + afterMethod.indexOf("{");
        const braceEnd = findMatchingClose(s, braceOffset, "{", "}");
        const lambdaInner = s.slice(braceOffset + 1, braceEnd).trim();
        const lambda = transformLambdaInner(lambdaInner);

        switch (methodName) {
          case "map": result += `.map(${lambda})`; break;
          case "filter": result += `.filter(${lambda})`; break;
          case "forEach": result += `.forEach(${lambda})`; break;
          case "any": result += `.some(${lambda})`; break;
          case "all": result += `.every(${lambda})`; break;
          case "count": result += `.filter(${lambda}).length`; break;
          case "flatMap": result += `.flatMap(${lambda})`; break;
          case "mapIndexed": result += `.map((it, idx) => (${lambda})(idx, it))`; break;
          case "forEachIndexed": result += `.forEach((it, idx) => (${lambda})(idx, it))`; break;
          case "sortedBy": result += `.slice().sort((a, b) => { const fa = (${lambda})(a), fb = (${lambda})(b); return fa < fb ? -1 : fa > fb ? 1 : 0; })`; break;
          default: result += `.${methodName}(${lambda})`; break;
        }
        i = braceEnd + 1;
        continue;
      }

      // .keys, .values, .entries
      if (methodName === "keys") { result += " && Object.keys(" + extractLastExpr() + ")"; i = j; continue; }
      if (methodName === "values") { result += " && Object.values(" + extractLastExpr() + ")"; i = j; continue; }

      // Simple property
      result += "." + methodName;
      i = j;
      continue;
    }

    // Lambda literal { ... }
    if (s[i] === "{" && (i === 0 || /[\s=(,!&|?:+\-*/<>]/.test(s[i - 1]))) {
      const braceEnd = findMatchingClose(s, i, "{", "}");
      const inner = s.slice(i + 1, braceEnd).trim();
      result += transformLambdaInner(inner);
      i = braceEnd + 1;
      continue;
    }

    // == and !=
    if (s[i] === "=" && s[i + 1] === "=") {
      if (s[i + 2] === "=") { result += "==="; i += 3; }
      else if (i > 0 && /[!<>]/.test(s[i - 1])) { result += "=="; i += 2; }
      else { result += "==="; i += 2; }
      continue;
    }
    if (s[i] === "!" && s[i + 1] === "=") {
      if (s[i + 2] === "=") { result += "!=="; i += 3; }
      else { result += "!=="; i += 2; }
      continue;
    }

    result += s[i];
    i++;
  }

  return result;
}

function extractLastExpr(): string {
  // For .keys/.values patterns on objects, return empty string for now
  return "";
}

// ---------------------------------------------------------------------------
// Statement transformer
// ---------------------------------------------------------------------------

function transformStatement(line: string): string {
  const indent = line.match(/^(\s*)/)?.[1] ?? "";
  const s = line.trim();
  if (!s || s === "}") return line;

  // val x: Type = expr  or  val x = expr
  const valMatch = s.match(/^val\s+(\w+)(?:\s*:\s*[^=]+)?\s*=\s*(.+)$/);
  if (valMatch) {
    return indent + `const ${valMatch[1]} = ${transformExpr(valMatch[2].trim())};`;
  }

  // var x: Type = expr  or  var x = expr
  const varMatch = s.match(/^var\s+(\w+)(?:\s*:\s*[^=]+)?\s*=\s*(.+)$/);
  if (varMatch) {
    return indent + `let ${varMatch[1]} = ${transformExpr(varMatch[2].trim())};`;
  }

  // var x: Type (declaration without value)
  const varDeclMatch = s.match(/^var\s+(\w+)\s*:\s*\S+\s*$/);
  if (varDeclMatch) {
    return indent + `let ${varDeclMatch[1]};`;
  }

  // x = expr (reassignment — must not be keyword)
  const reassignMatch = s.match(/^([a-zA-Z_]\w*)\s*=\s*(.+)$/);
  if (reassignMatch && !["if", "else", "for", "while", "when", "return", "fun", "val", "var", "class", "data"].includes(reassignMatch[1])) {
    return indent + `${reassignMatch[1]} = ${transformExpr(reassignMatch[2].trim())};`;
  }

  // x += / -= / *= / /= / %=
  const compoundMatch = s.match(/^([a-zA-Z_]\w*)\s*(\+=|-=|\*=|\/=|%=)\s*(.+)$/);
  if (compoundMatch) {
    return indent + `${compoundMatch[1]} ${compoundMatch[2]} ${transformExpr(compoundMatch[3].trim())};`;
  }

  // return expr
  if (s.startsWith("return ")) {
    return indent + `return ${transformExpr(s.slice(7).trim())};`;
  }
  if (s === "return") return indent + "return;";

  // println(...) and print(...)
  if (/^println\s*\(/.test(s)) {
    const inner = extractCallArgs(s, s.indexOf("("));
    if (inner.trim() === "") return indent + "__println();";
    return indent + `__println(${transformExpr(inner)});`;
  }
  if (/^print\s*\(/.test(s)) {
    const inner = extractCallArgs(s, s.indexOf("("));
    return indent + `__print(${transformExpr(inner)});`;
  }

  // for (i in a..b) {
  const forRangeMatch = s.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\.\.(.*?)\s*\)\s*\{?$/);
  if (forRangeMatch) {
    const [, variable, start, end] = forRangeMatch;
    return indent + `for (let ${variable} = ${transformExpr(start.trim())}; ${variable} <= ${transformExpr(end.trim())}; ${variable}++) {`;
  }

  // for (i in a until b) {
  const forUntilMatch = s.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\s+until\s+(.+?)\s*\)\s*\{?$/);
  if (forUntilMatch) {
    const [, variable, start, end] = forUntilMatch;
    return indent + `for (let ${variable} = ${transformExpr(start.trim())}; ${variable} < ${transformExpr(end.trim())}; ${variable}++) {`;
  }

  // for (i in a downTo b) {
  const forDownToMatch = s.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\s+downTo\s+(.+?)\s*\)\s*\{?$/);
  if (forDownToMatch) {
    const [, variable, start, end] = forDownToMatch;
    return indent + `for (let ${variable} = ${transformExpr(start.trim())}; ${variable} >= ${transformExpr(end.trim())}; ${variable}--) {`;
  }

  // for (item in list) {
  const forInMatch = s.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\s*\)\s*\{?$/);
  if (forInMatch) {
    const [, variable, iterable] = forInMatch;
    return indent + `for (const ${variable} of ${transformExpr(iterable.trim())}) {`;
  }

  // while
  const whileMatch = s.match(/^while\s*\((.+)\)\s*\{?$/);
  if (whileMatch) {
    return indent + `while (${transformExpr(whileMatch[1])}) {`;
  }

  // if
  const ifMatch = s.match(/^if\s*\((.+)\)\s*\{?$/);
  if (ifMatch) {
    return indent + `if (${transformExpr(ifMatch[1])}) {`;
  }

  // } else if
  const elseIfMatch = s.match(/^}\s*else\s+if\s*\((.+)\)\s*\{?$/);
  if (elseIfMatch) {
    return indent + `} else if (${transformExpr(elseIfMatch[1])}) {`;
  }

  // } else {
  if (/^}\s*else\s*\{?$/.test(s)) return indent + "} else {";
  if (/^else\s*\{?$/.test(s)) return indent + "} else {";

  // Generic expression statement
  return indent + transformExpr(s) + ";";
}

function extractCallArgs(s: string, parenIdx: number): string {
  const close = findMatchingClose(s, parenIdx, "(", ")");
  return s.slice(parenIdx + 1, close);
}

// ---------------------------------------------------------------------------
// When block transformer
// ---------------------------------------------------------------------------

function collectWhenBlock(lines: string[], startIdx: number): { arms: string[]; next: number } {
  const arms: string[] = [];
  let depth = 0;
  // count opening braces on start line
  for (const ch of lines[startIdx]) {
    if (ch === "{") depth++;
    else if (ch === "}") depth--;
  }
  let i = startIdx + 1;
  while (i < lines.length) {
    const t = lines[i].trim();
    if (!t) { i++; continue; }
    for (const ch of t) {
      if (ch === "{") depth++;
      else if (ch === "}") depth--;
    }
    if (depth <= 0) { i++; break; }
    if (t.includes(" -> ")) arms.push(t);
    i++;
  }
  return { arms, next: i };
}

function buildWhenIife(expr: string, arms: string[], indent: string): string {
  const lines: string[] = [`(() => {`, `  const __w = ${expr};`];
  for (const arm of arms) {
    const arrowIdx = arm.indexOf(" -> ");
    const pat = arm.slice(0, arrowIdx).trim();
    const body = arm.slice(arrowIdx + 4).trim();
    if (pat === "else") {
      lines.push(`  return ${transformExpr(body)};`);
    } else {
      lines.push(`  if (__w === ${transformExpr(pat)}) return ${transformExpr(body)};`);
    }
  }
  lines.push(`})()`);
  return lines.map((l) => indent + l).join("\n");
}

// ---------------------------------------------------------------------------
// Function transformer
// ---------------------------------------------------------------------------

function collectFunctionBody(lines: string[], startIdx: number): { bodyLines: string[]; next: number } {
  const bodyLines: string[] = [];
  let depth = 0;
  for (const ch of lines[startIdx]) {
    if (ch === "{") depth++;
    else if (ch === "}") depth--;
  }
  let i = startIdx + 1;
  while (i < lines.length) {
    // Count braces but respect string literals
    let inStr = false;
    for (const ch of lines[i]) {
      if (ch === '"') { inStr = !inStr; continue; }
      if (inStr) continue;
      if (ch === "{") depth++;
      else if (ch === "}") depth--;
    }
    if (depth <= 0) { i++; break; }
    bodyLines.push(lines[i]);
    i++;
  }
  return { bodyLines, next: i };
}

function transformFunctionBody(lines: string[]): string {
  const result: string[] = [];
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const s = line.trim();
    const indent = line.match(/^(\s*)/)?.[1] ?? "";

    if (!s) { result.push(""); i++; continue; }
    if (s === "}") { result.push(indent + "}"); i++; continue; }

    // when as expression: val x = when (expr) { ... }
    const whenAssign = s.match(/^(val|var)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*when\s*\((.+)\)\s*\{?$/);
    const whenReturn = s.match(/^return\s+when\s*\((.+)\)\s*\{?$/);
    const whenStmt = s.match(/^when\s*\((.+)\)\s*\{?$/);

    if (whenAssign || whenReturn || whenStmt) {
      const { arms, next } = collectWhenBlock(lines, i);
      if (whenAssign) {
        const [, kw, vn, ex] = whenAssign;
        const decl = kw === "val" ? "const" : "let";
        result.push(`${indent}${decl} ${vn} = ${buildWhenIife(transformExpr(ex), arms, indent)};`);
      } else if (whenReturn) {
        result.push(`${indent}return ${buildWhenIife(transformExpr(whenReturn[1]), arms, indent)};`);
      } else if (whenStmt) {
        result.push(`${indent}${buildWhenIife(transformExpr(whenStmt![1]), arms, indent)};`);
      }
      i = next;
      continue;
    }

    // Nested function body (e.g., if block with braces on same line)
    result.push(transformStatement(line));
    i++;
  }

  return result.join("\n");
}

// ---------------------------------------------------------------------------
// Top-level processors
// ---------------------------------------------------------------------------

function collectDataClass(lines: string[], start: number): { code: string; next: number } {
  const line = lines[start].trim();
  const match = line.match(/^data\s+class\s+(\w+)\s*\((.+)\)/);
  if (!match) return { code: "// unhandled data class", next: start + 1 };

  const className = match[1];
  const params = splitByComma(match[2]).map((p) => {
    p = p.trim().replace(/^(?:val|var)\s+/, "");
    const ci = p.indexOf(":");
    return ci !== -1 ? p.slice(0, ci).trim() : p.trim();
  });

  const paramList = params.join(", ");
  const strParts = params.map((p) => `${p}=\${__toString(${p})}`).join(", ");
  const copyAssigns = params.map((p) => `(o && o.${p} !== undefined ? o.${p} : ${p})`).join(", ");

  const code = `function ${className}(${paramList}) {
  const __self = {
    ${params.join(", ")},
    __str() { return \`${className}(${strParts})\`; },
    toString() { return this.__str(); },
    copy(o) { return ${className}(${copyAssigns}); },
  };
  return __self;
}`;
  return { code, next: start + 1 };
}

function collectFunction(lines: string[], start: number): { code: string; next: number } {
  const line = lines[start].trim();

  // Find function name
  const funNameMatch = line.match(/^fun\s+(\w+)\s*\(/);
  if (!funNameMatch) {
    return { code: transformStatement(lines[start]), next: start + 1 };
  }

  const name = funNameMatch[1];
  const parenStart = line.indexOf("(");
  const parenEnd = findMatchingClose(line, parenStart, "(", ")");
  if (parenEnd === -1) {
    return { code: transformStatement(lines[start]), next: start + 1 };
  }

  const params = line.slice(parenStart + 1, parenEnd);
  const jsParams = stripParamTypes(params);
  let afterParen = line.slice(parenEnd + 1).trim();

  // Strip return type annotation (": Type") before = or {
  if (afterParen.startsWith(":")) {
    let depth = 0;
    let j = 1;
    while (j < afterParen.length) {
      const ch = afterParen[j];
      if ("<([{".includes(ch)) depth++;
      else if (">)]}".includes(ch)) depth--;
      else if (depth === 0 && (ch === "=" || ch === "{")) break;
      j++;
    }
    afterParen = afterParen.slice(j).trim();
  }

  // Single-expression: = expr
  if (afterParen.startsWith("=")) {
    const body = afterParen.slice(1).trim();
    return {
      code: `function ${name}(${jsParams}) { return ${transformExpr(body)}; }`,
      next: start + 1,
    };
  }

  // Single-line block: { body }
  if (afterParen.startsWith("{") && afterParen.endsWith("}")) {
    const innerBody = afterParen.slice(1, -1).trim();
    const stmts = innerBody.split(";").map((s) => transformStatement(s.trim())).filter(Boolean).join(" ");
    return {
      code: `function ${name}(${jsParams}) { ${stmts} }`,
      next: start + 1,
    };
  }

  // Block function (possibly just "{" or empty at end of line)
  const { bodyLines, next } = collectFunctionBody(lines, start);
  const jsBody = transformFunctionBody(bodyLines);
  return {
    code: `function ${name}(${jsParams}) {\n${jsBody}\n}`,
    next,
  };
}

// ---------------------------------------------------------------------------
// Main transpiler
// ---------------------------------------------------------------------------

export function transpileKotlin(code: string): string {
  code = stripComments(code);
  const lines = code.split("\n");
  const output: string[] = [PRELUDE];

  let i = 0;
  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trim();

    if (!trimmed) { i++; continue; }

    if (trimmed.startsWith("data class ")) {
      const { code: dc, next } = collectDataClass(lines, i);
      output.push(dc);
      i = next;
      continue;
    }

    if (/^fun\s+\w+/.test(trimmed)) {
      const { code: fn, next } = collectFunction(lines, i);
      output.push(fn);
      i = next;
      continue;
    }

    // Top-level val/var (rare in Kotlin but handle it)
    output.push(transformStatement(line));
    i++;
  }

  output.push('\nif (typeof main === "function") main();');
  return output.join("\n");
}

// Returns raw Kotlin source for non-main functions and data classes,
// suitable for use with {{FUNC}} test templates (which are then transpiled).
export function extractKotlinFunctions(code: string): string {
  code = stripComments(code);
  const lines = code.split("\n");
  const result: string[] = [];
  let i = 0;

  while (i < lines.length) {
    const trimmed = lines[i].trim();

    if (trimmed.startsWith("data class ")) {
      result.push(lines[i]);
      i++;
      continue;
    }

    if (/^fun\s+\w+/.test(trimmed) && !trimmed.startsWith("fun main")) {
      // Collect the raw Kotlin source of this function
      result.push(lines[i]);
      // Check if it's a block function (has opening brace)
      let depth = 0;
      for (const ch of trimmed) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth > 0) {
        i++;
        while (i < lines.length && depth > 0) {
          const bl = lines[i];
          let inStr = false;
          for (const ch of bl) {
            if (ch === '"') { inStr = !inStr; continue; }
            if (inStr) continue;
            if (ch === "{") depth++;
            else if (ch === "}") depth--;
          }
          result.push(bl);
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

export async function runKotlin(code: string): Promise<RunResult> {
  try {
    const js = transpileKotlin(code);
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
      const fns = extractKotlinFunctions(code);
      codeToRun = test.code.replace("{{FUNC}}", fns);
    }

    const result = await runKotlin(codeToRun);
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
