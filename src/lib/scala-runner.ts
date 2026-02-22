import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let scalaReady = false;

export function isScalaReady(): boolean {
  return scalaReady;
}

export async function initScalaRunner(): Promise<void> {
  scalaReady = true;
}

// ---------------------------------------------------------------------------
// Scala → JavaScript transpiler
// Handles the subset of Scala used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function print(...args) {
  const fmt = (v) => {
    if (v === null || v === undefined) return 'None';
    if (Array.isArray(v)) return 'List(' + v.map(fmt).join(', ') + ')';
    if (typeof v === 'boolean') return String(v);
    return String(v);
  };
  __output.push(args.map(fmt).join(' ') + '\\n');
}
const println = print;
function List(...args) { return [...args]; }
function Some(v) { return v; }
const None = null;
const Nil = [];
`;

// ---------------------------------------------------------------------------
// String interpolation: s"Hello, $name, ${expr}" → \`Hello, ${name}, ${expr}\`
// ---------------------------------------------------------------------------

function transformStringInterpolation(line: string): string {
  let result = "";
  let i = 0;
  while (i < line.length) {
    // s-string interpolation
    if (line[i] === "s" && line[i + 1] === '"') {
      i += 2; // skip s"
      let inner = "";
      while (i < line.length && line[i] !== '"') {
        if (line[i] === "\\" && i + 1 < line.length) {
          inner += line[i] + line[i + 1];
          i += 2;
        } else if (line[i] === "$") {
          i++;
          if (line[i] === "{") {
            i++;
            let depth = 1;
            let expr = "";
            while (i < line.length && depth > 0) {
              if (line[i] === "{") depth++;
              else if (line[i] === "}") {
                depth--;
                if (depth === 0) { i++; break; }
              }
              expr += line[i++];
            }
            inner += "${" + expr + "}";
          } else {
            let name = "";
            while (i < line.length && /\w/.test(line[i])) name += line[i++];
            inner += "${" + name + "}";
          }
        } else {
          inner += line[i++];
        }
      }
      i++; // closing "
      result += "`" + inner + "`";
    } else if (line[i] === '"') {
      // Regular string — pass through unchanged
      result += line[i++];
      while (i < line.length && line[i] !== '"') {
        if (line[i] === "\\" && i + 1 < line.length) {
          result += line[i] + line[i + 1];
          i += 2;
        } else {
          result += line[i++];
        }
      }
      result += line[i] ?? "";
      if (i < line.length) i++;
    } else {
      result += line[i++];
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Parse Scala parameter list "a: Int, b: List[Int]" → ["a", "b"]
// Handles generic types (brackets) and tuple types.
// ---------------------------------------------------------------------------

function parseScalaParams(raw: string): string[] {
  if (!raw.trim()) return [];
  const params: string[] = [];
  let depth = 0;
  let current = "";
  for (const ch of raw) {
    if (ch === "[" || ch === "(" || ch === "<") depth++;
    else if (ch === "]" || ch === ")" || ch === ">") depth--;
    else if (ch === "," && depth === 0) {
      const trimmed = current.trim();
      const colonIdx = trimmed.indexOf(":");
      if (colonIdx >= 0) {
        const namePart = trimmed.slice(0, colonIdx).trim();
        params.push(namePart.replace(/^(val|var)\s+/, "").trim());
      } else {
        params.push(trimmed);
      }
      current = "";
      continue;
    }
    current += ch;
  }
  if (current.trim()) {
    const trimmed = current.trim();
    const colonIdx = trimmed.indexOf(":");
    if (colonIdx >= 0) {
      const namePart = trimmed.slice(0, colonIdx).trim();
      params.push(namePart.replace(/^(val|var)\s+/, "").trim());
    } else {
      params.push(trimmed);
    }
  }
  return params.filter(Boolean);
}

// ---------------------------------------------------------------------------
// Rewrite implicit field access inside class methods: "width" → "this.width"
// ---------------------------------------------------------------------------

function rewriteImplicitThis(expr: string, fields: string[]): string {
  for (const f of fields) {
    expr = expr.replace(new RegExp(`(?<!this\\.)\\b${f}\\b`, "g"), `this.${f}`);
  }
  return expr;
}

// ---------------------------------------------------------------------------
// Transform .foldLeft(init)(lambda) → .reduce(lambda, init)
// Uses balanced-paren parsing.
// ---------------------------------------------------------------------------

function transformFoldLeft(line: string): string {
  let result = line;
  const marker = ".foldLeft(";
  let searchFrom = 0;
  while (true) {
    const idx = result.indexOf(marker, searchFrom);
    if (idx < 0) break;

    // Parse init arg
    let i = idx + marker.length;
    let depth = 1;
    let init = "";
    while (i < result.length && depth > 0) {
      if (result[i] === "(") depth++;
      else if (result[i] === ")") { depth--; if (depth === 0) break; }
      init += result[i++];
    }
    i++; // skip closing )

    if (result[i] !== "(") { searchFrom = idx + 1; continue; }
    i++;
    depth = 1;
    let lambda = "";
    while (i < result.length && depth > 0) {
      if (result[i] === "(") depth++;
      else if (result[i] === ")") { depth--; if (depth === 0) break; }
      lambda += result[i++];
    }
    i++; // skip closing )

    const before = result.slice(0, idx);
    const after = result.slice(i);
    result = `${before}.reduce(${lambda}, ${init})${after}`;
    searchFrom = before.length;
  }
  return result;
}

// ---------------------------------------------------------------------------
// Transform match expression in a single case line
// Returns the JS if-statement for that case.
// ---------------------------------------------------------------------------

function parseCase(caseLine: string, matchVar: string): string {
  const content = caseLine.replace(/^\s*case\s+/, "").trim();
  const arrowIdx = content.indexOf("=>");
  if (arrowIdx < 0) return "";
  const pattern = content.slice(0, arrowIdx).trim();
  const body = content.slice(arrowIdx + 2).trim();

  // Wildcard
  if (pattern === "_") return `return ${body};`;

  // Guard: "n if condition"
  const guardMatch = pattern.match(/^(\w+)\s+if\s+(.+)$/);
  if (guardMatch) {
    const [, name, cond] = guardMatch;
    const jsCondition = cond
      .replace(new RegExp(`\\b${name}\\b`, "g"), matchVar)
      .replace(/(?<![=!<>])==(?!=)/g, "===")
      .replace(/!=(?!=)/g, "!==");
    return `if (${jsCondition}) return ${body};`;
  }

  // OR patterns: "12 | 1 | 2"
  if (pattern.includes("|")) {
    const parts = pattern.split("|").map((p) => p.trim());
    const conditions = parts.map((p) => `${matchVar} === ${p}`).join(" || ");
    return `if (${conditions}) return ${body};`;
  }

  // Simple literal or variable
  return `if (${matchVar} === ${pattern}) return ${body};`;
}

// ---------------------------------------------------------------------------
// Apply common transforms to a line (not string-interpolation-sensitive)
// ---------------------------------------------------------------------------

function addNewToConstructors(line: string, classNames: Set<string>): string {
  for (const name of classNames) {
    // TypeName(...) not preceded by "new" → new TypeName(...)
    const re = new RegExp(`(?<!new )(?<![.\\w])\\b(${name})\\s*\\(`, "g");
    line = line.replace(re, `new $1(`);
  }
  return line;
}

function applyCommonTransforms(line: string, fields: string[] = [], classNames: Set<string> = new Set()): string {
  // .size → .length
  line = line.replace(/\.size\b/g, ".length");

  // .isEmpty → .length === 0
  line = line.replace(/\.isEmpty\b/g, ".length === 0");

  // .nonEmpty → .length > 0
  line = line.replace(/\.nonEmpty\b/g, ".length > 0");

  // .head → [0]
  line = line.replace(/\.head\b/g, "[0]");

  // .tail → .slice(1)
  line = line.replace(/\.tail\b/g, ".slice(1)");

  // .toList → remove
  line = line.replace(/\.toList\b/g, "");

  // .mkString(sep) → .join(sep)
  line = line.replace(/\.mkString\(/g, ".join(");

  // Some(x) → x (unwrap) — handled via PRELUDE function definition

  // None → null
  line = line.replace(/\bNone\b/g, "null");

  // Nil → []
  line = line.replace(/\bNil\b/g, "[]");

  // .getOrElse(x) → ?? x
  line = line.replace(/\.getOrElse\(([^)]+)\)/g, " ?? $1");

  // .reverse (no parens, string reverse) → .split('').reverse().join('')
  line = line.replace(/\.reverse(?!\s*\()/g, ".split('').reverse().join('')");

  // Tuple access: ._1 → [0], ._2 → [1], ._3 → [2]
  line = line.replace(/\._1\b/g, "[0]");
  line = line.replace(/\._2\b/g, "[1]");
  line = line.replace(/\._3\b/g, "[2]");

  // == → === (not already ===, <=, >=, !=)
  line = line.replace(/(?<![=!<>])==(?!=)/g, "===");

  // != → !== (not already !==)
  line = line.replace(/!=(?!=)/g, "!==");

  // foldLeft transform
  line = transformFoldLeft(line);

  // Constructor calls: TypeName(args) → new TypeName(args)
  if (classNames.size > 0) {
    line = addNewToConstructors(line, classNames);
  }

  // Rewrite implicit this in class context
  if (fields.length > 0) {
    line = rewriteImplicitThis(line, fields);
  }

  return line;
}

// ---------------------------------------------------------------------------
// Single-line if/else → ternary chain (for use in single-expression defs)
// "if (c) a else b" → "(c) ? a : b"
// Uses balanced-paren parsing for the condition.
// ---------------------------------------------------------------------------

function findMatchingParen(s: string, start: number): number {
  let depth = 1;
  let i = start + 1;
  while (i < s.length && depth > 0) {
    if (s[i] === "(") depth++;
    else if (s[i] === ")") depth--;
    i++;
  }
  return i - 1;
}

function transformInlineIfElse(expr: string): string {
  const trimmed = expr.trim();
  if (!/^if\s*\(/.test(trimmed)) return expr;

  const openParen = trimmed.indexOf("(");
  const closeParen = findMatchingParen(trimmed, openParen);
  const cond = trimmed.slice(openParen + 1, closeParen);
  const afterCond = trimmed.slice(closeParen + 1).trim();

  // Find " else " in afterCond (first occurrence, word boundary)
  const elseMatch = afterCond.match(/^(.*?)\belse\b(.*)$/s);
  if (!elseMatch) return expr;

  const then_ = elseMatch[1].trim();
  const else_ = elseMatch[2].trim();
  return `(${cond}) ? ${then_} : ${transformInlineIfElse(else_)}`;
}

// ---------------------------------------------------------------------------
// Main transpile function
// ---------------------------------------------------------------------------

export function transpileScala(code: string): string {
  // Phase 1: string interpolation
  code = code.split("\n").map(transformStringInterpolation).join("\n");

  // Phase 2: collect class/case-class names for constructor call detection
  const classNames = new Set<string>();
  for (const line of code.split("\n")) {
    const m = line.trim().match(/^(?:case\s+)?class\s+(\w+)/);
    if (m) classNames.add(m[1]);
  }

  const lines = code.split("\n");
  const out: string[] = [PRELUDE];

  type CtxType = "top" | "class" | "object" | "trait" | "match";
  const ctxStack: Array<{ type: CtxType; depth: number; fields: string[]; name: string }> = [];
  const traitNames = new Set<string>();

  let depth = 0;
  let inMatchBlock = false;
  let matchVar = "__m";
  let matchCloseDepth = 0; // depth at which match closes (depth of the matching })

  const currentCtx = (): CtxType =>
    ctxStack.length ? ctxStack[ctxStack.length - 1].type : "top";
  const currentFields = (): string[] =>
    ctxStack.length ? ctxStack[ctxStack.length - 1].fields : [];

  let i = 0;
  while (i < lines.length) {
    const raw = lines[i];
    const trimmed = raw.trim();
    i++;

    const opens = (raw.match(/\{/g) ?? []).length;
    const closes = (raw.match(/\}/g) ?? []).length;
    const newDepth = depth + opens - closes;

    // Pop closed contexts
    while (ctxStack.length > 0 && ctxStack[ctxStack.length - 1].depth > newDepth) {
      ctxStack.pop();
    }

    // Blank lines
    if (trimmed === "") { out.push(""); depth = newDepth; continue; }

    // Comments
    if (trimmed.startsWith("//")) { out.push(raw); depth = newDepth; continue; }

    // ----- Match block: collect case lines -----
    if (inMatchBlock) {
      if (trimmed.startsWith("case ")) {
        const indent = raw.match(/^(\s*)/)?.[1] ?? "  ";
        out.push(`${indent}${parseCase(trimmed, matchVar)}`);
        depth = newDepth;
        continue;
      }
      if (trimmed === "}") {
        // End of match block — also end the function/expression
        const indent = raw.match(/^(\s*)/)?.[1] ?? "";
        out.push(`${indent}}`);
        inMatchBlock = false;
        depth = newDepth;
        continue;
      }
      depth = newDepth;
      continue;
    }

    // ----- Trait declaration — skip body -----
    const traitM = trimmed.match(/^trait\s+(\w+)/);
    if (traitM) {
      traitNames.add(traitM[1]);
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      out.push(`${indent}// trait ${traitM[1]} (skipped)`);
      if (opens > 0) {
        // skip until closing }
        let d = opens - closes;
        while (d > 0 && i < lines.length) {
          const l = lines[i++];
          d += (l.match(/\{/g) ?? []).length - (l.match(/\}/g) ?? []).length;
        }
      }
      depth = newDepth;
      continue;
    }

    // ----- Case class -----
    const caseClassM = trimmed.match(/^case\s+class\s+(\w+)\s*\(([^)]*)\)/);
    if (caseClassM) {
      const [, className, rawParams] = caseClassM;
      const fields = parseScalaParams(rawParams);
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      const ctorBody = fields.map((f) => `${indent}  this.${f} = ${f};`).join("\n");
      out.push(`${indent}class ${className} {`);
      out.push(`${indent}  constructor(${fields.join(", ")}) {`);
      out.push(ctorBody);
      out.push(`${indent}  }`);
      if (opens > 0) {
        // Has a body — push class context
        ctxStack.push({ type: "class", depth: depth + opens, fields, name: className });
      } else {
        out.push(`${indent}}`);
      }
      depth = newDepth;
      continue;
    }

    // ----- Regular class -----
    const classM = trimmed.match(/^class\s+(\w+)\s*(?:\(([^)]*)\))?\s*(?:extends\s+([\w,\s]+))?\s*\{/);
    if (classM) {
      const [, className, rawParams, extendsClause] = classM;
      const fields = rawParams ? parseScalaParams(rawParams) : [];
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";

      // Filter out trait parents
      let extendsStr = "";
      if (extendsClause) {
        const parents = extendsClause.split(",").map((p) => p.trim()).filter((p) => !traitNames.has(p));
        if (parents.length > 0) extendsStr = ` extends ${parents.join(", ")}`;
      }

      out.push(`${indent}class ${className}${extendsStr} {`);
      if (fields.length > 0) {
        const superCall = extendsStr ? `${indent}    super();\n` : "";
        out.push(`${indent}  constructor(${fields.join(", ")}) {`);
        if (superCall) out.push(superCall.trimEnd());
        for (const f of fields) {
          out.push(`${indent}    this.${f} = ${f};`);
        }
        out.push(`${indent}  }`);
      }
      ctxStack.push({ type: "class", depth: depth + opens, fields, name: className });
      depth = newDepth;
      continue;
    }

    // ----- Object (singleton) -----
    const objectM = trimmed.match(/^object\s+(\w+)\s*\{/);
    if (objectM) {
      const [, objName] = objectM;
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      out.push(`${indent}const ${objName} = {`);
      ctxStack.push({ type: "object", depth: depth + opens, fields: [], name: objName });
      depth = newDepth;
      continue;
    }

    // ----- def declaration -----
    // Handle multiline def: "def f(params): T =\n  body" — merge next non-empty line into current
    let rawDef = raw;
    if (/^\s*(?:(?:override|final|protected|private|abstract|sealed)\s+)*def\s+\w+.*=\s*$/.test(raw)) {
      // Body is on the next line — consume it
      while (i < lines.length && lines[i].trim() === "") i++;
      if (i < lines.length) {
        rawDef = raw.trimEnd() + " " + lines[i].trim();
        i++;
      }
    }

    const defPattern = /^(\s*)(?:(?:override|final|protected|private|abstract|sealed)\s+)*def\s+(\w+)\s*(?:\(([^)]*)\))?\s*(?::\s*[^={\n]+?)?\s*=\s*(.+)$/;
    const defBlockPattern = /^(\s*)(?:(?:override|final|protected|private|abstract|sealed)\s+)*def\s+(\w+)\s*(?:\(([^)]*)\))?\s*(?::\s*[^={\n]+?)?\s*=\s*\{/;

    const isDefBlock = defBlockPattern.test(rawDef.trim());
    const defM = !isDefBlock ? rawDef.match(defPattern) : null;
    const defBlockM = isDefBlock ? rawDef.match(defBlockPattern) : null;

    const ctx = currentCtx();
    const fields = currentFields();

    if (defBlockM) {
      const [, indent, name, rawParams] = defBlockM;
      const params = rawParams ? parseScalaParams(rawParams) : [];
      if (ctx === "object") {
        out.push(`${indent}${name}(${params.join(", ")}) {`);
      } else if (ctx === "class") {
        if (params.length === 0) {
          out.push(`${indent}get ${name}() {`);
        } else {
          out.push(`${indent}${name}(${params.join(", ")}) {`);
        }
      } else {
        out.push(`${indent}function ${name}(${params.join(", ")}) {`);
      }
      depth = newDepth;
      continue;
    }

    if (defM) {
      const [, indent, name, rawParams, body] = defM;
      const params = rawParams != null ? parseScalaParams(rawParams) : [];
      const bodyTrimmed = body.trim();

      // Match expression: "def f(params): T = expr match {"
      if (bodyTrimmed.endsWith("match {")) {
        const matchSubject = bodyTrimmed.slice(0, -7).trim();
        if (ctx === "object") {
          out.push(`${indent}${name}(${params.join(", ")}) {`);
        } else if (ctx === "class") {
          if (params.length === 0) {
            out.push(`${indent}get ${name}() {`);
          } else {
            out.push(`${indent}${name}(${params.join(", ")}) {`);
          }
        } else {
          out.push(`${indent}function ${name}(${params.join(", ")}) {`);
        }
        out.push(`${indent}  const __m = ${applyCommonTransforms(matchSubject, fields, classNames)};`);
        inMatchBlock = true;
        matchVar = "__m";
        matchCloseDepth = depth + opens;
        depth = newDepth;
        continue;
      }

      // Single-expression function
      let jsBody = applyCommonTransforms(bodyTrimmed, fields, classNames);

      // Transform inline if/else to ternary
      if (/^if\s*\(/.test(jsBody)) {
        jsBody = transformInlineIfElse(jsBody);
      }

      // Tuple literal: (a, b) → [a, b]
      if (/^\([^()]+,[^()]+\)$/.test(jsBody)) {
        jsBody = "[" + jsBody.slice(1, -1) + "]";
      }

      if (ctx === "object") {
        // In object: method shorthand with trailing comma
        // Closing brace is handled by the object's }
        out.push(`${indent}${name}(${params.join(", ")}) { return ${jsBody}; },`);
      } else if (ctx === "class") {
        if (params.length === 0) {
          // Property getter
          out.push(`${indent}get ${name}() { return ${jsBody}; }`);
        } else {
          out.push(`${indent}${name}(${params.join(", ")}) { return ${jsBody}; }`);
        }
      } else {
        out.push(`${indent}function ${name}(${params.join(", ")}) { return ${jsBody}; }`);
      }
      depth = newDepth;
      continue;
    }

    // ----- val / var declarations -----
    const valVarM = raw.match(/^(\s*)(val|var)\s+(\w+)\s*(?::\s*[^=\n]+?)?\s*=\s*(.+)$/);
    if (valVarM) {
      const [, indent, kw, varName, rhs] = valVarM;
      const jsKw = kw === "val" ? "const" : "let";
      const jsRhs = applyCommonTransforms(rhs.trim(), fields, classNames);
      out.push(`${indent}${jsKw} ${varName} = ${jsRhs}`);
      depth = newDepth;
      continue;
    }

    // ----- Closing } for object (needs ;) -----
    if (trimmed === "}" && ctx === "object" && depth === ctxStack[ctxStack.length - 1]?.depth) {
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      out.push(`${indent}};`);
      depth = newDepth;
      continue;
    }

    // ----- for loops -----
    // for (i <- 1 to n) { or for (i <- 1 until n) {
    const forRangeM = raw.match(/^(\s*)for\s*\(\s*(\w+)\s*<-\s*(.+?)\s+(to|until)\s+(.+?)\s*\)\s*(\{?)$/);
    if (forRangeM) {
      const [, indent, v, start, kind, end, brace] = forRangeM;
      const op = kind === "to" ? "<=" : "<";
      out.push(`${indent}for (let ${v} = ${start.trim()}; ${v} ${op} ${end.trim()}; ${v}++) ${brace}`);
      depth = newDepth;
      continue;
    }

    // for (x <- list) {
    const forListM = raw.match(/^(\s*)for\s*\(\s*(\w+)\s*<-\s*(.+?)\s*\)\s*(\{?)$/);
    if (forListM) {
      const [, indent, v, iterable, brace] = forListM;
      out.push(`${indent}for (const ${v} of ${iterable.trim()}) ${brace}`);
      depth = newDepth;
      continue;
    }

    // ----- Standalone expression line in function (possible implicit return) -----
    // Simple identifier on its own → return it
    if (/^\s*[a-z_]\w*\s*$/.test(raw) && !["if", "else", "for", "while", "return", "val", "var", "def"].includes(trimmed)) {
      const indent = raw.match(/^(\s*)/)?.[1] ?? "";
      out.push(`${indent}return ${trimmed};`);
      depth = newDepth;
      continue;
    }

    // ----- General line -----
    let line = raw;
    line = applyCommonTransforms(line, fields, classNames);
    out.push(line);
    depth = newDepth;
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Extract declarations (def / case class / class / object) from solution code
// ---------------------------------------------------------------------------

export function extractScalaDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let depth = 0;
  let inDecl = false;
  let hasSeenBrace = false;
  // When a def ends with `=` (body on next line), capture the next non-empty line too
  let pendingBodyLine = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!inDecl) {
      // If we're waiting for the body line of a multiline def
      if (pendingBodyLine) {
        if (trimmed === "") continue; // skip blank lines between def and body
        result.push(line);
        pendingBodyLine = false;
        inDecl = false;
        continue;
      }
      const startsDecl =
        /^(?:(?:override|final|protected|private|abstract|sealed)\s+)*def\s+/.test(trimmed) ||
        /^(?:case\s+class|class|object|trait)\s+/.test(trimmed);
      if (startsDecl) {
        inDecl = true;
        hasSeenBrace = false;
        result.push(line);
        const opens = (line.match(/\{/g) ?? []).length;
        const closes = (line.match(/\}/g) ?? []).length;
        depth += opens - closes;
        if (opens > 0) hasSeenBrace = true;
        // Def ending with `=` and no body on same line: next non-empty line is the body
        if (depth <= 0 && !hasSeenBrace && /=\s*$/.test(line)) {
          inDecl = false;
          depth = 0;
          pendingBodyLine = true;
          continue;
        }
        // Single-line def with no braces: already complete
        if (depth <= 0 && (hasSeenBrace || opens === 0)) { inDecl = false; depth = 0; }
      }
      // Skip top-level executable statements
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

export async function runScala(code: string): Promise<RunResult> {
  try {
    const js = transpileScala(code);
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
      const decls = extractScalaDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }
    const result = await runScala(codeToRun);
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
