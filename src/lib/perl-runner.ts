import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let perlReady = false;

export function isPerlReady(): boolean {
  return perlReady;
}

export async function initPerlRunner(): Promise<void> {
  perlReady = true;
}

// ---------------------------------------------------------------------------
// Perl -> JavaScript transpiler
// Handles the subset of Perl used in the beginner course lessons.
// ---------------------------------------------------------------------------

const PRELUDE = `
const __output = [];
function say(...args) {
  __output.push(args.map(String).join('') + '\\n');
}
function __print(...args) {
  __output.push(args.map(String).join(''));
}
function __chomp(s) { return s.replace(/\\n$/, ''); }
function __length(s) { return typeof s === 'string' ? s.length : s; }
function __substr(s, start, len) {
  if (len === undefined) return s.substring(start);
  return s.substring(start, start + len);
}
function __uc(s) { return s.toUpperCase(); }
function __lc(s) { return s.toLowerCase(); }
function __join(sep, arr) { return arr.join(sep); }
function __split(sep, str) {
  if (typeof sep === 'string') return str.split(sep);
  return str.split(sep);
}
function __push(arr, ...vals) { arr.push(...vals); return arr.length; }
function __pop(arr) { return arr.pop(); }
function __shift(arr) { return arr.shift(); }
function __unshift(arr, ...vals) { arr.unshift(...vals); return arr.length; }
function __reverse(arr) { return [...arr].reverse(); }
function __sort(arr) { return [...arr].sort(); }
function __keys(obj) { return Object.keys(obj); }
function __values(obj) { return Object.values(obj); }
function __exists(obj, key) { return key in obj; }
function __delete_key(obj, key) { delete obj[key]; }
function __abs(n) { return Math.abs(n); }
function __int(n) { return Math.trunc(n); }
function __sqrt(n) { return Math.sqrt(n); }
function __chr(n) { return String.fromCharCode(n); }
function __ord(s) { return s.charCodeAt(0); }
function __index(str, substr) { return str.indexOf(substr); }
function __sprintf(fmt, ...args) {
  let i = 0;
  return fmt.replace(/%(-?\\d*\\.?\\d*)?([dsf%])/g, (match, flags, type) => {
    if (type === '%') return '%';
    const val = args[i++];
    if (type === 'd') return String(Math.trunc(Number(val)));
    if (type === 'f') {
      const m = (flags || '').match(/\\.(\\d+)/);
      if (m) return Number(val).toFixed(Number(m[1]));
      return String(Number(val));
    }
    return String(val);
  });
}
function __grep(fn, arr) { return arr.filter(fn); }
function __map(fn, arr) { return arr.map(fn); }
function __scalar(arr) { return arr.length; }
`;

// ---------------------------------------------------------------------------
// String interpolation: "Hello $name" -> template literal
// Handles $var, $var[idx], $var{key}, and ${expr} inside double-quoted strings.
// ---------------------------------------------------------------------------

function transformStringInterpolation(line: string): string {
  let result = "";
  let i = 0;
  while (i < line.length) {
    if (line[i] === '"') {
      i++; // skip opening "
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
            // ${expr}
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
            // $varname possibly followed by [idx] or {key}
            let name = "";
            while (i < line.length && /\w/.test(line[i])) name += line[i++];
            // Check for array/hash access
            if (i < line.length && line[i] === "[") {
              // $arr[idx]
              i++; // skip [
              let idx = "";
              while (i < line.length && line[i] !== "]") idx += line[i++];
              if (i < line.length) i++; // skip ]
              inner += "${" + name + "[" + idx + "]}";
            } else if (i < line.length && line[i] === "{") {
              // $hash{key}
              i++; // skip {
              let key = "";
              while (i < line.length && line[i] !== "}") key += line[i++];
              if (i < line.length) i++; // skip }
              // Quote bare words
              if (!/^["']/.test(key) && !/^\d+$/.test(key)) {
                key = '"' + key + '"';
              }
              inner += "${" + name + "[" + key + "]}";
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
      if (i < line.length) i++; // skip closing "
      if (hasInterp) {
        result += "`" + inner + "`";
      } else {
        result += '"' + inner + '"';
      }
    } else if (line[i] === "'") {
      // Single-quoted string: no interpolation
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
    } else if (line[i] === "#") {
      // Comment -- rest of line
      result += "//" + line.slice(i + 1);
      break;
    } else {
      result += line[i++];
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Strip sigils from variable names
// ---------------------------------------------------------------------------

function stripSigil(name: string): string {
  if (name.startsWith("$") || name.startsWith("@") || name.startsWith("%")) {
    return name.slice(1);
  }
  return name;
}

// ---------------------------------------------------------------------------
// Transform string comparison operators to JS equivalents
// ---------------------------------------------------------------------------

function transformStringOps(line: string): string {
  line = line.replace(/\beq\b/g, "===");
  line = line.replace(/\bne\b/g, "!==");
  line = line.replace(/\blt\b/g, "<");
  line = line.replace(/\bgt\b/g, ">");
  line = line.replace(/\ble\b/g, "<=");
  line = line.replace(/\bge\b/g, ">=");
  return line;
}

// ---------------------------------------------------------------------------
// Replace Perl string concatenation (.) with JS (+)
// Only replace dots that are surrounded by spaces (to avoid matching decimals)
// ---------------------------------------------------------------------------

function transformDotConcat(line: string): string {
  // Replace .= with +=
  line = line.replace(/\s*\.=\s*/g, " += ");
  // Only replace . that is surrounded by spaces: " . "
  line = line.replace(/\s+\.\s+/g, " + ");
  return line;
}

// ---------------------------------------------------------------------------
// Main transpile function
// ---------------------------------------------------------------------------

export function transpilePerl(code: string): string {
  // Remove 'use strict;' and 'use warnings;' and shebang
  const rawLines = code.split("\n");
  const filteredLines: string[] = [];
  for (const l of rawLines) {
    const t = l.trim();
    if (t === "use strict;" || t === "use warnings;" || t.startsWith("#!/")) continue;
    filteredLines.push(l);
  }

  // Phase 0.5: join multi-line expressions
  // If a line ends with ( or , or => and the next line doesn't start a new
  // statement, join them.
  const joinedLines: string[] = [];
  for (let j = 0; j < filteredLines.length; j++) {
    let current = filteredLines[j];
    while (j + 1 < filteredLines.length) {
      const trimCur = current.trimEnd().replace(/;\s*$/, "");
      // Count open/close parens
      const opens = (current.match(/\(/g) ?? []).length;
      const closes = (current.match(/\)/g) ?? []).length;
      if (opens > closes) {
        // Unbalanced -- join with next line
        j++;
        current = current.trimEnd() + " " + filteredLines[j].trim();
      } else {
        break;
      }
    }
    joinedLines.push(current);
  }

  // Phase 1: string interpolation
  const interpLines = joinedLines.map(transformStringInterpolation);

  const lines = interpLines;
  const out: string[] = [PRELUDE];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();
    const indent = line.match(/^(\s*)/)?.[1] ?? "";

    // Skip blank lines
    if (trimmed === "") { out.push(""); continue; }

    // Comments (already transformed by interpolation pass)
    if (trimmed.startsWith("//")) { out.push(line); continue; }

    // Skip lone semicolons
    if (trimmed === ";") continue;

    // Remove trailing semicolons for processing
    const stmt = trimmed.replace(/;\s*$/, "");

    // --- print ---
    const printMatch = stmt.match(/^print\s+(.+)$/);
    if (printMatch) {
      const arg = transformExpr(printMatch[1].trim());
      out.push(`${indent}__print(${arg});`);
      continue;
    }

    // --- say ---
    const sayMatch = stmt.match(/^say\s+(.+)$/);
    if (sayMatch) {
      const arg = transformExpr(sayMatch[1].trim());
      out.push(`${indent}say(${arg});`);
      continue;
    }

    // --- my ($a, $b) = @_ (sub parameter extraction) ---
    const argsMatch = stmt.match(/^my\s+\(([^)]+)\)\s*=\s*@_$/);
    if (argsMatch) {
      const vars = argsMatch[1].split(",").map(v => stripSigil(v.trim()));
      // Look back for the previous function() { line and add params
      for (let j = out.length - 1; j >= 0; j--) {
        const funcMatch = out[j].match(/^(\s*)function\s+(\w+)\(\)\s*\{$/);
        if (funcMatch) {
          const [, fIndent, fName] = funcMatch;
          out[j] = `${fIndent}function ${fName}(${vars.join(", ")}) {`;
          break;
        }
        // Don't look too far back
        if (out.length - j > 5) break;
      }
      continue;
    }

    // --- my $var = @_ (single param) ---
    const singleArgMatch = stmt.match(/^my\s+\$(\w+)\s*=\s*@_$/);
    if (singleArgMatch) {
      const varName = singleArgMatch[1];
      for (let j = out.length - 1; j >= 0; j--) {
        const funcMatch = out[j].match(/^(\s*)function\s+(\w+)\(\)\s*\{$/);
        if (funcMatch) {
          const [, fIndent, fName] = funcMatch;
          out[j] = `${fIndent}function ${fName}(${varName}) {`;
          break;
        }
        if (out.length - j > 5) break;
      }
      continue;
    }

    // --- my @arr = @_ (sub param as array) ---
    const arrayArgMatch = stmt.match(/^my\s+@(\w+)\s*=\s*@_$/);
    if (arrayArgMatch) {
      const varName = arrayArgMatch[1];
      for (let j = out.length - 1; j >= 0; j--) {
        const funcMatch = out[j].match(/^(\s*)function\s+(\w+)\(\)\s*\{$/);
        if (funcMatch) {
          const [, fIndent, fName] = funcMatch;
          out[j] = `${fIndent}function ${fName}(...${varName}) {`;
          break;
        }
        if (out.length - j > 5) break;
      }
      continue;
    }

    // --- my $var = pop @arr ---
    const popDeclMatch = stmt.match(/^my\s+\$(\w+)\s*=\s*pop\s+@(\w+)$/);
    if (popDeclMatch) {
      out.push(`${indent}let ${popDeclMatch[1]} = __pop(${popDeclMatch[2]});`);
      continue;
    }

    // --- my $var = shift @arr ---
    const shiftDeclMatch2 = stmt.match(/^my\s+\$(\w+)\s*=\s*shift\s+@(\w+)$/);
    if (shiftDeclMatch2) {
      out.push(`${indent}let ${shiftDeclMatch2[1]} = __shift(${shiftDeclMatch2[2]});`);
      continue;
    }

    // --- my $var = expr ---
    const scalarDeclMatch = stmt.match(/^my\s+\$(\w+)\s*=\s*(.+)$/);
    if (scalarDeclMatch) {
      const [, name, rhs] = scalarDeclMatch;
      const jsRhs = transformExpr(rhs.trim());
      out.push(`${indent}let ${name} = ${jsRhs};`);
      continue;
    }

    // --- my @arr = expr ---
    const arrayDeclMatch = stmt.match(/^my\s+@(\w+)\s*=\s*(.+)$/);
    if (arrayDeclMatch) {
      const [, name, rhs] = arrayDeclMatch;
      const jsRhs = transformArrayRhs(rhs.trim());
      out.push(`${indent}let ${name} = ${jsRhs};`);
      continue;
    }

    // --- my %hash = (...) ---
    const hashDeclMatch = stmt.match(/^my\s+%(\w+)\s*=\s*(.+)$/);
    if (hashDeclMatch) {
      const [, name, rhs] = hashDeclMatch;
      const jsRhs = transformHashExpr(rhs.trim());
      out.push(`${indent}let ${name} = ${jsRhs};`);
      continue;
    }

    // --- my %hash (uninitialized) ---
    const hashBareDeclMatch = stmt.match(/^my\s+%(\w+)$/);
    if (hashBareDeclMatch) {
      out.push(`${indent}let ${hashBareDeclMatch[1]} = {};`);
      continue;
    }

    // --- my ($a, $b) = expr (destructuring) ---
    const multiDeclMatch = stmt.match(/^my\s+\(([^)]+)\)\s*=\s*(.+)$/);
    if (multiDeclMatch) {
      const [, vars, rhs] = multiDeclMatch;
      const varNames = vars.split(",").map(v => stripSigil(v.trim()));
      const jsRhs = transformExpr(rhs.trim());
      out.push(`${indent}let [${varNames.join(", ")}] = ${jsRhs};`);
      continue;
    }

    // --- sub name { ---
    const subMatch = stmt.match(/^sub\s+(\w+)\s*\{$/);
    if (subMatch) {
      out.push(`${indent}function ${subMatch[1]}() {`);
      continue;
    }

    // --- if ---
    const ifMatch = stmt.match(/^if\s*\((.+)\)\s*\{$/);
    if (ifMatch) {
      out.push(`${indent}if (${transformExpr(ifMatch[1])}) {`);
      continue;
    }

    // --- elsif ---
    const elsifMatch = stmt.match(/^}\s*elsif\s*\((.+)\)\s*\{$/);
    if (elsifMatch) {
      out.push(`${indent}} else if (${transformExpr(elsifMatch[1])}) {`);
      continue;
    }

    // --- else ---
    const elseMatch = stmt.match(/^}\s*else\s*\{$/);
    if (elseMatch) {
      out.push(`${indent}} else {`);
      continue;
    }

    // --- unless ---
    const unlessMatch = stmt.match(/^unless\s*\((.+)\)\s*\{$/);
    if (unlessMatch) {
      out.push(`${indent}if (!(${transformExpr(unlessMatch[1])})) {`);
      continue;
    }

    // --- while ---
    const whileMatch = stmt.match(/^while\s*\((.+)\)\s*\{$/);
    if (whileMatch) {
      out.push(`${indent}while (${transformExpr(whileMatch[1])}) {`);
      continue;
    }

    // --- until ---
    const untilMatch = stmt.match(/^until\s*\((.+)\)\s*\{$/);
    if (untilMatch) {
      out.push(`${indent}while (!(${transformExpr(untilMatch[1])})) {`);
      continue;
    }

    // --- for my $i (N..M) { --- (literal range)
    const forRangeMatch = stmt.match(/^for(?:each)?\s+my\s+\$(\w+)\s*\((\d+)\.\.(\d+)\)\s*\{$/);
    if (forRangeMatch) {
      const [, v, start, end] = forRangeMatch;
      out.push(`${indent}for (let ${v} = ${start}; ${v} <= ${end}; ${v}++) {`);
      continue;
    }

    // --- for my $i (expr..expr) { --- (variable range)
    const forRangeExprMatch = stmt.match(/^for(?:each)?\s+my\s+\$(\w+)\s*\((.+?)\.\.(.+?)\)\s*\{$/);
    if (forRangeExprMatch) {
      const [, v, start, end] = forRangeExprMatch;
      out.push(`${indent}for (let ${v} = ${transformExpr(start)}; ${v} <= ${transformExpr(end)}; ${v}++) {`);
      continue;
    }

    // --- foreach my $item (@arr) { ---
    const foreachMatch = stmt.match(/^for(?:each)?\s+my\s+\$(\w+)\s*\(@(\w+)\)\s*\{$/);
    if (foreachMatch) {
      const [, v, arr] = foreachMatch;
      out.push(`${indent}for (const ${v} of ${arr}) {`);
      continue;
    }

    // --- foreach my $item (expr) { ---
    const foreachExprMatch = stmt.match(/^for(?:each)?\s+my\s+\$(\w+)\s*\((.+)\)\s*\{$/);
    if (foreachExprMatch) {
      const [, v, expr] = foreachExprMatch;
      out.push(`${indent}for (const ${v} of ${transformExpr(expr)}) {`);
      continue;
    }

    // --- push @arr, val ---
    const pushMatch = stmt.match(/^push\s+@(\w+)\s*,\s*(.+)$/);
    if (pushMatch) {
      out.push(`${indent}__push(${pushMatch[1]}, ${transformExpr(pushMatch[2])});`);
      continue;
    }

    // --- pop @arr ---
    const popMatch = stmt.match(/^pop\s+@(\w+)$/);
    if (popMatch) {
      out.push(`${indent}__pop(${popMatch[1]});`);
      continue;
    }

    // --- shift @arr ---
    const shiftMatch = stmt.match(/^shift\s+@(\w+)$/);
    if (shiftMatch) {
      out.push(`${indent}__shift(${shiftMatch[1]});`);
      continue;
    }

    // --- unshift @arr, val ---
    const unshiftMatch = stmt.match(/^unshift\s+@(\w+)\s*,\s*(.+)$/);
    if (unshiftMatch) {
      out.push(`${indent}__unshift(${unshiftMatch[1]}, ${transformExpr(unshiftMatch[2])});`);
      continue;
    }

    // --- $hash{key} = val ---
    const hashSetMatch = stmt.match(/^\$(\w+)\{(.+?)\}\s*=\s*(.+)$/);
    if (hashSetMatch) {
      const [, hash, key, val] = hashSetMatch;
      out.push(`${indent}${hash}[${quoteHashKey(key)}] = ${transformExpr(val)};`);
      continue;
    }

    // --- delete $hash{key} ---
    const deleteMatch = stmt.match(/^delete\s+\$(\w+)\{(.+?)\}$/);
    if (deleteMatch) {
      out.push(`${indent}__delete_key(${deleteMatch[1]}, ${quoteHashKey(deleteMatch[2])});`);
      continue;
    }

    // --- $var = expr ---
    const assignMatch = stmt.match(/^\$(\w+)\s*=\s*(.+)$/);
    if (assignMatch) {
      out.push(`${indent}${assignMatch[1]} = ${transformExpr(assignMatch[2].trim())};`);
      continue;
    }

    // --- $var++ / $var-- ---
    const incrMatch = stmt.match(/^\$(\w+)(\+\+|--)$/);
    if (incrMatch) {
      out.push(`${indent}${incrMatch[1]}${incrMatch[2]};`);
      continue;
    }

    // --- $var += expr etc ---
    const compoundAssignMatch = stmt.match(/^\$(\w+)\s*(\+=|-=|\*=|\/=|%=|\.=)\s*(.+)$/);
    if (compoundAssignMatch) {
      const [, name, op, rhs] = compoundAssignMatch;
      const jsRhs = transformExpr(rhs.trim());
      if (op === ".=") {
        out.push(`${indent}${name} += String(${jsRhs});`);
      } else if (op === "+=") {
        out.push(`${indent}${name} += Number(${jsRhs});`);
      } else {
        out.push(`${indent}${name} ${op} ${jsRhs};`);
      }
      continue;
    }

    // --- return expr ---
    const returnMatch = stmt.match(/^return\s+(.+)$/);
    if (returnMatch) {
      out.push(`${indent}return ${transformExpr(returnMatch[1].trim())};`);
      continue;
    }

    // --- return (bare) ---
    if (stmt === "return") {
      out.push(`${indent}return;`);
      continue;
    }

    // --- closing brace ---
    if (stmt === "}") {
      out.push(`${indent}}`);
      continue;
    }

    // --- function call as statement ---
    const funcCallMatch = stmt.match(/^(\w+)\((.*)?\)$/);
    if (funcCallMatch) {
      const [, fn, args] = funcCallMatch;
      const jsArgs = args ? transformExpr(args) : "";
      out.push(`${indent}${fn}(${jsArgs});`);
      continue;
    }

    // --- General line ---
    out.push(`${indent}${transformExpr(stmt)};`);
  }

  out.push('\nconsole.log(__output.join(""))');
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Quote a hash key if it's a bare word
// ---------------------------------------------------------------------------

function quoteHashKey(key: string): string {
  key = key.trim();
  if (/^["']/.test(key)) return key;
  if (/^\d+$/.test(key)) return key;
  if (/^\$/.test(key)) return key.slice(1); // $var -> var
  return `"${key}"`;
}

// ---------------------------------------------------------------------------
// Transform right-hand side for array declarations
// Handles: (1,2,3), split(...), grep {...}, etc.
// ---------------------------------------------------------------------------

function transformArrayRhs(rhs: string): string {
  // If it starts with a function call that returns an array
  if (/^split\s*\(/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^grep\s*\{/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^map\s*\{/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^keys\s+/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^values\s+/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^sort\s+/.test(rhs)) {
    return transformExpr(rhs);
  }
  if (/^reverse\s+/.test(rhs)) {
    return transformExpr(rhs);
  }

  // Parenthesized list
  let inner = rhs;
  if (inner.startsWith("(") && inner.endsWith(")")) {
    inner = inner.slice(1, -1).trim();
  }

  // Range: 1..5
  const rangeMatch = inner.match(/^(\d+)\.\.(\d+)$/);
  if (rangeMatch) {
    const start = parseInt(rangeMatch[1], 10);
    const end = parseInt(rangeMatch[2], 10);
    const arr = [];
    for (let n = start; n <= end; n++) arr.push(n);
    return `[${arr.join(", ")}]`;
  }

  // Regular list
  const items = splitArgs(inner);
  return `[${items.map(item => transformExpr(item)).join(", ")}]`;
}

// ---------------------------------------------------------------------------
// Transform hash expression: (key => val, key2 => val2) -> {key: val}
// ---------------------------------------------------------------------------

function transformHashExpr(expr: string): string {
  expr = expr.trim();
  if (expr.startsWith("(") && expr.endsWith(")")) {
    expr = expr.slice(1, -1).trim();
  }

  const pairs = splitArgs(expr);
  const entries: string[] = [];
  for (const pair of pairs) {
    const arrowIdx = pair.indexOf("=>");
    if (arrowIdx >= 0) {
      let key = pair.slice(0, arrowIdx).trim();
      const val = pair.slice(arrowIdx + 2).trim();
      if (!key.startsWith('"') && !key.startsWith("'")) {
        key = `"${key}"`;
      }
      entries.push(`${key}: ${transformExpr(val)}`);
    }
  }
  return `{${entries.join(", ")}}`;
}

// ---------------------------------------------------------------------------
// Transform a Perl expression to JavaScript
// ---------------------------------------------------------------------------

function transformExpr(expr: string): string {
  expr = expr.trim();
  if (expr === "") return expr;

  // chomp($var)
  expr = expr.replace(/chomp\s*\(\s*\$(\w+)\s*\)/g, "$1 = $1.replace(/\\n$/, '')");

  // length($var) or length("str")
  expr = expr.replace(/\blength\s*\(\s*\$(\w+)\s*\)/g, "$1.length");
  expr = expr.replace(/\blength\s*\(\s*("[^"]*")\s*\)/g, "$1.length");

  // substr(args) -- use balanced paren extraction
  expr = replaceFunc(expr, "substr", (args) => {
    const parts = splitArgs(args);
    return `__substr(${parts.map(p => transformExpr(p)).join(", ")})`;
  });

  // uc/lc with any argument
  expr = replaceFunc(expr, "uc", (args) => `__uc(${transformExpr(args)})`);
  expr = replaceFunc(expr, "lc", (args) => `__lc(${transformExpr(args)})`);

  // join(sep, @arr)
  expr = replaceFunc(expr, "join", (args) => {
    const parts = splitArgs(args);
    if (parts.length >= 2) {
      const sep = transformExpr(parts[0]);
      const arr = transformExpr(parts[1]);
      return `__join(${sep}, ${arr})`;
    }
    return `__join(${args})`;
  });

  // split(sep, str) -- handle regex and string separators
  expr = expr.replace(/\bsplit\s*\(\s*(\/[^/]+\/)\s*,\s*\$(\w+)\s*\)/g, "__split($1, $2)");
  expr = expr.replace(/\bsplit\s*\(\s*(\/[^/]+\/)\s*,\s*("[^"]*")\s*\)/g, "__split($1, $2)");
  expr = expr.replace(/\bsplit\s*\(\s*("[^"]*"|'[^']*')\s*,\s*\$(\w+)\s*\)/g, "__split($1, $2)");
  expr = expr.replace(/\bsplit\s*\(\s*("[^"]*"|'[^']*')\s*,\s*("[^"]*")\s*\)/g, "__split($1, $2)");

  // scalar(@arr)
  expr = expr.replace(/\bscalar\s*\(\s*@(\w+)\s*\)/g, "$1.length");

  // $#arr + 1 -> arr.length
  expr = expr.replace(/\$#(\w+)\s*\+\s*1/g, "$1.length");
  expr = expr.replace(/\$#(\w+)/g, "($1.length - 1)");

  // keys %hash
  expr = expr.replace(/\bkeys\s+%(\w+)/g, "__keys($1)");

  // values %hash
  expr = expr.replace(/\bvalues\s+%(\w+)/g, "__values($1)");

  // exists $hash{key}
  expr = expr.replace(/\bexists\s+\$(\w+)\{([^}]+)\}/g, (_, hash, key) => {
    return `__exists(${hash}, ${quoteHashKey(key)})`;
  });

  // defined($var)
  expr = expr.replace(/\bdefined\s*\(\s*\$(\w+)\s*\)/g, "($1 !== undefined && $1 !== null)");

  // abs/int/sqrt
  expr = replaceFunc(expr, "abs", (args) => `__abs(${transformExpr(args)})`);
  expr = expr.replace(/\bint\s*\(\s*([^)]+)\s*\)/g, (_, arg) => `__int(${transformExpr(arg)})`);
  expr = replaceFunc(expr, "sqrt", (args) => `__sqrt(${transformExpr(args)})`);

  // chr/ord
  expr = replaceFunc(expr, "chr", (args) => `__chr(${transformExpr(args)})`);
  expr = replaceFunc(expr, "ord", (args) => `__ord(${transformExpr(args)})`);

  // index(str, substr)
  expr = expr.replace(/\bindex\s*\(([^)]+)\)/g, (_, args) => {
    const parts = splitArgs(args);
    return `__index(${parts.map(p => transformExpr(p)).join(", ")})`;
  });

  // sprintf(fmt, args)
  expr = replaceFunc(expr, "sprintf", (args) => {
    const parts = splitArgs(args);
    return `__sprintf(${parts.map(p => transformExpr(p)).join(", ")})`;
  });

  // grep { ... } @arr
  expr = expr.replace(/\bgrep\s*\{\s*(.+?)\s*\}\s*@(\w+)/g, (_, body, arr) => {
    const jsBody = transformExpr(body.replace(/\$_/g, "__item"));
    return `__grep((__item) => ${jsBody}, ${arr})`;
  });

  // map { ... } @arr
  expr = expr.replace(/\bmap\s*\{\s*(.+?)\s*\}\s*@(\w+)/g, (_, body, arr) => {
    const jsBody = transformExpr(body.replace(/\$_/g, "__item"));
    return `__map((__item) => ${jsBody}, ${arr})`;
  });

  // $hash{key} -> hash["key"]
  expr = expr.replace(/\$(\w+)\{([^}]+)\}/g, (_, hash, key) => {
    return `${hash}[${quoteHashKey(key)}]`;
  });

  // $arr[$idx] -> arr[idx]
  expr = expr.replace(/\$(\w+)\[([^\]]+)\]/g, (_, arr, idx) => {
    return `${arr}[${transformExpr(idx)}]`;
  });

  // @arr -> arr
  expr = expr.replace(/@(\w+)/g, "$1");

  // $var -> var (but not inside template literals or already-transformed)
  expr = expr.replace(/\$(\w+)/g, "$1");

  // String comparison operators
  expr = transformStringOps(expr);

  // Dot concatenation (only " . " with spaces to avoid matching decimals)
  expr = transformDotConcat(expr);

  // and/or/not -> &&/||/!
  expr = expr.replace(/\band\b/g, "&&");
  expr = expr.replace(/\bor\b/g, "||");
  expr = expr.replace(/\bnot\b/g, "!");

  // x operator for string repetition: "str" x N -> "str".repeat(N)
  expr = expr.replace(/("[^"]*"|'[^']*'|`[^`]*`)\s+x\s+(\d+|\w+)/g, "$1.repeat($2)");

  return expr;
}

// ---------------------------------------------------------------------------
// Replace a function call by name, using balanced paren matching
// ---------------------------------------------------------------------------

function replaceFunc(expr: string, name: string, replacer: (args: string) => string): string {
  const pattern = new RegExp(`\\b${name}\\s*\\(`);
  let match = pattern.exec(expr);
  while (match) {
    const start = match.index;
    const parenStart = expr.indexOf("(", start + name.length);
    let depth = 1;
    let j = parenStart + 1;
    while (j < expr.length && depth > 0) {
      if (expr[j] === "(") depth++;
      else if (expr[j] === ")") depth--;
      j++;
    }
    const args = expr.slice(parenStart + 1, j - 1);
    const replacement = replacer(args);
    expr = expr.slice(0, start) + replacement + expr.slice(j);
    match = pattern.exec(expr);
  }
  return expr;
}

// ---------------------------------------------------------------------------
// Split arguments by comma, respecting parens and quotes
// ---------------------------------------------------------------------------

function splitArgs(s: string): string[] {
  const args: string[] = [];
  let depth = 0;
  let current = "";
  let inStr: string | null = null;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (inStr) {
      current += ch;
      if (ch === "\\" && i + 1 < s.length) {
        current += s[++i];
      } else if (ch === inStr) {
        inStr = null;
      }
    } else if (ch === '"' || ch === "'") {
      inStr = ch;
      current += ch;
    } else if (ch === "(" || ch === "[" || ch === "{") {
      depth++;
      current += ch;
    } else if (ch === ")" || ch === "]" || ch === "}") {
      depth--;
      current += ch;
    } else if (ch === "," && depth === 0) {
      args.push(current.trim());
      current = "";
    } else {
      current += ch;
    }
  }
  if (current.trim()) args.push(current.trim());
  return args;
}

// ---------------------------------------------------------------------------
// Extract subroutine declarations from solution code (for test harness)
// ---------------------------------------------------------------------------

export function extractPerlDeclarations(code: string): string {
  const lines = code.split("\n");
  const result: string[] = [];
  let inSub = false;
  let depth = 0;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!inSub) {
      if (/^sub\s+\w+/.test(trimmed)) {
        inSub = true;
        depth = 0;
        result.push(line);
        depth += (line.match(/\{/g) ?? []).length;
        depth -= (line.match(/\}/g) ?? []).length;
        if (depth <= 0 && line.includes("{") && line.includes("}")) {
          inSub = false;
          depth = 0;
        }
      }
    } else {
      result.push(line);
      depth += (line.match(/\{/g) ?? []).length;
      depth -= (line.match(/\}/g) ?? []).length;
      if (depth <= 0) {
        inSub = false;
        depth = 0;
      }
    }
  }
  return result.join("\n").trim();
}

// ---------------------------------------------------------------------------
// Browser runner API
// ---------------------------------------------------------------------------

export async function runPerl(code: string): Promise<RunResult> {
  try {
    const js = transpilePerl(code);
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
      const decls = extractPerlDeclarations(code);
      codeToRun = test.code.replace("{{FUNC}}", decls);
    }
    const result = await runPerl(codeToRun);
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
