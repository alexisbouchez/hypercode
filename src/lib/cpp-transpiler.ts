/**
 * Hypercode C++ to JavaScript transpiler.
 * Handles the specific subset of C++ used in the Hypercode C++ course.
 */

const CPP_TYPES = [
	"int", "double", "float", "long", "short", "unsigned", "signed",
	"char", "bool", "void", "auto", "string", "size_t",
];

const CPP_KEYWORDS = new Set([
	...CPP_TYPES,
	"class", "struct", "public", "private", "protected",
	"template", "typename", "virtual", "override",
	"return", "if", "else", "for", "while", "do", "switch", "case", "break", "continue",
	"new", "delete", "this", "const", "static", "inline", "explicit",
	"using", "namespace", "nullptr", "true", "false",
]);

/** Strip type prefix from a parameter to get just the name and optional default. */
function stripParamType(param: string): string {
	const trimmed = param.trim();
	if (!trimmed) return "";
	const eqIdx = trimmed.indexOf("=");
	const typePart = eqIdx >= 0 ? trimmed.slice(0, eqIdx) : trimmed;
	const defaultPart = eqIdx >= 0 ? trimmed.slice(eqIdx) : "";
	const cleaned = typePart
		.replace(/<[^>]+>/g, "")
		.replace(/\b(const|volatile|unsigned|signed|long|short|int|double|float|char|bool|void|auto|string|size_t|struct|class)\b/g, "")
		.replace(/[&*]/g, "")
		.trim();
	const words = cleaned.split(/\s+/).filter(Boolean);
	if (words.length === 0) return trimmed;
	return words[words.length - 1] + defaultPart;
}

function stripParams(params: string): string {
	if (!params.trim()) return "";
	return params.split(",").map(stripParamType).filter(Boolean).join(", ");
}

/** Find matching closing brace. openIdx must point to '{'. */
function findMatchingBrace(code: string, openIdx: number): number {
	let depth = 0;
	let inStr = false;
	let strChar = "";
	for (let i = openIdx; i < code.length; i++) {
		const c = code[i];
		if (inStr) {
			if (c === "\\") { i++; continue; }
			if (c === strChar) inStr = false;
			continue;
		}
		if (c === '"' || c === "'") { inStr = true; strChar = c; continue; }
		if (c === "{") depth++;
		else if (c === "}") {
			depth--;
			if (depth === 0) return i;
		}
	}
	return -1;
}

/** Parse a cout statement into a JS output assignment. */
function parseCoutStmt(stmt: string): string {
	let inner = stmt.replace(/^cout/, "").trim();
	if (inner.endsWith(";")) inner = inner.slice(0, -1).trim();

	const parts: string[] = [];
	let current = "";
	let depth = 0;
	let inStr = false;
	let strChar = "";

	for (let i = 0; i < inner.length; i++) {
		const c = inner[i];
		if (inStr) {
			if (c === "\\") { current += c + inner[i + 1]; i++; continue; }
			if (c === strChar) inStr = false;
			current += c;
			continue;
		}
		if (c === '"' || c === "'") { inStr = true; strChar = c; current += c; continue; }
		if (c === "(" || c === "[") { depth++; current += c; continue; }
		if (c === ")" || c === "]") { depth--; current += c; continue; }
		if (c === "<" && inner[i + 1] === "<" && depth === 0) {
			parts.push(current.trim());
			current = "";
			i++;
			continue;
		}
		current += c;
	}
	if (current.trim()) parts.push(current.trim());

	const transformed = parts.filter(Boolean).map((p) => {
		if (p === "endl") return '"\\n"';
		return `__cpp_str(${p})`;
	});

	if (transformed.length === 0) return "";
	return `__out += ${transformed.join(" + ")};`;
}

/** Find and transform all cout statements in a code block. */
function transformCout(code: string): string {
	const result: string[] = [];
	let i = 0;
	while (i < code.length) {
		const coutIdx = code.indexOf("cout", i);
		if (coutIdx === -1) { result.push(code.slice(i)); break; }
		const before = code[coutIdx - 1] ?? " ";
		if (/\w/.test(before)) {
			result.push(code.slice(i, coutIdx + 4));
			i = coutIdx + 4;
			continue;
		}
		result.push(code.slice(i, coutIdx));

		// Collect to end of statement (respecting parens/strings)
		let j = coutIdx + 4;
		let depth = 0;
		let inStr = false;
		let strChar = "";
		while (j < code.length) {
			const c = code[j];
			if (inStr) {
				if (c === "\\") { j += 2; continue; }
				if (c === strChar) inStr = false;
				j++;
				continue;
			}
			if (c === '"' || c === "'") { inStr = true; strChar = c; j++; continue; }
			if (c === "(" || c === "[") { depth++; j++; continue; }
			if (c === ")" || c === "]") { depth--; j++; continue; }
			if (c === ";" && depth === 0) break;
			j++;
		}
		result.push(parseCoutStmt(code.slice(coutIdx, j + 1)));
		i = j + 1;
	}
	return result.join("");
}

/** Transform range-based for loop: `for (const T& x : v)` → `for (let x of v)` */
function transformRangeFor(code: string): string {
	return code.replace(
		/for\s*\(\s*(?:const\s+)?(?:\w+(?:<[^>]+>)?)\s*(?:&)?\s*(\w+)\s*:\s*(\w+)\s*\)/g,
		"for (let $1 of $2)",
	);
}

/** Transform C-style for loop init: `for (int i = 0;` → `for (let i = 0;` */
function transformForInit(code: string): string {
	return code.replace(
		/\bfor\s*\(\s*(?:int|double|float|long|short|unsigned|auto)\s+(\w+)/g,
		"for (let $1",
	);
}

type FuncOverload = {
	startIdx: number;
	endIdx: number;
	rawParams: string;
	strippedParams: string;
	paramCount: number;
	paramTypes: string[];
	transformedBody: string;
};

/** Extract the primary C++ type from a raw parameter string (for overload dispatch). */
function extractCppType(rawParam: string): string {
	if (/\bint\b/.test(rawParam)) return "int";
	if (/\bdouble\b/.test(rawParam)) return "double";
	if (/\bfloat\b/.test(rawParam)) return "float";
	if (/\bstring\b/.test(rawParam)) return "string";
	if (/\bbool\b/.test(rawParam)) return "bool";
	if (/\bchar\b/.test(rawParam)) return "char";
	return "unknown";
}

/** Get a JS runtime type-check expression for a C++ type applied to argName. */
function typeCheckExpr(cppType: string, argName: string): string {
	if (cppType === "int") return `Number.isInteger(${argName})`;
	if (cppType === "double" || cppType === "float") return `typeof ${argName} === "number"`;
	if (cppType === "bool") return `typeof ${argName} === "boolean"`;
	if (cppType === "string" || cppType === "char") return `typeof ${argName} === "string"`;
	return "true";
}

/** Build a JS overload dispatcher for C++ functions with the same name. */
function buildOverloadDispatcher(fname: string, overloads: FuncOverload[]): string {
	const helperDefs: string[] = [];
	for (let idx = 0; idx < overloads.length; idx++) {
		const o = overloads[idx];
		helperDefs.push(`function _${fname}_${idx}(${o.strippedParams}) {${o.transformedBody}}`);
	}

	const maxParams = Math.max(...overloads.map((o) => o.paramCount), 0);
	const dispatchArgs = Array.from({ length: maxParams }, (_, i) => `_a${i}`).join(", ");

	const byCount = new Map<number, Array<{ idx: number; overload: FuncOverload }>>();
	for (let idx = 0; idx < overloads.length; idx++) {
		const o = overloads[idx];
		if (!byCount.has(o.paramCount)) byCount.set(o.paramCount, []);
		byCount.get(o.paramCount)!.push({ idx, overload: o });
	}

	const countGroups = [...byCount.entries()].sort(([a], [b]) => a - b);
	const needsCountCheck = countGroups.length > 1;
	const typePriority: Record<string, number> = { int: 0, bool: 1, double: 2, float: 2, char: 3, string: 4, unknown: 5 };
	const dispatchLines: string[] = [];

	for (const [count, group] of countGroups) {
		const argList = Array.from({ length: count }, (_, i) => `_a${i}`).join(", ");
		if (group.length === 1) {
			const { idx } = group[0];
			if (needsCountCheck) {
				dispatchLines.push(`  if (arguments.length === ${count}) return _${fname}_${idx}(${argList});`);
			} else {
				dispatchLines.push(`  return _${fname}_${idx}(${argList});`);
			}
		} else {
			const sorted = [...group].sort((a, b) => {
				const ap = typePriority[a.overload.paramTypes[0] ?? "unknown"] ?? 5;
				const bp = typePriority[b.overload.paramTypes[0] ?? "unknown"] ?? 5;
				return ap - bp;
			});
			for (let gi = 0; gi < sorted.length; gi++) {
				const { idx, overload: o } = sorted[gi];
				const isLast = gi === sorted.length - 1;
				const type = o.paramTypes[0] ?? "unknown";
				if (isLast) {
					if (needsCountCheck) {
						dispatchLines.push(`  if (arguments.length === ${count}) return _${fname}_${idx}(${argList});`);
					} else {
						dispatchLines.push(`  return _${fname}_${idx}(${argList});`);
					}
				} else {
					const typeCond = typeCheckExpr(type, "_a0");
					const fullCond = needsCountCheck ? `arguments.length === ${count} && ${typeCond}` : typeCond;
					dispatchLines.push(`  if (${fullCond}) return _${fname}_${idx}(${argList});`);
				}
			}
		}
	}

	const dispatcher = `function ${fname}(${dispatchArgs}) {\n${dispatchLines.join("\n")}\n}`;
	return [...helperDefs, dispatcher].join("\n");
}

/** Parse initializer list: `: Animal(n)` or `: x(a), y(b)` */
function parseInitList(
	raw: string,
	knownClasses: Set<string>,
): { superCall: string; memberInits: string } {
	const stripped = raw.replace(/^:\s*/, "").trim();
	const inits: string[] = [];
	let cur = "";
	let depth = 0;
	for (const c of stripped) {
		if (c === "(") { depth++; cur += c; }
		else if (c === ")") { depth--; cur += c; }
		else if (c === "," && depth === 0) { inits.push(cur.trim()); cur = ""; }
		else cur += c;
	}
	if (cur.trim()) inits.push(cur.trim());

	let superCall = "";
	const memberLines: string[] = [];
	for (const init of inits) {
		const p = init.indexOf("(");
		if (p === -1) continue;
		const name = init.slice(0, p).trim();
		const args = init.slice(p + 1, init.lastIndexOf(")")).trim();
		if (knownClasses.has(name)) superCall = `super(${args});`;
		else memberLines.push(`this.${name} = ${args};`);
	}
	return { superCall, memberInits: memberLines.join("\n\t\t") };
}

/** Transform a function/method body (without the outer braces). */
function transformBody(body: string, knownClasses: Set<string>): string {
	let r = body;
	r = r.replace(/this\s*->\s*(\w+)/g, "this.$1");
	// Fix: return ClassName(args) → return new ClassName(args)
	// Fix: = ClassName(args) → = new ClassName(args)
	for (const cls of knownClasses) {
		r = r.replace(new RegExp(`\\breturn\\s+${cls}\\s*\\(`, "g"), `return new ${cls}(`);
		r = r.replace(new RegExp(`=\\s*${cls}\\s*\\(`, "g"), `= new ${cls}(`);
	}
	r = transformCout(r);
	r = r.replace(/\.push_back\s*\(/g, ".push(");
	r = r.replace(/\.size\s*\(\s*\)/g, ".length");
	r = r.replace(/(\w+)\.back\s*\(\s*\)/g, "$1[$1.length - 1]");
	r = r.replace(/\.length\s*\(\s*\)/g, ".length");
	r = transformRangeFor(r);
	r = transformForInit(r);
	r = transformLocalDecls(r, knownClasses);
	return r;
}

/** Transform local variable declarations in a body. */
function transformLocalDecls(code: string, knownClasses: Set<string>): string {
	return code
		.split("\n")
		.map((line) => {
			const t = line.trim();
			const ind = line.slice(0, line.length - line.trimStart().length);

			// Custom class: ClassName var = expr;
			for (const cls of knownClasses) {
				const clsAssign = t.match(new RegExp(`^${cls}\\s+(\\w+)\\s*=\\s*(.+);$`));
				if (clsAssign) return `${ind}let ${clsAssign[1]} = ${clsAssign[2]};`;
			}

			// vector<T> v; → let v = [];
			const vecMatch = t.match(/^vector\s*<[^>]+>\s+(\w+)\s*;$/);
			if (vecMatch) return `${ind}let ${vecMatch[1]} = [];`;

			// vector<T> v = {a,b}; → let v = [a,b];
			const vecInitMatch = t.match(/^vector\s*<[^>]+>\s+(\w+)\s*=\s*\{([^}]*)\}\s*;$/);
			if (vecInitMatch) return `${ind}let ${vecInitMatch[1]} = [${vecInitMatch[2]}];`;

			// Known class: ClassName v; → let v = new ClassName();
			for (const cls of knownClasses) {
				const m1 = t.match(new RegExp(`^${cls}\\s+(\\w+)\\s*;$`));
				if (m1) return `${ind}let ${m1[1]} = new ${cls}();`;
				const m2 = t.match(new RegExp(`^${cls}\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*;$`));
				if (m2) return `${ind}let ${m2[1]} = new ${cls}(${m2[2]});`;
			}

			// int/double/etc. var = expr; → let var = expr;
			const typedMatch = t.match(
				/^(?:int|double|float|bool|char|string|auto|long|short)\s+(\w+)\s*=\s*(.+);$/,
			);
			if (typedMatch) return `${ind}let ${typedMatch[1]} = ${typedMatch[2]};`;

			// int var; → let var = 0;
			const noInitMatch = t.match(/^(?:int|double|float|long|short|unsigned)\s+(\w+)\s*;$/);
			if (noInitMatch) return `${ind}let ${noInitMatch[1]} = 0;`;

			// string var; → let var = "";
			const strNoInit = t.match(/^string\s+(\w+)\s*;$/);
			if (strNoInit) return `${ind}let ${strNoInit[1]} = "";`;

			// bool var; → let var = false;
			const boolNoInit = t.match(/^bool\s+(\w+)\s*;$/);
			if (boolNoInit) return `${ind}let ${boolNoInit[1]} = false;`;

			// int a = 1, b = 2; (multi-decl)
			const multiMatch = t.match(
				/^(?:int|double|float|bool|char|string|auto|long|short)\s+((?:\w+\s*=\s*[^,;]+,?\s*)+);$/,
			);
			if (multiMatch) {
				const decls = multiMatch[1].split(",").map((d) => {
					const parts = d.trim().split("=");
					return parts.length === 2
						? `let ${parts[0].trim()} = ${parts[1].trim()}`
						: `let ${d.trim()} = 0`;
				});
				return `${ind}${decls.join("; ")};`;
			}

			return line;
		})
		.join("\n");
}

/** Transform a class body into JavaScript class body. */
function transformClassBody(
	body: string,
	className: string,
	baseClass: string,
	knownClasses: Set<string>,
): string {
	const lines = body.split("\n");
	const output: string[] = [];
	let i = 0;

	while (i < lines.length) {
		const line = lines[i];
		const t = line.trim();

		if (!t || /^(public|private|protected)\s*:/.test(t)) { i++; continue; }

		const rest = lines.slice(i).join("\n");

		// Constructor: ClassName(params) [: initList] {
		const ctorRe = new RegExp(
			`^${className}\\s*\\(([^)]*)\\)\\s*(?::\\s*([^{]+))?\\{`,
		);
		const ctorM = t.match(ctorRe);
		if (ctorM) {
			const rawParams = ctorM[1] ?? "";
			const initListRaw = ctorM[2] ?? "";
			const strippedParams = stripParams(rawParams);
			const { superCall, memberInits } = initListRaw
				? parseInitList(`:${initListRaw}`, knownClasses)
				: { superCall: "", memberInits: "" };

			const relOpen = rest.indexOf("{");
			const relClose = findMatchingBrace(rest, relOpen);
			const bodyStr = rest.slice(relOpen + 1, relClose);
			const transformedBody = transformBody(bodyStr, knownClasses);

			// For derived classes without an explicit super(), add super()
			const needsSuper = baseClass && !superCall;
			let ctor = `\tconstructor(${strippedParams}) {`;
			if (needsSuper) ctor += `\n\t\tsuper();`;
			if (superCall) ctor += `\n\t\t${superCall}`;
			if (memberInits) ctor += `\n\t\t${memberInits}`;
			ctor += transformedBody + "\t}";
			output.push(ctor);

			const used = rest.slice(0, relClose + 1).split("\n").length;
			i += used;
			continue;
		}

		// Method: [virtual] [Type] name(params) [override] [const] {
		const methRe =
			/^(?:virtual\s+)?(?:(?:int|double|float|bool|void|string|char|auto|[A-Z]\w*)\s+)?(\w+)\s*\(([^)]*)\)\s*(?:override\s*)?(?:const\s*)?\{/;
		const methM = t.match(methRe);
		if (methM && !CPP_KEYWORDS.has(methM[1]) && methM[1] !== className) {
			const mName = methM[1];
			const rawParams = methM[2] ?? "";
			const strippedParams = stripParams(rawParams);
			const relOpen = rest.indexOf("{");
			const relClose = findMatchingBrace(rest, relOpen);
			const bodyStr = rest.slice(relOpen + 1, relClose);
			const transformedBody = transformBody(bodyStr, knownClasses);

			output.push(`\t${mName}(${strippedParams}) {`);
			output.push(transformedBody);
			output.push("\t}");

			const used = rest.slice(0, relClose + 1).split("\n").length;
			i += used;
			continue;
		}

		// Member variable declarations — skip them
		if (/^(?:(?:const\s+)?(?:int|double|float|bool|char|string|auto)\s+\w+(?:\s*,\s*\w+)*\s*(?:=\s*[^;]+)?\s*;)$/.test(t)) {
			i++;
			continue;
		}

		output.push(line);
		i++;
	}
	return output.join("\n");
}

const PREAMBLE = `let __out = "";
function __cpp_str(x) {
  if (typeof x === "boolean") return x ? "1" : "0";
  if (typeof x === "number" && !isFinite(x)) return x > 0 ? "Infinity" : "-Infinity";
  return String(x);
}
const sqrt = Math.sqrt;
const pow = Math.pow;
const abs = Math.abs;
const floor = Math.floor;
const ceil = Math.ceil;
const round = Math.round;
const cos = Math.cos;
const sin = Math.sin;
const tan = Math.tan;
const atan2 = Math.atan2;
const M_PI = Math.PI;
`;

/** Main transpilation entry point. */
export function transpileCpp(source: string): { js: string; error: string } {
	try {
		let code = source;

		// 1. Remove preprocessor directives
		code = code.replace(/^\s*#[^\n]*/gm, "");

		// 2. Remove using namespace
		code = code.replace(/\busing\s+namespace\s+\w+\s*;/g, "");

		// 3. Remove template declarations
		code = code.replace(/^\s*template\s*<[^>]+>\s*\n?/gm, "");

		// 4. Collect class names
		const knownClasses = new Set<string>();
		for (const m of code.matchAll(/\bclass\s+(\w+)/g)) {
			knownClasses.add(m[1]);
		}

		// 5. Transform class definitions
		let result = "";
		let lastIdx = 0;
		const classRe = /\bclass\s+(\w+)(?:\s*:\s*public\s+(\w+))?\s*\{/g;
		let m: RegExpExecArray | null;
		while ((m = classRe.exec(code)) !== null) {
			const className = m[1];
			const baseClass = m[2] ?? "";
			const openBrace = m.index + m[0].length - 1;
			const closeBrace = findMatchingBrace(code, openBrace);
			if (closeBrace === -1) continue;

			result += code.slice(lastIdx, m.index);
			const classBody = code.slice(openBrace + 1, closeBrace);
			const transformedBody = transformClassBody(classBody, className, baseClass, knownClasses);
			const extendsStr = baseClass ? ` extends ${baseClass}` : "";
			result += `class ${className}${extendsStr} {\n${transformedBody}\n}`;

			lastIdx = closeBrace + 1;
			if (code[lastIdx] === ";") lastIdx++;
			classRe.lastIndex = lastIdx;
		}
		result += code.slice(lastIdx);
		code = result;

		// 6. Transform global functions (non-class, non-main), with overload dispatch
		{
			const funcRe =
				/(?:^|(?<=\n))(?:(?:int|double|float|bool|void|string|char|auto|[A-Z]\w*)\s+)(\w+)\s*\(([^)]*)\)\s*\{/gm;
			// First pass: collect all function definitions
			const overloadMap = new Map<string, FuncOverload[]>();
			const allRanges: Array<{ startIdx: number; endIdx: number; fname: string }> = [];
			let fm: RegExpExecArray | null;
			while ((fm = funcRe.exec(code)) !== null) {
				const fname = fm[1];
				if (fname === "main" || CPP_KEYWORDS.has(fname)) continue;

				const openBrace = fm.index + fm[0].length - 1;
				const closeBrace = findMatchingBrace(code, openBrace);
				if (closeBrace === -1) continue;

				const rawParams = fm[2] ?? "";
				const strippedParams = stripParams(rawParams);
				const paramParts = rawParams.split(",").map((p) => p.trim()).filter(Boolean);
				const paramCount = paramParts.length;
				const paramTypes = paramParts.map(extractCppType);
				const bodyStr = code.slice(openBrace + 1, closeBrace);
				const transformedBody = transformBody(bodyStr, knownClasses);

				const overload: FuncOverload = {
					startIdx: fm.index,
					endIdx: closeBrace + 1,
					rawParams,
					strippedParams,
					paramCount,
					paramTypes,
					transformedBody,
				};
				if (!overloadMap.has(fname)) overloadMap.set(fname, []);
				overloadMap.get(fname)!.push(overload);
				allRanges.push({ startIdx: fm.index, endIdx: closeBrace + 1, fname });
			}

			// Second pass: emit code with overload dispatchers where needed
			result = "";
			lastIdx = 0;
			allRanges.sort((a, b) => a.startIdx - b.startIdx);
			const emittedFunctions = new Set<string>();
			for (const range of allRanges) {
				result += code.slice(lastIdx, range.startIdx);
				if (emittedFunctions.has(range.fname)) {
					lastIdx = range.endIdx;
					continue;
				}
				emittedFunctions.add(range.fname);
				const overloads = overloadMap.get(range.fname)!;
				if (overloads.length === 1) {
					const o = overloads[0];
					result += `function ${range.fname}(${o.strippedParams}) {${o.transformedBody}}`;
				} else {
					result += buildOverloadDispatcher(range.fname, overloads);
				}
				lastIdx = range.endIdx;
			}
			result += code.slice(lastIdx);
			code = result;
		}

		// 7. Extract main function
		const mainMatch = code.match(/\bint\s+main\s*\([^)]*\)\s*\{/);
		if (!mainMatch) return { js: "", error: "No main() function found" };

		const mainOpen = code.indexOf("{", mainMatch.index!);
		const mainClose = findMatchingBrace(code, mainOpen);
		if (mainClose === -1) return { js: "", error: "Malformed main()" };

		let mainBody = code.slice(mainOpen + 1, mainClose);
		const beforeMain = code.slice(0, mainMatch.index!);

		// Transform main body
		mainBody = transformBody(mainBody, knownClasses);

		// Remove `return 0;` (or similar) from main body to prevent early JS return
		mainBody = mainBody.replace(/^\s*return\s+\d+\s*;\s*$/gm, "");

		const js = PREAMBLE + beforeMain + "\n" + mainBody;
		return { js, error: "" };
	} catch (e: unknown) {
		return { js: "", error: e instanceof Error ? e.message : String(e) };
	}
}

/** Execute transpiled JS and capture output via __out. */
export function executeCppJs(js: string): { stdout: string; error: string } {
	try {
		const fn = new Function(`${js}\nreturn __out;`);
		const output = fn();
		return { stdout: String(output ?? ""), error: "" };
	} catch (e: unknown) {
		return { stdout: "", error: e instanceof Error ? e.message : String(e) };
	}
}
