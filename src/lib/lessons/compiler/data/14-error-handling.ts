import type { Lesson } from "../../types";

export const errorHandling: Lesson = {
	id: "error-handling",
	title: "Error Handling",
	chapterId: "advanced",
	content: `## Graceful Error Handling

A robust interpreter should report errors clearly instead of crashing. We need to handle:

### Types of Errors

| Error | Example | Message |
|-------|---------|---------|
| Undefined variable | \`print x;\` | \`Error: undefined variable 'x'\` |
| Division by zero | \`print 10 / 0;\` | \`Error: division by zero\` |
| Undefined function | \`print foo(1);\` | \`Error: undefined function 'foo'\` |

### Implementation Strategy

Instead of returning \`undefined\` or crashing, we'll throw descriptive errors and catch them at the top level:

\`\`\`js
if (node.type === "Identifier") {
    if (!(node.name in env)) {
        throw new Error("undefined variable '" + node.name + "'");
    }
    return env[node.name];
}
\`\`\`

For division by zero:

\`\`\`js
if (node.op === "/") {
    if (r === 0) throw new Error("division by zero");
    return l / r;
}
\`\`\`

### Catching Errors

Wrap the top-level interpret call:

\`\`\`js
function interpret(input) {
    try {
        // tokenize, parse, evaluate
    } catch (e) {
        console.log(e.message);
    }
}
\`\`\`

### Your Task

Add error handling for undefined variables, division by zero, and undefined function calls. The interpreter should print the error message and continue to the next program.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " " || input[i] === "\\n") { i++; }
\t\telse if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet n = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") { n += input[i++]; }
\t\t\ttokens.push({ type: "NUMBER", value: n });
\t\t}
\t\telse if ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z")) {
\t\t\tlet w = "";
\t\t\twhile (i < input.length && ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z") || (input[i] >= "0" && input[i] <= "9"))) { w += input[i++]; }
\t\t\ttokens.push({ type: "IDENT", value: w });
\t\t}
\t\telse if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse if (input[i] === "*") { tokens.push({ type: "STAR", value: "*" }); i++; }
\t\telse if (input[i] === "/") { tokens.push({ type: "SLASH", value: "/" }); i++; }
\t\telse if (input[i] === "(") { tokens.push({ type: "LPAREN", value: "(" }); i++; }
\t\telse if (input[i] === ")") { tokens.push({ type: "RPAREN", value: ")" }); i++; }
\t\telse if (input[i] === "=") { tokens.push({ type: "EQUALS", value: "=" }); i++; }
\t\telse if (input[i] === ";") { tokens.push({ type: "SEMICOLON", value: ";" }); i++; }
\t\telse if (input[i] === ",") { tokens.push({ type: "COMMA", value: "," }); i++; }
\t\telse if (input[i] === ">") { tokens.push({ type: "GT", value: ">" }); i++; }
\t\telse if (input[i] === "<") { tokens.push({ type: "LT", value: "<" }); i++; }
\t\telse { i++; }
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

class Parser {
\tconstructor(tokens) { this.tokens = tokens; this.pos = 0; }
\tpeek() { return this.tokens[this.pos]; }
\tadvance() { return this.tokens[this.pos++]; }
\tparseFactor() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "if") {
\t\t\tthis.advance(); const condition = this.parseExpr();
\t\t\tthis.advance(); const thenBranch = this.parseExpr();
\t\t\tthis.advance(); const elseBranch = this.parseExpr();
\t\t\treturn { type: "IfExpr", condition, thenBranch, elseBranch };
\t\t}
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\tif (this.peek().type === "LPAREN") {
\t\t\t\tthis.advance(); const args = [];
\t\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\t\targs.push(this.parseExpr());
\t\t\t\t\twhile (this.peek().type === "COMMA") { this.advance(); args.push(this.parseExpr()); }
\t\t\t\t}
\t\t\t\tthis.advance();
\t\t\t\treturn { type: "CallExpr", name, args };
\t\t\t}
\t\t\treturn { type: "Identifier", name };
\t\t}
\t\tif (this.peek().type === "LPAREN") { this.advance(); const e = this.parseExpr(); this.advance(); return e; }
\t\treturn { type: "NumberLiteral", value: Number(this.advance().value) };
\t}
\tparseTerm() {
\t\tlet l = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseFactor() }; }
\t\treturn l;
\t}
\tparseAddSub() {
\t\tlet l = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseTerm() }; }
\t\treturn l;
\t}
\tparseExpr() {
\t\tlet l = this.parseAddSub();
\t\twhile (this.peek().type === "GT" || this.peek().type === "LT") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseAddSub() }; }
\t\treturn l;
\t}
\tparseStatement() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "let") {
\t\t\tthis.advance(); const name = this.advance().value; this.advance();
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "LetStatement", name, value };
\t\t}
\t\tif (this.peek().type === "IDENT" && this.peek().value === "fn") {
\t\t\tthis.advance(); const name = this.advance().value;
\t\t\tthis.advance(); const params = [];
\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\tparams.push(this.advance().value);
\t\t\t\twhile (this.peek().type === "COMMA") { this.advance(); params.push(this.advance().value); }
\t\t\t}
\t\t\tthis.advance(); this.advance();
\t\t\tconst body = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "FnStatement", name, params, body };
\t\t}
\t\tif (this.peek().type === "IDENT" && this.peek().value === "print") {
\t\t\tthis.advance(); const value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "PrintStatement", value };
\t\t}
\t\tconst expr = this.parseExpr();
\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\treturn expr;
\t}
\tparse() { const s = []; while (this.peek().type !== "EOF") s.push(this.parseStatement()); return { type: "Program", body: s }; }
}

function evaluate(node, env) {
\tif (node.type === "Program") { let r; for (const s of node.body) r = evaluate(s, env); return r; }
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") {
\t\t// TODO: throw error if variable not found
\t\treturn env[node.name];
\t}
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r;
\t\t// TODO: handle division by zero
\t\tif (node.op === "/") return l / r;
\t\tif (node.op === ">") return l > r ? 1 : 0; if (node.op === "<") return l < r ? 1 : 0;
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "FnStatement") {
\t\tenv[node.name] = { type: "Function", params: node.params, body: node.body, env: Object.assign({}, env) };
\t\treturn;
\t}
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "IfExpr") {
\t\treturn evaluate(node.condition, env) !== 0 ? evaluate(node.thenBranch, env) : evaluate(node.elseBranch, env);
\t}
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t\tconst fn = env[node.name];
\t\tif (fn && fn.type === "Function") {
\t\t\tconst newEnv = Object.assign({}, fn.env);
\t\t\tfn.params.forEach(function(p, i) { newEnv[p] = args[i]; });
\t\t\treturn evaluate(fn.body, newEnv);
\t\t}
\t\t// TODO: throw error if function not found
\t}
\treturn undefined;
}

function interpret(input) {
\t// TODO: wrap in try/catch and print error message
\tconst tokens = tokenize(input);
\tconst ast = new Parser(tokens).parse();
\treturn evaluate(ast, {});
}

interpret("print x;");
interpret("print 10 / 0;");
interpret("print foo(1);");
interpret("let x = 42; print x;");
`,

	solution: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " " || input[i] === "\\n") { i++; }
\t\telse if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet n = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") { n += input[i++]; }
\t\t\ttokens.push({ type: "NUMBER", value: n });
\t\t}
\t\telse if ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z")) {
\t\t\tlet w = "";
\t\t\twhile (i < input.length && ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z") || (input[i] >= "0" && input[i] <= "9"))) { w += input[i++]; }
\t\t\ttokens.push({ type: "IDENT", value: w });
\t\t}
\t\telse if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse if (input[i] === "*") { tokens.push({ type: "STAR", value: "*" }); i++; }
\t\telse if (input[i] === "/") { tokens.push({ type: "SLASH", value: "/" }); i++; }
\t\telse if (input[i] === "(") { tokens.push({ type: "LPAREN", value: "(" }); i++; }
\t\telse if (input[i] === ")") { tokens.push({ type: "RPAREN", value: ")" }); i++; }
\t\telse if (input[i] === "=") { tokens.push({ type: "EQUALS", value: "=" }); i++; }
\t\telse if (input[i] === ";") { tokens.push({ type: "SEMICOLON", value: ";" }); i++; }
\t\telse if (input[i] === ",") { tokens.push({ type: "COMMA", value: "," }); i++; }
\t\telse if (input[i] === ">") { tokens.push({ type: "GT", value: ">" }); i++; }
\t\telse if (input[i] === "<") { tokens.push({ type: "LT", value: "<" }); i++; }
\t\telse { i++; }
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

class Parser {
\tconstructor(tokens) { this.tokens = tokens; this.pos = 0; }
\tpeek() { return this.tokens[this.pos]; }
\tadvance() { return this.tokens[this.pos++]; }
\tparseFactor() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "if") {
\t\t\tthis.advance(); const condition = this.parseExpr();
\t\t\tthis.advance(); const thenBranch = this.parseExpr();
\t\t\tthis.advance(); const elseBranch = this.parseExpr();
\t\t\treturn { type: "IfExpr", condition, thenBranch, elseBranch };
\t\t}
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\tif (this.peek().type === "LPAREN") {
\t\t\t\tthis.advance(); const args = [];
\t\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\t\targs.push(this.parseExpr());
\t\t\t\t\twhile (this.peek().type === "COMMA") { this.advance(); args.push(this.parseExpr()); }
\t\t\t\t}
\t\t\t\tthis.advance();
\t\t\t\treturn { type: "CallExpr", name, args };
\t\t\t}
\t\t\treturn { type: "Identifier", name };
\t\t}
\t\tif (this.peek().type === "LPAREN") { this.advance(); const e = this.parseExpr(); this.advance(); return e; }
\t\treturn { type: "NumberLiteral", value: Number(this.advance().value) };
\t}
\tparseTerm() {
\t\tlet l = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseFactor() }; }
\t\treturn l;
\t}
\tparseAddSub() {
\t\tlet l = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseTerm() }; }
\t\treturn l;
\t}
\tparseExpr() {
\t\tlet l = this.parseAddSub();
\t\twhile (this.peek().type === "GT" || this.peek().type === "LT") { const op = this.advance().value; l = { type: "BinaryExpr", op, left: l, right: this.parseAddSub() }; }
\t\treturn l;
\t}
\tparseStatement() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "let") {
\t\t\tthis.advance(); const name = this.advance().value; this.advance();
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "LetStatement", name, value };
\t\t}
\t\tif (this.peek().type === "IDENT" && this.peek().value === "fn") {
\t\t\tthis.advance(); const name = this.advance().value;
\t\t\tthis.advance(); const params = [];
\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\tparams.push(this.advance().value);
\t\t\t\twhile (this.peek().type === "COMMA") { this.advance(); params.push(this.advance().value); }
\t\t\t}
\t\t\tthis.advance(); this.advance();
\t\t\tconst body = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "FnStatement", name, params, body };
\t\t}
\t\tif (this.peek().type === "IDENT" && this.peek().value === "print") {
\t\t\tthis.advance(); const value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "PrintStatement", value };
\t\t}
\t\tconst expr = this.parseExpr();
\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\treturn expr;
\t}
\tparse() { const s = []; while (this.peek().type !== "EOF") s.push(this.parseStatement()); return { type: "Program", body: s }; }
}

function evaluate(node, env) {
\tif (node.type === "Program") { let r; for (const s of node.body) r = evaluate(s, env); return r; }
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") {
\t\tif (!(node.name in env)) throw new Error("undefined variable '" + node.name + "'");
\t\treturn env[node.name];
\t}
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r;
\t\tif (node.op === "/") {
\t\t\tif (r === 0) throw new Error("division by zero");
\t\t\treturn l / r;
\t\t}
\t\tif (node.op === ">") return l > r ? 1 : 0; if (node.op === "<") return l < r ? 1 : 0;
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "FnStatement") {
\t\tenv[node.name] = { type: "Function", params: node.params, body: node.body, env: Object.assign({}, env) };
\t\treturn;
\t}
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "IfExpr") {
\t\treturn evaluate(node.condition, env) !== 0 ? evaluate(node.thenBranch, env) : evaluate(node.elseBranch, env);
\t}
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t\tconst fn = env[node.name];
\t\tif (fn && fn.type === "Function") {
\t\t\tconst newEnv = Object.assign({}, fn.env);
\t\t\tfn.params.forEach(function(p, i) { newEnv[p] = args[i]; });
\t\t\treturn evaluate(fn.body, newEnv);
\t\t}
\t\tthrow new Error("undefined function '" + node.name + "'");
\t}
\treturn undefined;
}

function interpret(input) {
\ttry {
\t\tconst tokens = tokenize(input);
\t\tconst ast = new Parser(tokens).parse();
\t\treturn evaluate(ast, {});
\t} catch (e) {
\t\tconsole.log(e.message);
\t}
}

interpret("print x;");
interpret("print 10 / 0;");
interpret("print foo(1);");
interpret("let x = 42; print x;");
`,

	tests: [
		{
			name: "catches undefined variable, division by zero, undefined function",
			expected: "undefined variable 'x'\ndivision by zero\nundefined function 'foo'\n42\n",
		},
		{
			name: "error in let does not crash",
			code: `{{FUNC}}
interpret("let a = y + 1;");`,
			expected: "undefined variable 'y'\n",
		},
		{
			name: "valid code after error works",
			code: `{{FUNC}}
interpret("print z;");
interpret("let z = 99; print z;");`,
			expected: "undefined variable 'z'\n99\n",
		},
	],
};
