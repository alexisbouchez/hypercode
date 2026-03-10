import type { Lesson } from "../../types";

export const closuresAndScoping: Lesson = {
	id: "closures-and-scoping",
	title: "Closures and Scoping",
	chapterId: "advanced",
	content: `## User-Defined Functions and Closures

The most powerful feature of a language is **user-defined functions**. We'll add:

\`\`\`
fn square(x) = x * x;
\`\`\`

### What is a Closure?

A **closure** captures the environment where it was defined. This means a function can "remember" variables from its surrounding scope:

\`\`\`
let a = 10;
fn addA(x) = x + a;
print addA(5);    // prints 15
\`\`\`

### Implementation

A function value stores: \`{ type: "Function", params: [...], body: <expr>, env: {...} }\`

When calling a user-defined function:
1. Create a new environment that extends the closure's captured environment
2. Bind the arguments to parameter names
3. Evaluate the body in this new environment

\`\`\`js
if (node.type === "FnStatement") {
    env[node.name] = {
        type: "Function",
        params: node.params,
        body: node.body,
        env: { ...env }  // capture current environment
    };
}

// In CallExpr evaluation:
const fn = env[node.name];
if (fn && fn.type === "Function") {
    const newEnv = { ...fn.env };
    fn.params.forEach((p, i) => { newEnv[p] = args[i]; });
    return evaluate(fn.body, newEnv);
}
\`\`\`

### Your Task

Add \`fn name(params) = body;\` syntax. Functions should capture their enclosing environment (closures).`,

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
\t\t// TODO: handle "fn name(params) = body;"
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
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r; if (node.op === "/") return l / r;
\t\tif (node.op === ">") return l > r ? 1 : 0; if (node.op === "<") return l < r ? 1 : 0;
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "IfExpr") {
\t\treturn evaluate(node.condition, env) !== 0 ? evaluate(node.thenBranch, env) : evaluate(node.elseBranch, env);
\t}
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t\t// TODO: handle user-defined function calls
\t}
\t// TODO: handle FnStatement
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("fn square(x) = x * x; print square(5);");
interpret("fn add(a, b) = a + b; print add(3, 4);");
interpret("let base = 10; fn addBase(x) = x + base; print addBase(5);");
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
\t\t\tthis.advance();
\t\t\tconst name = this.advance().value;
\t\t\tthis.advance(); // (
\t\t\tconst params = [];
\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\tparams.push(this.advance().value);
\t\t\t\twhile (this.peek().type === "COMMA") { this.advance(); params.push(this.advance().value); }
\t\t\t}
\t\t\tthis.advance(); // )
\t\t\tthis.advance(); // =
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
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r; if (node.op === "/") return l / r;
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
\t}
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("fn square(x) = x * x; print square(5);");
interpret("fn add(a, b) = a + b; print add(3, 4);");
interpret("let base = 10; fn addBase(x) = x + base; print addBase(5);");
`,

	tests: [
		{
			name: "user-defined functions and closures",
			expected: "25\n7\n15\n",
		},
		{
			name: "function with expression body",
			code: `{{FUNC}}
interpret("fn double(x) = x * 2; fn quadruple(x) = double(double(x)); print quadruple(3);");`,
			expected: "12\n",
		},
		{
			name: "closure captures environment",
			code: `{{FUNC}}
interpret("let multiplier = 3; fn mul(x) = x * multiplier; print mul(7);");`,
			expected: "21\n",
		},
	],
};
