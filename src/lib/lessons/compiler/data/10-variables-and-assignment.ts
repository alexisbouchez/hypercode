import type { Lesson } from "../../types";

export const variablesAndAssignment: Lesson = {
	id: "variables-and-assignment",
	title: "Variables and Assignment",
	chapterId: "evaluation",
	content: `## Adding Variables

A calculator is nice, but real languages have **variables**. We need:

1. **Assignment**: \`let x = 10;\`
2. **Lookup**: using \`x\` in an expression

### The Environment

An **environment** is a mapping from variable names to values. We can use a plain JavaScript object:

\`\`\`js
const env = {};
env["x"] = 10;
console.log(env["x"]); // 10
\`\`\`

### New AST Nodes

We need two new node types:

- \`LetStatement\`: \`{ type: "LetStatement", name: "x", value: <expr> }\`
- \`Identifier\`: \`{ type: "Identifier", name: "x" }\`

### Parsing \`let\`

When we see the \`let\` keyword, parse: \`let IDENT = expr ;\`

### Evaluating Variables

\`\`\`js
if (node.type === "LetStatement") {
    env[node.name] = evaluate(node.value, env);
    return;
}
if (node.type === "Identifier") {
    return env[node.name];
}
\`\`\`

### Your Task

Build an interpreter that supports \`let\` declarations, variable references in expressions, and a \`print\` statement. Parse a program as a list of statements separated by semicolons.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " " || input[i] === "\\n" || input[i] === "\\t") { i++; }
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
\t\tif (this.peek().type === "LPAREN") {
\t\t\tthis.advance(); const e = this.parseExpr(); this.advance(); return e;
\t\t}
\t\tif (this.peek().type === "IDENT") {
\t\t\treturn { type: "Identifier", name: this.advance().value };
\t\t}
\t\treturn { type: "NumberLiteral", value: Number(this.advance().value) };
\t}
\tparseTerm() {
\t\tlet left = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") {
\t\t\tconst op = this.advance().value;
\t\t\tleft = { type: "BinaryExpr", op, left, right: this.parseFactor() };
\t\t}
\t\treturn left;
\t}
\tparseExpr() {
\t\tlet left = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value;
\t\t\tleft = { type: "BinaryExpr", op, left, right: this.parseTerm() };
\t\t}
\t\treturn left;
\t}

\tparseStatement() {
\t\t// TODO: handle "let x = expr;"
\t\t// TODO: handle "print expr;"
\t\t// Otherwise parse expression statement
\t\tconst expr = this.parseExpr();
\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\treturn expr;
\t}

\tparse() {
\t\tconst stmts = [];
\t\twhile (this.peek().type !== "EOF") {
\t\t\tstmts.push(this.parseStatement());
\t\t}
\t\treturn { type: "Program", body: stmts };
\t}
}

function evaluate(node, env) {
\tif (node.type === "Program") {
\t\tlet result;
\t\tfor (const stmt of node.body) result = evaluate(stmt, env);
\t\treturn result;
\t}
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r;
\t\tif (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r;
\t\tif (node.op === "/") return l / r;
\t}
\t// TODO: handle LetStatement and PrintStatement
\treturn undefined;
}

function interpret(input) {
\tconst tokens = tokenize(input);
\tconst ast = new Parser(tokens).parse();
\treturn evaluate(ast, {});
}

interpret("let x = 10; let y = 20; print x + y;");
interpret("let a = 5; let b = a * 2; print b;");
`,

	solution: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " " || input[i] === "\\n" || input[i] === "\\t") { i++; }
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
\t\tif (this.peek().type === "LPAREN") {
\t\t\tthis.advance(); const e = this.parseExpr(); this.advance(); return e;
\t\t}
\t\tif (this.peek().type === "IDENT") {
\t\t\treturn { type: "Identifier", name: this.advance().value };
\t\t}
\t\treturn { type: "NumberLiteral", value: Number(this.advance().value) };
\t}
\tparseTerm() {
\t\tlet left = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") {
\t\t\tconst op = this.advance().value;
\t\t\tleft = { type: "BinaryExpr", op, left, right: this.parseFactor() };
\t\t}
\t\treturn left;
\t}
\tparseExpr() {
\t\tlet left = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value;
\t\t\tleft = { type: "BinaryExpr", op, left, right: this.parseTerm() };
\t\t}
\t\treturn left;
\t}

\tparseStatement() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "let") {
\t\t\tthis.advance();
\t\t\tconst name = this.advance().value;
\t\t\tthis.advance(); // =
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "LetStatement", name, value };
\t\t}
\t\tif (this.peek().type === "IDENT" && this.peek().value === "print") {
\t\t\tthis.advance();
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "PrintStatement", value };
\t\t}
\t\tconst expr = this.parseExpr();
\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\treturn expr;
\t}

\tparse() {
\t\tconst stmts = [];
\t\twhile (this.peek().type !== "EOF") {
\t\t\tstmts.push(this.parseStatement());
\t\t}
\t\treturn { type: "Program", body: stmts };
\t}
}

function evaluate(node, env) {
\tif (node.type === "Program") {
\t\tlet result;
\t\tfor (const stmt of node.body) result = evaluate(stmt, env);
\t\treturn result;
\t}
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r;
\t\tif (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r;
\t\tif (node.op === "/") return l / r;
\t}
\tif (node.type === "LetStatement") {
\t\tenv[node.name] = evaluate(node.value, env);
\t\treturn;
\t}
\tif (node.type === "PrintStatement") {
\t\tconst val = evaluate(node.value, env);
\t\tconsole.log(val);
\t\treturn val;
\t}
\treturn undefined;
}

function interpret(input) {
\tconst tokens = tokenize(input);
\tconst ast = new Parser(tokens).parse();
\treturn evaluate(ast, {});
}

interpret("let x = 10; let y = 20; print x + y;");
interpret("let a = 5; let b = a * 2; print b;");
`,

	tests: [
		{
			name: "variables and print",
			expected: "30\n10\n",
		},
		{
			name: "variable in expression",
			code: `{{FUNC}}
interpret("let x = 7; let y = x + 3; print y * 2;");`,
			expected: "20\n",
		},
		{
			name: "multiple variables",
			code: `{{FUNC}}
interpret("let a = 100; let b = 50; let c = a - b; print c;");`,
			expected: "50\n",
		},
	],
};
