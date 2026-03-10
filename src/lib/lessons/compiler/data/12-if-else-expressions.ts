import type { Lesson } from "../../types";

export const ifElseExpressions: Lesson = {
	id: "if-else-expressions",
	title: "If/Else Expressions",
	chapterId: "evaluation",
	content: `## Control Flow with If/Else

Let's add conditional logic. In our language, \`if\`/\`else\` is an **expression** — it returns a value:

\`\`\`
if condition then trueExpr else falseExpr
\`\`\`

For example: \`if 1 then 42 else 0\` evaluates to \`42\` (any non-zero number is truthy).

### Comparison Operators

We need comparison operators to make conditions useful:

| Token | Meaning |
|-------|---------|
| \`>\` (\`GT\`) | Greater than |
| \`<\` (\`LT\`) | Less than |

### New AST Node

\`\`\`js
{
    type: "IfExpr",
    condition: <expr>,
    thenBranch: <expr>,
    elseBranch: <expr>
}
\`\`\`

### Parsing If

\`\`\`js
if (this.peek().value === "if") {
    this.advance(); // consume 'if'
    const condition = this.parseExpr();
    this.advance(); // consume 'then'
    const thenBranch = this.parseExpr();
    this.advance(); // consume 'else'
    const elseBranch = this.parseExpr();
    return { type: "IfExpr", condition, thenBranch, elseBranch };
}
\`\`\`

### Evaluating If

A condition is **truthy** if it's not \`0\`:

\`\`\`js
if (node.type === "IfExpr") {
    const cond = evaluate(node.condition, env);
    return cond !== 0
        ? evaluate(node.thenBranch, env)
        : evaluate(node.elseBranch, env);
}
\`\`\`

### Your Task

Add \`if\`/\`then\`/\`else\` expressions and comparison operators \`>\` and \`<\` to the interpreter.`,

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
\t\t// TODO: add > (GT) and < (LT)
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
\t\t// TODO: handle "if condition then trueExpr else falseExpr"
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\tif (this.peek().type === "LPAREN") {
\t\t\t\tthis.advance();
\t\t\t\tconst args = [];
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
\t\tlet left = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") {
\t\t\tconst op = this.advance().value; left = { type: "BinaryExpr", op, left, right: this.parseFactor() };
\t\t}
\t\treturn left;
\t}
\tparseAddSub() {
\t\tlet left = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value; left = { type: "BinaryExpr", op, left, right: this.parseTerm() };
\t\t}
\t\treturn left;
\t}
\tparseExpr() {
\t\tlet left = this.parseAddSub();
\t\t// TODO: handle > and < comparisons
\t\treturn left;
\t}
\tparseStatement() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "let") {
\t\t\tthis.advance(); const name = this.advance().value; this.advance();
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "LetStatement", name, value };
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
\tparse() {
\t\tconst stmts = [];
\t\twhile (this.peek().type !== "EOF") stmts.push(this.parseStatement());
\t\treturn { type: "Program", body: stmts };
\t}
}

function evaluate(node, env) {
\tif (node.type === "Program") { let r; for (const s of node.body) r = evaluate(s, env); return r; }
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r; if (node.op === "/") return l / r;
\t\t// TODO: handle > and <
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t}
\t// TODO: handle IfExpr
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("let x = 10; print if x > 5 then 1 else 0;");
interpret("let y = 3; print if y < 2 then 10 else 20;");
interpret("print if 0 then 1 else 2;");
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
\t\t\tthis.advance();
\t\t\tconst condition = this.parseExpr();
\t\t\tthis.advance(); // then
\t\t\tconst thenBranch = this.parseExpr();
\t\t\tthis.advance(); // else
\t\t\tconst elseBranch = this.parseExpr();
\t\t\treturn { type: "IfExpr", condition, thenBranch, elseBranch };
\t\t}
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\tif (this.peek().type === "LPAREN") {
\t\t\t\tthis.advance();
\t\t\t\tconst args = [];
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
\t\tlet left = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") {
\t\t\tconst op = this.advance().value; left = { type: "BinaryExpr", op, left, right: this.parseFactor() };
\t\t}
\t\treturn left;
\t}
\tparseAddSub() {
\t\tlet left = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value; left = { type: "BinaryExpr", op, left, right: this.parseTerm() };
\t\t}
\t\treturn left;
\t}
\tparseExpr() {
\t\tlet left = this.parseAddSub();
\t\twhile (this.peek().type === "GT" || this.peek().type === "LT") {
\t\t\tconst op = this.advance().value;
\t\t\tleft = { type: "BinaryExpr", op, left, right: this.parseAddSub() };
\t\t}
\t\treturn left;
\t}
\tparseStatement() {
\t\tif (this.peek().type === "IDENT" && this.peek().value === "let") {
\t\t\tthis.advance(); const name = this.advance().value; this.advance();
\t\t\tconst value = this.parseExpr();
\t\t\tif (this.peek().type === "SEMICOLON") this.advance();
\t\t\treturn { type: "LetStatement", name, value };
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
\tparse() {
\t\tconst stmts = [];
\t\twhile (this.peek().type !== "EOF") stmts.push(this.parseStatement());
\t\treturn { type: "Program", body: stmts };
\t}
}

function evaluate(node, env) {
\tif (node.type === "Program") { let r; for (const s of node.body) r = evaluate(s, env); return r; }
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "Identifier") return env[node.name];
\tif (node.type === "BinaryExpr") {
\t\tconst l = evaluate(node.left, env), r = evaluate(node.right, env);
\t\tif (node.op === "+") return l + r; if (node.op === "-") return l - r;
\t\tif (node.op === "*") return l * r; if (node.op === "/") return l / r;
\t\tif (node.op === ">") return l > r ? 1 : 0;
\t\tif (node.op === "<") return l < r ? 1 : 0;
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t}
\tif (node.type === "IfExpr") {
\t\tconst cond = evaluate(node.condition, env);
\t\treturn cond !== 0 ? evaluate(node.thenBranch, env) : evaluate(node.elseBranch, env);
\t}
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("let x = 10; print if x > 5 then 1 else 0;");
interpret("let y = 3; print if y < 2 then 10 else 20;");
interpret("print if 0 then 1 else 2;");
`,

	tests: [
		{
			name: "if/else with comparisons",
			expected: "1\n20\n2\n",
		},
		{
			name: "if/else in variable assignment",
			code: `{{FUNC}}
interpret("let x = 15; let result = if x > 10 then x * 2 else x; print result;");`,
			expected: "30\n",
		},
		{
			name: "nested if/else",
			code: `{{FUNC}}
interpret("let n = 5; print if n > 10 then 3 else if n > 3 then 2 else 1;");`,
			expected: "2\n",
		},
	],
};
