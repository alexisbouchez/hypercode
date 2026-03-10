import type { Lesson } from "../../types";

export const builtinFunctions: Lesson = {
	id: "builtin-functions",
	title: "Built-in Functions",
	chapterId: "evaluation",
	content: `## Adding Built-in Functions

Real languages come with built-in functions. Let's add three:

| Function | Behavior |
|----------|----------|
| \`abs(x)\` | Absolute value |
| \`max(a, b)\` | Larger of two values |
| \`min(a, b)\` | Smaller of two values |

### Parsing Function Calls

When we see an identifier followed by \`(\`, it's a function call:

\`\`\`js
parseFactor() {
    if (this.peek().type === "IDENT") {
        const name = this.advance().value;
        if (this.peek().type === "LPAREN") {
            this.advance(); // consume (
            const args = [];
            if (this.peek().type !== "RPAREN") {
                args.push(this.parseExpr());
                while (this.peek().type === "COMMA") {
                    this.advance();
                    args.push(this.parseExpr());
                }
            }
            this.advance(); // consume )
            return { type: "CallExpr", name, args };
        }
        return { type: "Identifier", name };
    }
    // ... numbers, parens
}
\`\`\`

### Evaluating Calls

\`\`\`js
if (node.type === "CallExpr") {
    const args = node.args.map(a => evaluate(a, env));
    if (node.name === "abs") return Math.abs(args[0]);
    if (node.name === "max") return Math.max(args[0], args[1]);
    if (node.name === "min") return Math.min(args[0], args[1]);
}
\`\`\`

### Your Task

Add support for \`abs\`, \`max\`, and \`min\` built-in functions. You will need a \`COMMA\` token type for argument separation.`,

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
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\t// TODO: check for ( to parse function call with args
\t\t\treturn { type: "Identifier", name };
\t\t}
\t\tif (this.peek().type === "LPAREN") {
\t\t\tthis.advance(); const e = this.parseExpr(); this.advance(); return e;
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
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\t// TODO: handle CallExpr for abs, max, min
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("print abs(0 - 42);");
interpret("print max(10, 20);");
interpret("print min(3, 7);");
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
\t\tif (this.peek().type === "IDENT") {
\t\t\tconst name = this.advance().value;
\t\t\tif (this.peek().type === "LPAREN") {
\t\t\t\tthis.advance();
\t\t\t\tconst args = [];
\t\t\t\tif (this.peek().type !== "RPAREN") {
\t\t\t\t\targs.push(this.parseExpr());
\t\t\t\t\twhile (this.peek().type === "COMMA") {
\t\t\t\t\t\tthis.advance();
\t\t\t\t\t\targs.push(this.parseExpr());
\t\t\t\t\t}
\t\t\t\t}
\t\t\t\tthis.advance();
\t\t\t\treturn { type: "CallExpr", name, args };
\t\t\t}
\t\t\treturn { type: "Identifier", name };
\t\t}
\t\tif (this.peek().type === "LPAREN") {
\t\t\tthis.advance(); const e = this.parseExpr(); this.advance(); return e;
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
\t}
\tif (node.type === "LetStatement") { env[node.name] = evaluate(node.value, env); return; }
\tif (node.type === "PrintStatement") { const v = evaluate(node.value, env); console.log(v); return v; }
\tif (node.type === "CallExpr") {
\t\tconst args = node.args.map(a => evaluate(a, env));
\t\tif (node.name === "abs") return Math.abs(args[0]);
\t\tif (node.name === "max") return Math.max(args[0], args[1]);
\t\tif (node.name === "min") return Math.min(args[0], args[1]);
\t}
\treturn undefined;
}

function interpret(input) {
\treturn evaluate(new Parser(tokenize(input)).parse(), {});
}

interpret("print abs(0 - 42);");
interpret("print max(10, 20);");
interpret("print min(3, 7);");
`,

	tests: [
		{
			name: "built-in functions work",
			expected: "42\n20\n3\n",
		},
		{
			name: "abs of positive number",
			code: `{{FUNC}}
interpret("print abs(5);");`,
			expected: "5\n",
		},
		{
			name: "nested function calls in expressions",
			code: `{{FUNC}}
interpret("print max(10, 5) + min(3, 7);");`,
			expected: "13\n",
		},
	],
};
