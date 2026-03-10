import type { Lesson } from "../../types";

export const parenthesizedExpressions: Lesson = {
	id: "parenthesized-expressions",
	title: "Parenthesized Expressions",
	chapterId: "parsing",
	content: `## Parsing Parentheses

Parentheses let users override precedence: \`(2 + 3) * 4\` should give \`20\`, not \`14\`.

In our grammar, parenthesized expressions live at the **factor** level — the tightest binding:

\`\`\`
factor -> NUMBER | '(' expr ')'
\`\`\`

### Implementation

In \`parseFactor\`, check if the current token is \`LPAREN\`. If so, consume it, parse a full expression, then expect \`RPAREN\`:

\`\`\`js
parseFactor() {
    if (this.peek().type === "LPAREN") {
        this.advance(); // consume '('
        const expr = this.parseExpr();
        this.advance(); // consume ')'
        return expr;
    }
    const tok = this.advance();
    return { type: "NumberLiteral", value: Number(tok.value) };
}
\`\`\`

The key insight: inside the parentheses, we call \`parseExpr()\` — the *lowest* precedence level. This means any expression can appear inside parens.

### Your Task

Add parenthesis support to the parser. \`(2 + 3) * 4\` should parse as \`(* (+ 2 3) 4)\`.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " ") { i++; }
\t\telse if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet n = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") { n += input[i++]; }
\t\t\ttokens.push({ type: "NUMBER", value: n });
\t\t}
\t\telse if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse if (input[i] === "*") { tokens.push({ type: "STAR", value: "*" }); i++; }
\t\telse if (input[i] === "/") { tokens.push({ type: "SLASH", value: "/" }); i++; }
\t\telse if (input[i] === "(") { tokens.push({ type: "LPAREN", value: "(" }); i++; }
\t\telse if (input[i] === ")") { tokens.push({ type: "RPAREN", value: ")" }); i++; }
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
\t\t// TODO: handle LPAREN ... RPAREN
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
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

\tparse() { return this.parseExpr(); }
}

function printAST(n) {
\tif (n.type === "NumberLiteral") return String(n.value);
\tif (n.type === "BinaryExpr") return "(" + n.op + " " + printAST(n.left) + " " + printAST(n.right) + ")";
\treturn "?";
}

function run(input) { return printAST(new Parser(tokenize(input)).parse()); }

console.log(run("(2 + 3) * 4"));
console.log(run("10 / (5 - 3)"));
console.log(run("((1 + 2))"));
`,

	solution: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " ") { i++; }
\t\telse if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet n = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") { n += input[i++]; }
\t\t\ttokens.push({ type: "NUMBER", value: n });
\t\t}
\t\telse if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse if (input[i] === "*") { tokens.push({ type: "STAR", value: "*" }); i++; }
\t\telse if (input[i] === "/") { tokens.push({ type: "SLASH", value: "/" }); i++; }
\t\telse if (input[i] === "(") { tokens.push({ type: "LPAREN", value: "(" }); i++; }
\t\telse if (input[i] === ")") { tokens.push({ type: "RPAREN", value: ")" }); i++; }
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
\t\t\tthis.advance();
\t\t\tconst expr = this.parseExpr();
\t\t\tthis.advance();
\t\t\treturn expr;
\t\t}
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
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

\tparse() { return this.parseExpr(); }
}

function printAST(n) {
\tif (n.type === "NumberLiteral") return String(n.value);
\tif (n.type === "BinaryExpr") return "(" + n.op + " " + printAST(n.left) + " " + printAST(n.right) + ")";
\treturn "?";
}

function run(input) { return printAST(new Parser(tokenize(input)).parse()); }

console.log(run("(2 + 3) * 4"));
console.log(run("10 / (5 - 3)"));
console.log(run("((1 + 2))"));
`,

	tests: [
		{
			name: "parses parenthesized expressions",
			expected: "(* (+ 2 3) 4)\n(/ 10 (- 5 3))\n(+ 1 2)\n",
		},
		{
			name: "parens override precedence",
			code: `{{FUNC}}
console.log(run("(1 + 2) * (3 + 4)"));`,
			expected: "(* (+ 1 2) (+ 3 4))\n",
		},
		{
			name: "nested parens with operations",
			code: `{{FUNC}}
console.log(run("((2 + 3) * (4 - 1))"));`,
			expected: "(* (+ 2 3) (- 4 1))\n",
		},
	],
};
