import type { Lesson } from "../../types";

export const operatorPrecedence: Lesson = {
	id: "operator-precedence",
	title: "Operator Precedence",
	chapterId: "parsing",
	content: `## Operator Precedence

In math, \`*\` and \`/\` bind tighter than \`+\` and \`-\`. The expression \`2 + 3 * 4\` equals \`14\`, not \`20\`.

We encode this in the grammar by using **separate rules** for each precedence level:

\`\`\`
expr   -> term (('+' | '-') term)*
term   -> factor (('*' | '/') factor)*
factor -> NUMBER
\`\`\`

- \`factor\`: the tightest — just numbers
- \`term\`: handles \`*\` and \`/\`
- \`expr\`: handles \`+\` and \`-\`

### Implementation

Each grammar rule becomes a method:

\`\`\`js
parseExpr() {
    let left = this.parseTerm();
    while (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
        const op = this.advance().value;
        left = { type: "BinaryExpr", op, left, right: this.parseTerm() };
    }
    return left;
}

parseTerm() {
    let left = this.parseFactor();
    while (this.peek().type === "STAR" || this.peek().type === "SLASH") {
        const op = this.advance().value;
        left = { type: "BinaryExpr", op, left, right: this.parseFactor() };
    }
    return left;
}
\`\`\`

### Your Task

Write a parser with proper operator precedence. \`2 + 3 * 4\` should parse as \`(+ 2 (* 3 4))\`, not \`(* (+ 2 3) 4)\`.`,

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
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
\t}

\tparseTerm() {
\t\t// TODO: parse * and / with parseFactor
\t\treturn this.parseFactor();
\t}

\tparseExpr() {
\t\t// TODO: parse + and - with parseTerm
\t\treturn this.parseTerm();
\t}

\tparse() { return this.parseExpr(); }
}

function printAST(n) {
\tif (n.type === "NumberLiteral") return String(n.value);
\tif (n.type === "BinaryExpr") return "(" + n.op + " " + printAST(n.left) + " " + printAST(n.right) + ")";
\treturn "?";
}

function run(input) { return printAST(new Parser(tokenize(input)).parse()); }

console.log(run("2 + 3 * 4"));
console.log(run("10 - 6 / 2"));
console.log(run("1 + 2 + 3"));
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
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
\t}

\tparseTerm() {
\t\tlet left = this.parseFactor();
\t\twhile (this.peek().type === "STAR" || this.peek().type === "SLASH") {
\t\t\tconst op = this.advance().value;
\t\t\tconst right = this.parseFactor();
\t\t\tleft = { type: "BinaryExpr", op: op, left: left, right: right };
\t\t}
\t\treturn left;
\t}

\tparseExpr() {
\t\tlet left = this.parseTerm();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value;
\t\t\tconst right = this.parseTerm();
\t\t\tleft = { type: "BinaryExpr", op: op, left: left, right: right };
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

console.log(run("2 + 3 * 4"));
console.log(run("10 - 6 / 2"));
console.log(run("1 + 2 + 3"));
`,

	tests: [
		{
			name: "respects operator precedence",
			expected: "(+ 2 (* 3 4))\n(- 10 (/ 6 2))\n(+ (+ 1 2) 3)\n",
		},
		{
			name: "multiplication chain",
			code: `{{FUNC}}
console.log(run("2 * 3 * 4"));`,
			expected: "(* (* 2 3) 4)\n",
		},
		{
			name: "mixed operations",
			code: `{{FUNC}}
console.log(run("1 * 2 + 3 * 4"));`,
			expected: "(+ (* 1 2) (* 3 4))\n",
		},
	],
};
