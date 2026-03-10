import type { Lesson } from "../../types";

export const parsingAdditionSubtraction: Lesson = {
	id: "parsing-addition-subtraction",
	title: "Parsing Addition and Subtraction",
	chapterId: "parsing",
	content: `## Recursive Descent Parsing

A **recursive descent parser** uses one function per grammar rule. For simple addition/subtraction:

\`\`\`
expr   -> number (('+' | '-') number)*
number -> NUMBER
\`\`\`

This means: parse a number, then keep parsing \`+\` or \`-\` followed by another number.

### Parser Structure

\`\`\`js
class Parser {
    constructor(tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }
    peek() { return this.tokens[this.pos]; }
    advance() { return this.tokens[this.pos++]; }
    expect(type) {
        const tok = this.advance();
        if (tok.type !== type) throw new Error("Expected " + type);
        return tok;
    }
}
\`\`\`

### Parsing an Expression

\`\`\`js
parseExpr() {
    let left = this.parseNumber();
    while (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
        const op = this.advance().value;
        const right = this.parseNumber();
        left = { type: "BinaryExpr", op, left, right };
    }
    return left;
}
\`\`\`

This builds a **left-associative** tree: \`1 + 2 + 3\` becomes \`(+ (+ 1 2) 3)\`.

### Your Task

Write a \`tokenize\` function and a \`Parser\` class that parses addition and subtraction into an AST. Include a \`printAST\` function to display the result.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " ") { i++; }
\t\telse if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet n = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") { n += input[i++]; }
\t\t\ttokens.push({ type: "NUMBER", value: n });
\t\t} else if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse { i++; }
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

class Parser {
\tconstructor(tokens) {
\t\tthis.tokens = tokens;
\t\tthis.pos = 0;
\t}
\tpeek() { return this.tokens[this.pos]; }
\tadvance() { return this.tokens[this.pos++]; }

\tparseNumber() {
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
\t}

\tparseExpr() {
\t\t// TODO: parse addition and subtraction
\t\treturn this.parseNumber();
\t}

\tparse() { return this.parseExpr(); }
}

function printAST(node) {
\tif (node.type === "NumberLiteral") return String(node.value);
\tif (node.type === "BinaryExpr") return "(" + node.op + " " + printAST(node.left) + " " + printAST(node.right) + ")";
\treturn "?";
}

function run(input) {
\treturn printAST(new Parser(tokenize(input)).parse());
}

console.log(run("3 + 4"));
console.log(run("10 - 2 + 5"));
console.log(run("1 + 2 + 3 + 4"));
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
\t\t} else if (input[i] === "+") { tokens.push({ type: "PLUS", value: "+" }); i++; }
\t\telse if (input[i] === "-") { tokens.push({ type: "MINUS", value: "-" }); i++; }
\t\telse { i++; }
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

class Parser {
\tconstructor(tokens) {
\t\tthis.tokens = tokens;
\t\tthis.pos = 0;
\t}
\tpeek() { return this.tokens[this.pos]; }
\tadvance() { return this.tokens[this.pos++]; }

\tparseNumber() {
\t\tconst tok = this.advance();
\t\treturn { type: "NumberLiteral", value: Number(tok.value) };
\t}

\tparseExpr() {
\t\tlet left = this.parseNumber();
\t\twhile (this.peek().type === "PLUS" || this.peek().type === "MINUS") {
\t\t\tconst op = this.advance().value;
\t\t\tconst right = this.parseNumber();
\t\t\tleft = { type: "BinaryExpr", op: op, left: left, right: right };
\t\t}
\t\treturn left;
\t}

\tparse() { return this.parseExpr(); }
}

function printAST(node) {
\tif (node.type === "NumberLiteral") return String(node.value);
\tif (node.type === "BinaryExpr") return "(" + node.op + " " + printAST(node.left) + " " + printAST(node.right) + ")";
\treturn "?";
}

function run(input) {
\treturn printAST(new Parser(tokenize(input)).parse());
}

console.log(run("3 + 4"));
console.log(run("10 - 2 + 5"));
console.log(run("1 + 2 + 3 + 4"));
`,

	tests: [
		{
			name: "parses simple expressions",
			expected: "(+ 3 4)\n(+ (- 10 2) 5)\n(+ (+ (+ 1 2) 3) 4)\n",
		},
		{
			name: "parses single number",
			code: `{{FUNC}}
console.log(run("42"));`,
			expected: "42\n",
		},
		{
			name: "parses subtraction chain",
			code: `{{FUNC}}
console.log(run("100 - 50 - 25"));`,
			expected: "(- (- 100 50) 25)\n",
		},
	],
};
