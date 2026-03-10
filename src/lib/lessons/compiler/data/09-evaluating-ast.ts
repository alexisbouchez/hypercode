import type { Lesson } from "../../types";

export const evaluatingAST: Lesson = {
	id: "evaluating-ast",
	title: "Evaluating the AST",
	chapterId: "evaluation",
	content: `## Tree-Walk Interpretation

We have tokens and a parse tree. Now let's **evaluate** it! A tree-walk interpreter recursively visits each node and computes its value.

### The Evaluate Function

\`\`\`js
function evaluate(node) {
    if (node.type === "NumberLiteral") {
        return node.value;
    }
    if (node.type === "BinaryExpr") {
        const left = evaluate(node.left);
        const right = evaluate(node.right);
        if (node.op === "+") return left + right;
        if (node.op === "-") return left - right;
        if (node.op === "*") return left * right;
        if (node.op === "/") return left / right;
    }
}
\`\`\`

That's it! The recursion follows the tree structure naturally:
- For \`(+ 2 (* 3 4))\`: evaluate left (\`2\`), evaluate right (\`* 3 4\` = \`12\`), add = \`14\`

### Putting It All Together

Now we have the full pipeline:

\`\`\`
source code  ->  tokenize  ->  parse  ->  evaluate  ->  result
  "2+3*4"      [tokens...]     AST        14
\`\`\`

### Your Task

Combine the tokenizer, parser, and evaluator into a single \`interpret(input)\` function. It should return the numeric result of evaluating the expression.`,

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
\t\tif (this.peek().type === "LPAREN") {
\t\t\tthis.advance();
\t\t\tconst e = this.parseExpr();
\t\t\tthis.advance();
\t\t\treturn e;
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
\tparse() { return this.parseExpr(); }
}

function evaluate(node) {
\t// TODO: recursively evaluate the AST
}

function interpret(input) {
\tconst tokens = tokenize(input);
\tconst ast = new Parser(tokens).parse();
\treturn evaluate(ast);
}

console.log(interpret("2 + 3 * 4"));
console.log(interpret("(2 + 3) * 4"));
console.log(interpret("100 / (5 + 5)"));
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
\t\t\tconst e = this.parseExpr();
\t\t\tthis.advance();
\t\t\treturn e;
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
\tparse() { return this.parseExpr(); }
}

function evaluate(node) {
\tif (node.type === "NumberLiteral") return node.value;
\tif (node.type === "BinaryExpr") {
\t\tconst left = evaluate(node.left);
\t\tconst right = evaluate(node.right);
\t\tif (node.op === "+") return left + right;
\t\tif (node.op === "-") return left - right;
\t\tif (node.op === "*") return left * right;
\t\tif (node.op === "/") return left / right;
\t}
\treturn 0;
}

function interpret(input) {
\tconst tokens = tokenize(input);
\tconst ast = new Parser(tokens).parse();
\treturn evaluate(ast);
}

console.log(interpret("2 + 3 * 4"));
console.log(interpret("(2 + 3) * 4"));
console.log(interpret("100 / (5 + 5)"));
`,

	tests: [
		{
			name: "evaluates expressions correctly",
			expected: "14\n20\n10\n",
		},
		{
			name: "evaluates simple subtraction",
			code: `{{FUNC}}
console.log(interpret("50 - 8"));`,
			expected: "42\n",
		},
		{
			name: "evaluates complex nested expression",
			code: `{{FUNC}}
console.log(interpret("(1 + 2) * (3 + 4) - 1"));`,
			expected: "20\n",
		},
	],
};
