import type { Lesson } from "../../types";

export const astNodeTypes: Lesson = {
	id: "ast-node-types",
	title: "AST Node Types",
	chapterId: "parsing",
	content: `## Abstract Syntax Trees

Tokens are a flat list. To understand *structure*, we build a **tree**. For \`3 + 4 * 2\`, the tree shows that \`4 * 2\` is grouped together:

\`\`\`
    (+)
   /   \\
  3    (*)
      /   \\
     4     2
\`\`\`

Each node in the tree is called an **AST node**. We represent them as plain objects:

\`\`\`js
// A number literal
{ type: "NumberLiteral", value: 42 }

// A binary operation (left op right)
{ type: "BinaryExpr", op: "+", left: ..., right: ... }
\`\`\`

### Node Types We Need

| Node Type | Fields | Example |
|-----------|--------|---------|
| \`NumberLiteral\` | \`value\` | \`42\` |
| \`StringLiteral\` | \`value\` | \`"hello"\` |
| \`BinaryExpr\` | \`op\`, \`left\`, \`right\` | \`3 + 4\` |

### Printing an AST

To verify our tree, we can write a function that converts it to a string:

\`\`\`js
function printAST(node) {
    if (node.type === "NumberLiteral") return String(node.value);
    if (node.type === "BinaryExpr") {
        return "(" + node.op + " " + printAST(node.left) + " " + printAST(node.right) + ")";
    }
}
\`\`\`

This produces a Lisp-like format: \`(+ 3 (* 4 2))\`.

### Your Task

Write helper functions \`num(n)\`, \`str(s)\`, and \`binop(op, left, right)\` that create AST nodes. Then write \`printAST(node)\` to display them. For \`StringLiteral\`, print the value in double quotes.`,

	starterCode: `function num(n) {
\t// TODO: return a NumberLiteral node
}

function str(s) {
\t// TODO: return a StringLiteral node
}

function binop(op, left, right) {
\t// TODO: return a BinaryExpr node
}

function printAST(node) {
\t// TODO: recursively print the AST
}

// Build: (+ 3 (* 4 2))
const tree = binop("+", num(3), binop("*", num(4), num(2)));
console.log(printAST(tree));

// Build: (+ "hello" " world")
const tree2 = binop("+", str("hello"), str(" world"));
console.log(printAST(tree2));
`,

	solution: `function num(n) {
\treturn { type: "NumberLiteral", value: n };
}

function str(s) {
\treturn { type: "StringLiteral", value: s };
}

function binop(op, left, right) {
\treturn { type: "BinaryExpr", op: op, left: left, right: right };
}

function printAST(node) {
\tif (node.type === "NumberLiteral") return String(node.value);
\tif (node.type === "StringLiteral") return '"' + node.value + '"';
\tif (node.type === "BinaryExpr") {
\t\treturn "(" + node.op + " " + printAST(node.left) + " " + printAST(node.right) + ")";
\t}
\treturn "?";
}

// Build: (+ 3 (* 4 2))
const tree = binop("+", num(3), binop("*", num(4), num(2)));
console.log(printAST(tree));

// Build: (+ "hello" " world")
const tree2 = binop("+", str("hello"), str(" world"));
console.log(printAST(tree2));
`,

	tests: [
		{
			name: "prints AST for arithmetic and strings",
			expected: '(+ 3 (* 4 2))\n(+ "hello" " world")\n',
		},
		{
			name: "prints single number",
			code: `{{FUNC}}
console.log(printAST(num(99)));`,
			expected: "99\n",
		},
		{
			name: "prints nested expression",
			code: `{{FUNC}}
const t = binop("-", binop("+", num(1), num(2)), num(3));
console.log(printAST(t));`,
			expected: "(- (+ 1 2) 3)\n",
		},
	],
};
