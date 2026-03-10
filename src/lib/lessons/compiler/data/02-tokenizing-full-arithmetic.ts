import type { Lesson } from "../../types";

export const tokenizingFullArithmetic: Lesson = {
	id: "tokenizing-full-arithmetic",
	title: "Tokenizing Full Arithmetic",
	chapterId: "lexing",
	content: `## Expanding the Tokenizer

Our tokenizer handles \`+\` and \`-\`. Now let's add multiplication and division:

| Type | Matches |
|------|---------|
| \`STAR\` | \`*\` |
| \`SLASH\` | \`/\` |
| \`LPAREN\` | \`(\` |
| \`RPAREN\` | \`)\` |

With these additions, we can tokenize any arithmetic expression like \`(3 + 4) * 2\`.

### Handling Parentheses

Parentheses are single-character tokens, just like operators:

\`\`\`js
if (input[i] === "(") {
    tokens.push({ type: "LPAREN", value: "(" });
    i++;
}
\`\`\`

### Your Task

Extend the tokenizer to handle all six operators/delimiters: \`+\`, \`-\`, \`*\`, \`/\`, \`(\`, \`)\`, plus numbers and whitespace. Return the same \`{ type, value }\` format.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " ") {
\t\t\ti++;
\t\t} else if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet num = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") {
\t\t\t\tnum += input[i];
\t\t\t\ti++;
\t\t\t}
\t\t\ttokens.push({ type: "NUMBER", value: num });
\t\t} else if (input[i] === "+") {
\t\t\ttokens.push({ type: "PLUS", value: "+" });
\t\t\ti++;
\t\t} else if (input[i] === "-") {
\t\t\ttokens.push({ type: "MINUS", value: "-" });
\t\t\ti++;
\t\t}
\t\t// TODO: add *, /, (, )
\t\telse {
\t\t\ti++;
\t\t}
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

function formatTokens(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(formatTokens(tokenize("(3 + 4) * 2")));
console.log(formatTokens(tokenize("10 / 2 - 1")));
`,

	solution: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\tif (input[i] === " ") {
\t\t\ti++;
\t\t} else if (input[i] >= "0" && input[i] <= "9") {
\t\t\tlet num = "";
\t\t\twhile (i < input.length && input[i] >= "0" && input[i] <= "9") {
\t\t\t\tnum += input[i];
\t\t\t\ti++;
\t\t\t}
\t\t\ttokens.push({ type: "NUMBER", value: num });
\t\t} else if (input[i] === "+") {
\t\t\ttokens.push({ type: "PLUS", value: "+" });
\t\t\ti++;
\t\t} else if (input[i] === "-") {
\t\t\ttokens.push({ type: "MINUS", value: "-" });
\t\t\ti++;
\t\t} else if (input[i] === "*") {
\t\t\ttokens.push({ type: "STAR", value: "*" });
\t\t\ti++;
\t\t} else if (input[i] === "/") {
\t\t\ttokens.push({ type: "SLASH", value: "/" });
\t\t\ti++;
\t\t} else if (input[i] === "(") {
\t\t\ttokens.push({ type: "LPAREN", value: "(" });
\t\t\ti++;
\t\t} else if (input[i] === ")") {
\t\t\ttokens.push({ type: "RPAREN", value: ")" });
\t\t\ti++;
\t\t} else {
\t\t\ti++;
\t\t}
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

function formatTokens(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(formatTokens(tokenize("(3 + 4) * 2")));
console.log(formatTokens(tokenize("10 / 2 - 1")));
`,

	tests: [
		{
			name: "tokenizes (3 + 4) * 2 and 10 / 2 - 1",
			expected: "LPAREN:( NUMBER:3 PLUS:+ NUMBER:4 RPAREN:) STAR:* NUMBER:2 EOF:\nNUMBER:10 SLASH:/ NUMBER:2 MINUS:- NUMBER:1 EOF:\n",
		},
		{
			name: "tokenizes nested parens",
			code: `{{FUNC}}
console.log(formatTokens(tokenize("((1))")));`,
			expected: "LPAREN:( LPAREN:( NUMBER:1 RPAREN:) RPAREN:) EOF:\n",
		},
		{
			name: "tokenizes complex expression",
			code: `{{FUNC}}
console.log(formatTokens(tokenize("2 * (3 + 4) / 7")));`,
			expected: "NUMBER:2 STAR:* LPAREN:( NUMBER:3 PLUS:+ NUMBER:4 RPAREN:) SLASH:/ NUMBER:7 EOF:\n",
		},
	],
};
