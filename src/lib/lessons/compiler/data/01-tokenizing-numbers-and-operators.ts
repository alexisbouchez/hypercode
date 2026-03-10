import type { Lesson } from "../../types";

export const tokenizingNumbersAndOperators: Lesson = {
	id: "tokenizing-numbers-and-operators",
	title: "Tokenizing Numbers and Operators",
	chapterId: "lexing",
	content: `## What is a Token?

Before we can interpret code, we need to break raw text into meaningful chunks called **tokens**. This process is called **lexical analysis** (or "lexing").

A token is a small object describing a piece of syntax:

\`\`\`js
{ type: "NUMBER", value: "42" }
{ type: "PLUS", value: "+" }
\`\`\`

### Token Types for Arithmetic

For simple arithmetic like \`3 + 42\`, we need:

| Type | Matches |
|------|---------|
| \`NUMBER\` | \`0\`, \`7\`, \`42\`, \`100\` |
| \`PLUS\` | \`+\` |
| \`MINUS\` | \`-\` |
| \`EOF\` | end of input |

### A Simple Tokenizer

We can write a function that scans one character at a time:

\`\`\`js
function tokenize(input) {
    const tokens = [];
    let i = 0;
    while (i < input.length) {
        if (input[i] >= "0" && input[i] <= "9") {
            let num = "";
            while (i < input.length && input[i] >= "0" && input[i] <= "9") {
                num += input[i];
                i++;
            }
            tokens.push({ type: "NUMBER", value: num });
        } else if (input[i] === "+") {
            tokens.push({ type: "PLUS", value: "+" });
            i++;
        }
        // ... handle other characters
    }
    tokens.push({ type: "EOF", value: "" });
    return tokens;
}
\`\`\`

### Your Task

Write a \`tokenize(input)\` function that handles:
- Multi-digit numbers (\`NUMBER\`)
- \`+\` (\`PLUS\`) and \`-\` (\`MINUS\`)
- Whitespace (skip it)
- Appends an \`EOF\` token at the end

Return an array of \`{ type, value }\` objects. Format each token as \`"TYPE:VALUE"\` and print them separated by spaces.`,

	starterCode: `function tokenize(input) {
\tconst tokens = [];
\tlet i = 0;
\twhile (i < input.length) {
\t\t// TODO: handle digits, +, -, and whitespace
\t\ti++;
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

function formatTokens(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(formatTokens(tokenize("3 + 42")));
console.log(formatTokens(tokenize("100 - 7")));
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

console.log(formatTokens(tokenize("3 + 42")));
console.log(formatTokens(tokenize("100 - 7")));
`,

	tests: [
		{
			name: "tokenizes 3 + 42",
			expected: "NUMBER:3 PLUS:+ NUMBER:42 EOF:\nNUMBER:100 MINUS:- NUMBER:7 EOF:\n",
		},
		{
			name: "tokenizes single number",
			code: `{{FUNC}}
console.log(formatTokens(tokenize("99")));`,
			expected: "NUMBER:99 EOF:\n",
		},
		{
			name: "tokenizes chained operations",
			code: `{{FUNC}}
console.log(formatTokens(tokenize("1 + 2 - 3")));`,
			expected: "NUMBER:1 PLUS:+ NUMBER:2 MINUS:- NUMBER:3 EOF:\n",
		},
	],
};
