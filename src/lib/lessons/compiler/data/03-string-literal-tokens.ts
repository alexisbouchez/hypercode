import type { Lesson } from "../../types";

export const stringLiteralTokens: Lesson = {
	id: "string-literal-tokens",
	title: "String Literal Tokens",
	chapterId: "lexing",
	content: `## Tokenizing Strings

Real languages have more than numbers. Let's add **string literals** — text enclosed in double quotes:

\`\`\`
"hello"   =>  { type: "STRING", value: "hello" }
\`\`\`

### Scanning a String

When we encounter a \`"\`, we consume characters until we find the closing \`"\`:

\`\`\`js
if (input[i] === '"') {
    i++; // skip opening quote
    let str = "";
    while (i < input.length && input[i] !== '"') {
        str += input[i];
        i++;
    }
    i++; // skip closing quote
    tokens.push({ type: "STRING", value: str });
}
\`\`\`

The \`value\` stores the string contents *without* the surrounding quotes.

### Identifiers

We also need **identifiers** — names like \`print\` or \`x\`. An identifier starts with a letter and continues with letters or digits:

\`\`\`js
if (isAlpha(input[i])) {
    let name = "";
    while (i < input.length && isAlphaNumeric(input[i])) {
        name += input[i];
        i++;
    }
    tokens.push({ type: "IDENT", value: name });
}
\`\`\`

### Your Task

Write a \`tokenize(input)\` function that handles: numbers (\`NUMBER\`), strings in double quotes (\`STRING\`), identifiers (\`IDENT\`), \`+\` (\`PLUS\`), whitespace (skip), and \`EOF\`.`,

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
\t\t}
\t\t// TODO: handle strings in double quotes (STRING)
\t\t// TODO: handle identifiers (IDENT)
\t\t// TODO: handle + (PLUS)
\t\telse {
\t\t\ti++;
\t\t}
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

function fmt(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(fmt(tokenize('print "hello"')));
console.log(fmt(tokenize('"foo" + "bar"')));
console.log(fmt(tokenize('x 42')));
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
\t\t} else if (input[i] === '"') {
\t\t\ti++;
\t\t\tlet str = "";
\t\t\twhile (i < input.length && input[i] !== '"') {
\t\t\t\tstr += input[i];
\t\t\t\ti++;
\t\t\t}
\t\t\ti++;
\t\t\ttokens.push({ type: "STRING", value: str });
\t\t} else if ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z")) {
\t\t\tlet name = "";
\t\t\twhile (i < input.length && ((input[i] >= "a" && input[i] <= "z") || (input[i] >= "A" && input[i] <= "Z") || (input[i] >= "0" && input[i] <= "9"))) {
\t\t\t\tname += input[i];
\t\t\t\ti++;
\t\t\t}
\t\t\ttokens.push({ type: "IDENT", value: name });
\t\t} else if (input[i] === "+") {
\t\t\ttokens.push({ type: "PLUS", value: "+" });
\t\t\ti++;
\t\t} else {
\t\t\ti++;
\t\t}
\t}
\ttokens.push({ type: "EOF", value: "" });
\treturn tokens;
}

function fmt(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(fmt(tokenize('print "hello"')));
console.log(fmt(tokenize('"foo" + "bar"')));
console.log(fmt(tokenize('x 42')));
`,

	tests: [
		{
			name: "tokenizes print, strings, identifiers",
			expected: 'IDENT:print STRING:hello EOF:\nSTRING:foo PLUS:+ STRING:bar EOF:\nIDENT:x NUMBER:42 EOF:\n',
		},
		{
			name: "tokenizes multi-word string",
			code: `{{FUNC}}
console.log(fmt(tokenize('"hello world"')));`,
			expected: "STRING:hello world EOF:\n",
		},
		{
			name: "tokenizes identifier with digits",
			code: `{{FUNC}}
console.log(fmt(tokenize('x1 y2')));`,
			expected: "IDENT:x1 IDENT:y2 EOF:\n",
		},
	],
};
