import type { Lesson } from "../../types";

export const completeLexerClass: Lesson = {
	id: "complete-lexer-class",
	title: "Complete Lexer Class",
	chapterId: "lexing",
	content: `## Building a Lexer Class

So far we've used a standalone function. Real interpreters use a **Lexer class** that encapsulates the scanning state:

\`\`\`js
class Lexer {
    constructor(input) {
        this.input = input;
        this.pos = 0;
        this.tokens = [];
    }

    peek() { return this.input[this.pos]; }
    advance() { return this.input[this.pos++]; }
    isAtEnd() { return this.pos >= this.input.length; }
}
\`\`\`

### Benefits

- **Encapsulation**: All state (\`pos\`, \`input\`) is contained
- **Methods**: \`peek()\`, \`advance()\`, \`isAtEnd()\` make scanning clear
- **Extensibility**: Easy to add new token types

### Your Task

Build a \`Lexer\` class with a \`tokenize()\` method that returns all tokens. Support:
- \`NUMBER\`, \`STRING\`, \`IDENT\`
- \`PLUS\`, \`MINUS\`, \`STAR\`, \`SLASH\`, \`LPAREN\`, \`RPAREN\`
- \`EQUALS\` for \`=\`
- \`SEMICOLON\` for \`;\`
- Whitespace (skip) and \`EOF\`

The class should be reusable: \`new Lexer(input).tokenize()\`.`,

	starterCode: `class Lexer {
\tconstructor(input) {
\t\tthis.input = input;
\t\tthis.pos = 0;
\t}

\tpeek() { return this.input[this.pos]; }
\tadvance() { return this.input[this.pos++]; }
\tisAtEnd() { return this.pos >= this.input.length; }

\tisDigit(ch) { return ch >= "0" && ch <= "9"; }
\tisAlpha(ch) { return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z"); }
\tisAlphaNumeric(ch) { return this.isDigit(ch) || this.isAlpha(ch); }

\ttokenize() {
\t\tconst tokens = [];
\t\twhile (!this.isAtEnd()) {
\t\t\t// TODO: implement scanning
\t\t\tthis.advance();
\t\t}
\t\ttokens.push({ type: "EOF", value: "" });
\t\treturn tokens;
\t}
}

function fmt(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(fmt(new Lexer("x = 10 + 20;").tokenize()));
console.log(fmt(new Lexer('"hi" + "there"').tokenize()));
`,

	solution: `class Lexer {
\tconstructor(input) {
\t\tthis.input = input;
\t\tthis.pos = 0;
\t}

\tpeek() { return this.input[this.pos]; }
\tadvance() { return this.input[this.pos++]; }
\tisAtEnd() { return this.pos >= this.input.length; }

\tisDigit(ch) { return ch >= "0" && ch <= "9"; }
\tisAlpha(ch) { return (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z"); }
\tisAlphaNumeric(ch) { return this.isDigit(ch) || this.isAlpha(ch); }

\ttokenize() {
\t\tconst tokens = [];
\t\twhile (!this.isAtEnd()) {
\t\t\tconst ch = this.peek();
\t\t\tif (ch === " " || ch === "\\t" || ch === "\\n") {
\t\t\t\tthis.advance();
\t\t\t} else if (this.isDigit(ch)) {
\t\t\t\tlet num = "";
\t\t\t\twhile (!this.isAtEnd() && this.isDigit(this.peek())) {
\t\t\t\t\tnum += this.advance();
\t\t\t\t}
\t\t\t\ttokens.push({ type: "NUMBER", value: num });
\t\t\t} else if (ch === '"') {
\t\t\t\tthis.advance();
\t\t\t\tlet str = "";
\t\t\t\twhile (!this.isAtEnd() && this.peek() !== '"') {
\t\t\t\t\tstr += this.advance();
\t\t\t\t}
\t\t\t\tthis.advance();
\t\t\t\ttokens.push({ type: "STRING", value: str });
\t\t\t} else if (this.isAlpha(ch)) {
\t\t\t\tlet name = "";
\t\t\t\twhile (!this.isAtEnd() && this.isAlphaNumeric(this.peek())) {
\t\t\t\t\tname += this.advance();
\t\t\t\t}
\t\t\t\ttokens.push({ type: "IDENT", value: name });
\t\t\t} else if (ch === "+") { tokens.push({ type: "PLUS", value: this.advance() });
\t\t\t} else if (ch === "-") { tokens.push({ type: "MINUS", value: this.advance() });
\t\t\t} else if (ch === "*") { tokens.push({ type: "STAR", value: this.advance() });
\t\t\t} else if (ch === "/") { tokens.push({ type: "SLASH", value: this.advance() });
\t\t\t} else if (ch === "(") { tokens.push({ type: "LPAREN", value: this.advance() });
\t\t\t} else if (ch === ")") { tokens.push({ type: "RPAREN", value: this.advance() });
\t\t\t} else if (ch === "=") { tokens.push({ type: "EQUALS", value: this.advance() });
\t\t\t} else if (ch === ";") { tokens.push({ type: "SEMICOLON", value: this.advance() });
\t\t\t} else {
\t\t\t\tthis.advance();
\t\t\t}
\t\t}
\t\ttokens.push({ type: "EOF", value: "" });
\t\treturn tokens;
\t}
}

function fmt(tokens) {
\treturn tokens.map(t => t.type + ":" + t.value).join(" ");
}

console.log(fmt(new Lexer("x = 10 + 20;").tokenize()));
console.log(fmt(new Lexer('"hi" + "there"').tokenize()));
`,

	tests: [
		{
			name: "tokenizes assignment and string concatenation",
			expected: "IDENT:x EQUALS:= NUMBER:10 PLUS:+ NUMBER:20 SEMICOLON:; EOF:\nSTRING:hi PLUS:+ STRING:there EOF:\n",
		},
		{
			name: "tokenizes all operators",
			code: `{{FUNC}}
console.log(fmt(new Lexer("1 + 2 - 3 * 4 / 5").tokenize()));`,
			expected: "NUMBER:1 PLUS:+ NUMBER:2 MINUS:- NUMBER:3 STAR:* NUMBER:4 SLASH:/ NUMBER:5 EOF:\n",
		},
		{
			name: "tokenizes parenthesized expression with assignment",
			code: `{{FUNC}}
console.log(fmt(new Lexer("y = (1 + 2);").tokenize()));`,
			expected: "IDENT:y EQUALS:= LPAREN:( NUMBER:1 PLUS:+ NUMBER:2 RPAREN:) SEMICOLON:; EOF:\n",
		},
	],
};
