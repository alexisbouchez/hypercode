import type { Lesson } from "../../types";

export const functions: Lesson = {
	id: "functions",
	title: "Functions",
	chapterId: "functions",
	content: `## Functions

Functions are reusable blocks of code. Declare them with the \`function\` keyword:

\`\`\`js
function add(a, b) {
    return a + b;
}

console.log(add(3, 4));   // 7
console.log(add(10, 20)); // 30
\`\`\`

### Default Parameters

Parameters can have default values, used when the caller omits the argument:

\`\`\`js
function greet(name, greeting) {
    if (greeting === undefined) greeting = "Hello";
    return greeting + ", " + name + "!";
}

console.log(greet("Alice"));          // Hello, Alice!
console.log(greet("Bob", "Hi"));      // Hi, Bob!
\`\`\`

Or with the shorthand default syntax:

\`\`\`js
function greet(name, greeting = "Hello") {
    return greeting + ", " + name + "!";
}
\`\`\`

### Return Values

Every function returns a value. Without an explicit \`return\`, it returns \`undefined\`.

\`\`\`js
function square(n) {
    return n * n;
}
console.log(square(5));   // 25
console.log(square(12));  // 144
\`\`\`

### Your Task

Write \`function power(base, exp)\` that returns \`base\` raised to the power \`exp\`. Use a loop — do not use \`Math.pow\`.`,

	starterCode: `function power(base, exp) {
\t// Return base raised to the power exp
}

console.log(power(2, 10));
console.log(power(3, 4));
console.log(power(5, 0));
`,

	solution: `function power(base, exp) {
\tlet result = 1;
\tfor (let i = 0; i < exp; i++) {
\t\tresult *= base;
\t}
\treturn result;
}

console.log(power(2, 10));
console.log(power(3, 4));
console.log(power(5, 0));
`,

	tests: [
		{
			name: "2^10=1024, 3^4=81, 5^0=1",
			expected: "1024\n81\n1\n",
		},
		{
			name: "power(2, 1) = 2",
			code: `{{FUNC}}
console.log(power(2, 1));`,
			expected: "2\n",
		},
		{
			name: "power(10, 3) = 1000",
			code: `{{FUNC}}
console.log(power(10, 3));`,
			expected: "1000\n",
		},
		{
			name: "power(2, 8) = 256",
			code: `{{FUNC}}
console.log(power(2, 8));`,
			expected: "256\n",
		},
	],
};
