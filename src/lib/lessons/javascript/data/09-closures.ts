import type { Lesson } from "../../types";

export const closures: Lesson = {
	id: "closures",
	title: "Closures",
	chapterId: "functions",
	content: `## Closures

A closure is a function that **remembers** the variables from the scope where it was created, even after that scope has finished.

\`\`\`js
function makeCounter() {
    let count = 0;
    return function() {
        count++;
        return count;
    };
}

const counter = makeCounter();
console.log(counter());  // 1
console.log(counter());  // 2
console.log(counter());  // 3
\`\`\`

Each call to \`counter()\` increments \`count\`. The inner function has a reference to \`count\` — it closes over it. This is a **closure**.

### Function Factories

Closures let you create customized functions:

\`\`\`js
function makeMultiplier(factor) {
    return n => n * factor;
}

const triple = makeMultiplier(3);
const quadruple = makeMultiplier(4);

console.log(triple(5));    // 15
console.log(quadruple(5)); // 20
\`\`\`

Each returned function closes over its own \`factor\`.

### Your Task

Write \`function makeAdder(x)\` that returns a new function. The returned function takes a number \`y\` and returns \`x + y\`.

\`\`\`js
const add5 = makeAdder(5);
console.log(add5(3));   // 8
console.log(add5(10));  // 15
\`\`\``,

	starterCode: `function makeAdder(x) {
\t// Return a function that adds x to its argument
}

const add5 = makeAdder(5);
console.log(add5(3));
console.log(add5(10));
`,

	solution: `function makeAdder(x) {
\treturn function(y) {
\t\treturn x + y;
\t};
}

const add5 = makeAdder(5);
console.log(add5(3));
console.log(add5(10));
`,

	tests: [
		{
			name: "add5(3)=8, add5(10)=15",
			expected: "8\n15\n",
		},
		{
			name: "makeAdder(0) adds 0",
			code: `{{FUNC}}
const add0 = makeAdder(0);
console.log(add0(7));`,
			expected: "7\n",
		},
		{
			name: "makeAdder(100) adds 100",
			code: `{{FUNC}}
const add100 = makeAdder(100);
console.log(add100(1));
console.log(add100(99));`,
			expected: "101\n199\n",
		},
		{
			name: "multiple independent adders",
			code: `{{FUNC}}
const add1 = makeAdder(1);
const add2 = makeAdder(2);
console.log(add1(10));
console.log(add2(10));`,
			expected: "11\n12\n",
		},
	],
};
