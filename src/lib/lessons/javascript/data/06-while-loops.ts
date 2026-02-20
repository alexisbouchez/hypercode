import type { Lesson } from "../../types";

export const whileLoops: Lesson = {
	id: "while-loops",
	title: "While Loops",
	chapterId: "control-flow",
	content: `## While Loops

A \`while\` loop keeps running as long as its condition is true:

\`\`\`js
let n = 1;
while (n <= 4) {
    console.log(n);
    n++;
}
// prints 1, 2, 3, 4
\`\`\`

Use \`while\` when you do not know in advance how many iterations you need.

### Halving a Number

\`\`\`js
function halvingSteps(n) {
    let steps = 0;
    while (n > 1) {
        n = Math.floor(n / 2);
        steps++;
    }
    return steps;
}
console.log(halvingSteps(8));   // 3  (8→4→2→1)
console.log(halvingSteps(100)); // 6
\`\`\`

### Your Task

Write \`function countDigits(n)\` that returns the number of digits in \`n\`. Use a \`while\` loop that divides by 10 until nothing remains.

Special case: \`countDigits(0)\` should return \`1\`.`,

	starterCode: `function countDigits(n) {
\t// Return the number of digits in n
}

console.log(countDigits(0));
console.log(countDigits(5));
console.log(countDigits(42));
console.log(countDigits(1000));
`,

	solution: `function countDigits(n) {
\tif (n === 0) return 1;
\tlet count = 0;
\twhile (n > 0) {
\t\tcount++;
\t\tn = Math.floor(n / 10);
\t}
\treturn count;
}

console.log(countDigits(0));
console.log(countDigits(5));
console.log(countDigits(42));
console.log(countDigits(1000));
`,

	tests: [
		{
			name: "0→1, 5→1, 42→2, 1000→4",
			expected: "1\n1\n2\n4\n",
		},
		{
			name: "countDigits(999) = 3",
			code: `{{FUNC}}
console.log(countDigits(999));`,
			expected: "3\n",
		},
		{
			name: "countDigits(1) = 1",
			code: `{{FUNC}}
console.log(countDigits(1));`,
			expected: "1\n",
		},
		{
			name: "countDigits(100000) = 6",
			code: `{{FUNC}}
console.log(countDigits(100000));`,
			expected: "6\n",
		},
	],
};
