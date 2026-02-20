import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays",
	chapterId: "arrays",
	content: `## Arrays

An array is an ordered list of values. Create one with square brackets:

\`\`\`js
const fruits = ["apple", "banana", "cherry"];
\`\`\`

### Accessing Elements

Arrays are zero-indexed — the first element is at index 0:

\`\`\`js
console.log(fruits[0]);  // apple
console.log(fruits[1]);  // banana
console.log(fruits[2]);  // cherry
\`\`\`

### Length

\`\`\`js
console.log(fruits.length);  // 3
\`\`\`

### Modifying Arrays

\`push\` adds to the end, \`pop\` removes from the end:

\`\`\`js
fruits.push("date");
console.log(fruits.length);  // 4
console.log(fruits[3]);      // date

const last = fruits.pop();
console.log(last);            // date
console.log(fruits.length);  // 3
\`\`\`

### Iterating

\`\`\`js
for (const fruit of fruits) {
    console.log(fruit);
}
\`\`\`

### Your Task

Create an array \`primes\` containing \`[2, 3, 5, 7, 11]\`. Print the first element, the last element, and the length.`,

	starterCode: `// Create primes array and print first element, last element, length
`,

	solution: `const primes = [2, 3, 5, 7, 11];
console.log(primes[0]);
console.log(primes[primes.length - 1]);
console.log(primes.length);
`,

	tests: [
		{
			name: "prints 2, 11, 5",
			expected: "2\n11\n5\n",
		},
	],
};
