import type { Lesson } from "../../types";

export const fibonacciMemoization: Lesson = {
	id: "fibonacci-memoization",
	title: "Fibonacci & Memoization",
	chapterId: "dynamic-programming",
	content: `## Dynamic Programming: Memoization

Dynamic programming (DP) solves problems by breaking them into overlapping subproblems and caching the results to avoid redundant computation.

**Memoization** is the top-down approach: write the recursive solution, then add a cache.

### The Fibonacci Problem

The naive recursive Fibonacci is exponential:

\`\`\`js
function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);  // O(2^n) calls!
}
\`\`\`

For \`fib(50)\`, this makes over **20 trillion** function calls.

### With Memoization

Cache the result of each \`fib(n)\` call so it is only computed once:

\`\`\`js
function fib(n, memo = {}) {
  if (n <= 1) return n;
  if (memo[n] !== undefined) return memo[n];
  memo[n] = fib(n - 1, memo) + fib(n - 2, memo);
  return memo[n];
}
\`\`\`

Now \`fib(50)\` makes only **~100 calls**. Time complexity goes from O(2ⁿ) to **O(n)**.

### The Pattern

Memoization follows three steps:
1. Identify the **base cases**.
2. Check the **cache** before computing.
3. **Store** the result before returning.

This pattern applies to many problems: coin change, longest common subsequence, edit distance, 0/1 knapsack, and more.

### Your Task

Implement \`fib(n)\` using memoization. \`fib(0) = 0\`, \`fib(1) = 1\`, \`fib(n) = fib(n-1) + fib(n-2)\`.`,

	starterCode: `function fib(n, memo = {}) {
	// Base cases: fib(0) = 0, fib(1) = 1
	// Check cache before computing
	// Store result in cache before returning
}

console.log(fib(0));
console.log(fib(1));
console.log(fib(10));
console.log(fib(50));
`,

	solution: `function fib(n, memo = {}) {
	if (n <= 1) return n;
	if (memo[n] !== undefined) return memo[n];
	memo[n] = fib(n - 1, memo) + fib(n - 2, memo);
	return memo[n];
}

console.log(fib(0));
console.log(fib(1));
console.log(fib(10));
console.log(fib(50));
`,

	tests: [
		{
			name: "fib(0)=0, fib(1)=1, fib(10)=55, fib(50)=12586269025",
			expected: "0\n1\n55\n12586269025\n",
		},
		{
			name: "fib(2) = 1",
			code: `{{FUNC}}
console.log(fib(2));`,
			expected: "1\n",
		},
		{
			name: "fib(7) = 13",
			code: `{{FUNC}}
console.log(fib(7));`,
			expected: "13\n",
		},
		{
			name: "fib(20) = 6765",
			code: `{{FUNC}}
console.log(fib(20));`,
			expected: "6765\n",
		},
	],
};
