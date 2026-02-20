import type { Lesson } from "../../types";

export const lcs: Lesson = {
	id: "lcs",
	title: "Longest Common Subsequence",
	chapterId: "dynamic-programming",
	content: `## Longest Common Subsequence

The **Longest Common Subsequence (LCS)** problem: given two strings, find the length of the longest subsequence that appears in both.

A **subsequence** is a sequence derived by deleting some (or no) characters without changing the order. \`"ACE"\` is a subsequence of \`"ABCDE"\`.

### Example

\`\`\`
a = "ABCBDAB"
b = "BDCABA"
LCS = "BCAB" or "BDAB" → length 4
\`\`\`

### The Recurrence

Let \`dp[i][j]\` = length of LCS of \`a[0..i-1]\` and \`b[0..j-1]\`.

- If \`a[i-1] === b[j-1]\`: \`dp[i][j] = dp[i-1][j-1] + 1\`
- Otherwise: \`dp[i][j] = Math.max(dp[i-1][j], dp[i][j-1])\`

Base case: \`dp[i][0] = dp[0][j] = 0\`

\`\`\`js
function lcs(a, b) {
  const m = a.length, n = b.length;
  const dp = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      if (a[i - 1] === b[j - 1]) dp[i][j] = dp[i - 1][j - 1] + 1;
      else dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
    }
  }
  return dp[m][n];
}
\`\`\`

### Complexity

- Time: **O(m × n)** — fill every cell of the DP table once.
- Space: **O(m × n)** — can be optimized to O(n) using two rows.

### Real-World Uses

- **diff tools** (\`git diff\`, \`diff\`) find the LCS to determine minimal changes between files.
- **Bioinformatics** — comparing DNA sequences.
- **Spell checkers** — edit distance (similar algorithm).

### Your Task

Implement \`lcs(a, b)\` that returns the **length** of the longest common subsequence of strings \`a\` and \`b\`.`,

	starterCode: `function lcs(a, b) {
	// Create a (m+1) × (n+1) DP table filled with 0
	// Fill the table using the recurrence:
	//   if characters match: dp[i][j] = dp[i-1][j-1] + 1
	//   otherwise: dp[i][j] = max(dp[i-1][j], dp[i][j-1])
	// Return dp[m][n]
}

console.log(lcs("ABCBDAB", "BDCABA"));
console.log(lcs("AGGTAB", "GXTXAYB"));
console.log(lcs("", "ABC"));
`,

	solution: `function lcs(a, b) {
	const m = a.length, n = b.length;
	const dp = Array.from({ length: m + 1 }, () => new Array(n + 1).fill(0));
	for (let i = 1; i <= m; i++) {
		for (let j = 1; j <= n; j++) {
			if (a[i - 1] === b[j - 1]) dp[i][j] = dp[i - 1][j - 1] + 1;
			else dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
		}
	}
	return dp[m][n];
}

console.log(lcs("ABCBDAB", "BDCABA"));
console.log(lcs("AGGTAB", "GXTXAYB"));
console.log(lcs("", "ABC"));
`,

	tests: [
		{
			name: "LCS of ABCBDAB/BDCABA=4, AGGTAB/GXTXAYB=4, empty string=0",
			expected: "4\n4\n0\n",
		},
		{
			name: "identical strings",
			code: `{{FUNC}}
console.log(lcs("ABC", "ABC"));`,
			expected: "3\n",
		},
		{
			name: "no common characters",
			code: `{{FUNC}}
console.log(lcs("ABC", "XYZ"));`,
			expected: "0\n",
		},
		{
			name: "one character match",
			code: `{{FUNC}}
console.log(lcs("A", "A"));`,
			expected: "1\n",
		},
	],
};
