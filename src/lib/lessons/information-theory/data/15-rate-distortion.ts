import type { Lesson } from "../../types";

export const rateDistortion: Lesson = {
	id: "rate-distortion",
	title: "Distortion Measures",
	chapterId: "channel",
	content: `## Distortion Measures

**Rate-distortion theory** studies the tradeoff between compression rate and reconstruction quality. Central to it are **distortion measures** that quantify how different two strings or sequences are.

### Hamming Distance

The **Hamming distance** counts positions where two equal-length strings differ:

$$d_H(x, y) = \\sum_{i=1}^n \\mathbf{1}[x_i \\neq y_i]$$

Used to measure **bit errors** in digital communications.

### Bit Error Rate

The **BER** normalizes Hamming distance by string length:

$$\\text{BER}(x, y) = \\frac{d_H(x, y)}{n}$$

### Edit Distance (Levenshtein)

**Edit distance** is the minimum number of single-character **insertions, deletions, or substitutions** needed to transform string $s$ into string $t$. Unlike Hamming distance, strings can have different lengths.

The classic dynamic programming solution:

$$dp[i][j] = \\begin{cases} i & \\text{if } j = 0 \\\\ j & \\text{if } i = 0 \\\\ dp[i-1][j-1] & \\text{if } s[i] = t[j] \\\\ 1 + \\min(dp[i-1][j],\\ dp[i][j-1],\\ dp[i-1][j-1]) & \\text{otherwise} \\end{cases}$$

\`\`\`python
def edit_distance(s, t):
    m, n = len(s), len(t)
    dp = [[0] * (n+1) for _ in range(m+1)]
    for i in range(m+1): dp[i][0] = i
    for j in range(n+1): dp[0][j] = j
    for i in range(1, m+1):
        for j in range(1, n+1):
            if s[i-1] == t[j-1]:
                dp[i][j] = dp[i-1][j-1]
            else:
                dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])
    return dp[m][n]

print(edit_distance('kitten', 'sitting'))  # 3
\`\`\`

### Your Task

Implement:
- \`hamming_distance(x, y)\` — count differing positions (same-length strings)
- \`bit_error_rate(x, y)\` — Hamming distance divided by length
- \`edit_distance(s, t)\` — Levenshtein distance via dynamic programming`,

	starterCode: `def hamming_distance(x, y):
    # Count positions where x[i] != y[i]
    pass

def bit_error_rate(x, y):
    # hamming_distance(x, y) / len(x)
    pass

def edit_distance(s, t):
    # Levenshtein distance via DP
    pass

print(hamming_distance('abc', 'aXc'))
print(bit_error_rate('1010', '1000'))
print(edit_distance('kitten', 'sitting'))
`,

	solution: `def hamming_distance(x, y):
    return sum(1 for a, b in zip(x, y) if a != b)

def bit_error_rate(x, y):
    return hamming_distance(x, y) / len(x)

def edit_distance(s, t):
    m, n = len(s), len(t)
    dp = [[0] * (n+1) for _ in range(m+1)]
    for i in range(m+1):
        dp[i][0] = i
    for j in range(n+1):
        dp[0][j] = j
    for i in range(1, m+1):
        for j in range(1, n+1):
            if s[i-1] == t[j-1]:
                dp[i][j] = dp[i-1][j-1]
            else:
                dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])
    return dp[m][n]

print(hamming_distance('abc', 'aXc'))
print(bit_error_rate('1010', '1000'))
print(edit_distance('kitten', 'sitting'))
`,

	tests: [
		{
			name: "hamming('abc','aXc')=1, ber('1010','1000')=0.25, edit('kitten','sitting')=3",
			expected: "1\n0.25\n3\n",
		},
		{
			name: "hamming_distance('1010', '1000') = 1",
			code: `{{FUNC}}
print(hamming_distance('1010', '1000'))`,
			expected: "1\n",
		},
		{
			name: "edit_distance('abc', 'abc') = 0",
			code: `{{FUNC}}
print(edit_distance('abc', 'abc'))`,
			expected: "0\n",
		},
		{
			name: "edit_distance('', 'abc') = 3",
			code: `{{FUNC}}
print(edit_distance('', 'abc'))`,
			expected: "3\n",
		},
		{
			name: "edit_distance('abc', 'aXc') = 1",
			code: `{{FUNC}}
print(edit_distance('abc', 'aXc'))`,
			expected: "1\n",
		},
	],
};
