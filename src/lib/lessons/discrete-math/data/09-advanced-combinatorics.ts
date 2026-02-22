import type { Lesson } from "../../types";

export const advancedCombinatorics: Lesson = {
	id: "advanced-combinatorics",
	title: "Advanced Combinatorics",
	chapterId: "counting-combinatorics",
	content: `## Stars and Bars, Stirling Numbers, Catalan Numbers

### Stars and Bars

The number of ways to distribute $n$ identical objects into $k$ distinct bins (allowing empty bins) is:
$$\\binom{n+k-1}{k-1}$$

*Intuition*: arrange $n$ stars and $k-1$ bars in a row — each arrangement defines a distribution.

**Example**: 3 coins into 2 pockets: $\\binom{4}{1} = 4$ ways (0+3, 1+2, 2+1, 3+0... but 4 because 0 included).

Wait: 5 balls into 3 bins: $\\binom{5+3-1}{3-1} = \\binom{7}{2} = 21$.

### Stirling Numbers of the Second Kind

$S(n, k)$ counts the ways to partition $n$ **labeled** elements into $k$ **non-empty** unlabeled subsets:
$$S(n,k) = k \\cdot S(n-1,k) + S(n-1,k-1), \\quad S(0,0)=1$$

### Catalan Numbers

$$C_n = \\frac{1}{n+1}\\binom{2n}{n}$$

They count: valid parenthesizations, full binary trees, monotone lattice paths, and more.
$C_0=1, C_1=1, C_2=2, C_3=5, C_4=14, C_5=42$.

\`\`\`python
import math

def stars_and_bars(n, k):
    return math.comb(n + k - 1, k - 1)

def catalan(n):
    return math.comb(2 * n, n) // (n + 1)

print(stars_and_bars(5, 3))  # 21
print(catalan(5))            # 42
\`\`\`

### Your Task

Implement \`stars_and_bars(n, k)\`, \`stirling_second(n, k)\`, and \`catalan(n)\`.`,

	starterCode: `import math

def stars_and_bars(n, k):
    # C(n + k - 1, k - 1)
    return math.comb(n + k - 1, k - 1)

def stirling_second(n, k):
    # S(n,k) = k*S(n-1,k) + S(n-1,k-1), S(0,0)=1
    if n == 0 and k == 0: return 1
    if n == 0 or k == 0: return 0
    return k * stirling_second(n-1, k) + stirling_second(n-1, k-1)

def catalan(n):
    # C(2n, n) / (n + 1)
    pass

print(stars_and_bars(5, 3))  # 21
print(catalan(5))            # 42
`,

	solution: `import math

def stars_and_bars(n, k):
    return math.comb(n + k - 1, k - 1)

def stirling_second(n, k):
    if n == 0 and k == 0: return 1
    if n == 0 or k == 0: return 0
    return k * stirling_second(n-1, k) + stirling_second(n-1, k-1)

def catalan(n):
    return math.comb(2 * n, n) // (n + 1)

print(stars_and_bars(5, 3))
print(catalan(5))
`,

	tests: [
		{
			name: "stars_and_bars(5, 3) = 21",
			code: `{{FUNC}}
print(stars_and_bars(5, 3))`,
			expected: "21\n",
		},
		{
			name: "catalan(5) = 42",
			code: `{{FUNC}}
print(catalan(5))`,
			expected: "42\n",
		},
		{
			name: "stirling_second(4, 2) = 7",
			code: `{{FUNC}}
print(stirling_second(4, 2))`,
			expected: "7\n",
		},
		{
			name: "catalan(10) = 16796",
			code: `{{FUNC}}
print(catalan(10))`,
			expected: "16796\n",
		},
	],
};
