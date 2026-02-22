import type { Lesson } from "../../types";

export const inclusionExclusion: Lesson = {
	id: "inclusion-exclusion",
	title: "Inclusion-Exclusion & Pigeonhole",
	chapterId: "counting-combinatorics",
	content: `## Counting Unions and Avoiding Overlaps

### Inclusion-Exclusion Principle

When events overlap, naive addition double-counts intersections. The **inclusion-exclusion principle** corrects this:

$$|A \\cup B| = |A| + |B| - |A \\cap B|$$

$$|A \\cup B \\cup C| = |A| + |B| + |C| - |A \\cap B| - |A \\cap C| - |B \\cap C| + |A \\cap B \\cap C|$$

### Derangements

A **derangement** is a permutation with no fixed points (no element stays in its original position). The count $D(n)$ satisfies:
$$D(n) = (n-1)(D(n-1) + D(n-2)), \\quad D(0)=1,\\ D(1)=0$$

This follows from inclusion-exclusion applied to the $n!$ permutations.

### Pigeonhole Principle

If $n+1$ objects are placed in $n$ bins, at least one bin contains 2 or more objects.

**Generalized**: If $kn + 1$ objects go into $n$ bins, some bin has at least $k+1$ objects.

\`\`\`python
def derangements(n):
    if n == 0: return 1
    if n == 1: return 0
    d = [0] * (n + 1)
    d[0], d[1] = 1, 0
    for i in range(2, n + 1):
        d[i] = (i - 1) * (d[i-1] + d[i-2])
    return d[n]

print(derangements(4))  # 9
\`\`\`

### Your Task

Implement \`inclusion_exclusion_3\`, \`derangements\`, and \`min_pigeons_for_guarantee\`.`,

	starterCode: `def inclusion_exclusion_3(a, b, c, ab, ac, bc, abc):
    # |A ∪ B ∪ C| = |A| + |B| + |C| - |A∩B| - |A∩C| - |B∩C| + |A∩B∩C|
    pass

def derangements(n):
    if n == 0: return 1
    if n == 1: return 0
    d = [0] * (n + 1)
    d[0], d[1] = 1, 0
    for i in range(2, n + 1):
        d[i] = (i - 1) * (d[i-1] + d[i-2])
    return d[n]

def min_pigeons_for_guarantee(holes):
    # Minimum pigeons to guarantee 2 in the same hole
    return holes + 1

print(derangements(4))  # 9
`,

	solution: `def inclusion_exclusion_3(a, b, c, ab, ac, bc, abc):
    return a + b + c - ab - ac - bc + abc

def derangements(n):
    if n == 0: return 1
    if n == 1: return 0
    d = [0] * (n + 1)
    d[0], d[1] = 1, 0
    for i in range(2, n + 1):
        d[i] = (i - 1) * (d[i-1] + d[i-2])
    return d[n]

def min_pigeons_for_guarantee(holes):
    return holes + 1

print(derangements(4))
`,

	tests: [
		{
			name: "D(4) = 9 derangements",
			expected: "9\n",
		},
		{
			name: "inclusion_exclusion_3(10,15,20,5,4,6,2) = 32",
			code: `{{FUNC}}
print(inclusion_exclusion_3(10, 15, 20, 5, 4, 6, 2))`,
			expected: "32\n",
		},
		{
			name: "D(5) = 44 derangements",
			code: `{{FUNC}}
print(derangements(5))`,
			expected: "44\n",
		},
		{
			name: "pigeonhole: 12 holes → 13 pigeons needed",
			code: `{{FUNC}}
print(min_pigeons_for_guarantee(12))`,
			expected: "13\n",
		},
	],
};
