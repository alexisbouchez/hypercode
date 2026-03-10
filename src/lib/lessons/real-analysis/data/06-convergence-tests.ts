import type { Lesson } from "../../types";

export const convergenceTests: Lesson = {
	id: "convergence-tests",
	title: "Convergence Tests",
	chapterId: "series",
	content: `## Convergence Tests

Several tests help determine whether a series converges without computing the sum.

### Ratio Test

For \\(\\sum a_n\\) with \\(a_n > 0\\), compute:

$$L = \\lim_{n \\to \\infty} \\frac{a_{n+1}}{a_n}$$

- If \\(L < 1\\), the series **converges**
- If \\(L > 1\\), the series **diverges**
- If \\(L = 1\\), the test is **inconclusive**

### Root Test

Compute:

$$L = \\lim_{n \\to \\infty} \\sqrt[n]{|a_n|}$$

Same conclusion rules as the ratio test.

### Comparison Test

If \\(0 \\le a_n \\le b_n\\) for all \\(n\\) and \\(\\sum b_n\\) converges, then \\(\\sum a_n\\) converges.

### Numerical Approximation

We approximate the limit in the ratio/root test using large \\(n\\):

\`\`\`python
def ratio_test(f, n=1000):
    return f(n + 1) / f(n)
\`\`\`

### Your Task

Implement:
1. \`ratio_test(f, n)\` -- returns \\(f(n+1) / f(n)\\) as an approximation of the ratio test limit
2. \`root_test(f, n)\` -- returns \\(|f(n)|^{1/n}\\) as an approximation of the root test limit
3. \`convergence_verdict(L)\` -- returns \`"converges"\` if \\(L < 1\\), \`"diverges"\` if \\(L > 1\\), \`"inconclusive"\` if \\(L = 1\\)`,

	starterCode: `def ratio_test(f, n):
    # Return f(n+1) / f(n)
    pass

def root_test(f, n):
    # Return abs(f(n)) ** (1/n)
    pass

def convergence_verdict(L):
    # Return "converges", "diverges", or "inconclusive"
    pass

factorial_inv = lambda n: 1
for i in range(1, 1001):
    pass  # Build 1/n! iteratively below

# Test with 1/2^n (converges)
geo = lambda n: (1/2) ** n
print(round(ratio_test(geo, 500), 4))
print(convergence_verdict(ratio_test(geo, 500)))

# Test with 1/n (diverges: ratio -> 1, inconclusive)
harm = lambda n: 1 / n
print(round(root_test(harm, 500), 4))
`,

	solution: `def ratio_test(f, n):
    return f(n + 1) / f(n)

def root_test(f, n):
    return abs(f(n)) ** (1 / n)

def convergence_verdict(L):
    if L < 1:
        return "converges"
    elif L > 1:
        return "diverges"
    else:
        return "inconclusive"

# Test with 1/2^n (converges)
geo = lambda n: (1/2) ** n
print(round(ratio_test(geo, 500), 4))
print(convergence_verdict(ratio_test(geo, 500)))

# Test with 1/n (diverges: ratio -> 1, inconclusive)
harm = lambda n: 1 / n
print(round(root_test(harm, 500), 4))
`,

	tests: [
		{
			name: "ratio test for geometric series 1/2^n",
			code: `{{FUNC}}
geo = lambda n: (1/2) ** n
print(round(ratio_test(geo, 500), 4))`,
			expected: "0.5\n",
		},
		{
			name: "convergence verdict for L=0.5",
			code: `{{FUNC}}
print(convergence_verdict(0.5))`,
			expected: "converges\n",
		},
		{
			name: "convergence verdict for L=1.5",
			code: `{{FUNC}}
print(convergence_verdict(1.5))`,
			expected: "diverges\n",
		},
		{
			name: "root test for 1/n approaches 1",
			code: `{{FUNC}}
harm = lambda n: 1 / n
print(round(root_test(harm, 500), 4))`,
			expected: "0.9876\n",
		},
	],
};
