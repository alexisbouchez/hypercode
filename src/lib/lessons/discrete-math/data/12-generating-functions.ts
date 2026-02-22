import type { Lesson } from "../../types";

export const generatingFunctions: Lesson = {
	id: "generating-functions",
	title: "Generating Functions",
	chapterId: "induction-recurrences",
	content: `## Encoding Sequences as Power Series

A **generating function** encodes a sequence $a_0, a_1, a_2, \\ldots$ as a formal power series:
$$A(x) = a_0 + a_1 x + a_2 x^2 + \\cdots = \\sum_{n=0}^{\\infty} a_n x^n$$

We manipulate $A(x)$ algebraically — multiplication, differentiation, partial fractions — to extract combinatorial information.

### Convolution = Multiplication

If $A(x) = \\sum a_n x^n$ and $B(x) = \\sum b_n x^n$, then:
$$A(x) \\cdot B(x) = \\sum_n \\left(\\sum_{k=0}^{n} a_k b_{n-k}\\right) x^n$$

The coefficient of $x^n$ counts ways to choose items from $A$ and $B$ that sum to $n$.

### Catalan Numbers via Generating Functions

The Catalan number OGF satisfies $C(x) = 1 + x \\cdot C(x)^2$, giving:
$$C_n = \\frac{1}{n+1}\\binom{2n}{n}, \\quad C_0=1,\\ C_1=1,\\ C_2=2,\\ C_3=5,\\ C_4=14,\\ C_5=42$$

\`\`\`python
import math

def catalan(n):
    return math.comb(2 * n, n) // (n + 1)

def ogf_multiply(f, g, terms):
    result = [0] * terms
    for i, fi in enumerate(f):
        for j, gj in enumerate(g):
            if i + j < terms:
                result[i + j] += fi * gj
    return result

print(catalan(5))                           # 42
print(ogf_multiply([1,1,1], [1,1,1], 5))   # [1, 2, 3, 2, 1]
\`\`\`

### Your Task

Implement \`catalan(n)\`, \`ogf_multiply(f, g, terms)\`, and \`extract_coefficient(ogf, n)\`.`,

	starterCode: `import math

def catalan(n):
    # C(2n, n) // (n + 1)
    return math.comb(2 * n, n) // (n + 1)

def ogf_multiply(f, g, terms):
    # Multiply two OGFs (coefficient lists) keeping only first 'terms' coefficients
    result = [0] * terms
    for i, fi in enumerate(f):
        for j, gj in enumerate(g):
            if i + j < terms:
                result[i + j] += fi * gj
    return result

def extract_coefficient(ogf, n):
    # Return coefficient of x^n (ogf[n]), or 0 if out of range
    pass

print(catalan(5))
print(ogf_multiply([1,1,1], [1,1,1], 5))
`,

	solution: `import math

def catalan(n):
    return math.comb(2 * n, n) // (n + 1)

def ogf_multiply(f, g, terms):
    result = [0] * terms
    for i, fi in enumerate(f):
        for j, gj in enumerate(g):
            if i + j < terms:
                result[i + j] += fi * gj
    return result

def extract_coefficient(ogf, n):
    if n < len(ogf):
        return ogf[n]
    return 0

print(catalan(5))
print(ogf_multiply([1,1,1], [1,1,1], 5))
`,

	tests: [
		{
			name: "catalan(5) = 42",
			code: `{{FUNC}}
print(catalan(5))`,
			expected: "42\n",
		},
		{
			name: "ogf_multiply([1,1,1],[1,1,1],5) = [1,2,3,2,1]",
			code: `{{FUNC}}
print(ogf_multiply([1,1,1], [1,1,1], 5))`,
			expected: "[1, 2, 3, 2, 1]\n",
		},
		{
			name: "catalan(10) = 16796",
			code: `{{FUNC}}
print(catalan(10))`,
			expected: "16796\n",
		},
		{
			name: "coeff of x^4 in [1,1,1,1,1]^2 = 5",
			code: `{{FUNC}}
print(extract_coefficient(ogf_multiply([1,1,1,1,1],[1,1,1,1,1], 10), 4))`,
			expected: "5\n",
		},
	],
};
