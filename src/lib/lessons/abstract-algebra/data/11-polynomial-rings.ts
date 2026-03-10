import type { Lesson } from "../../types";

export const polynomialRings: Lesson = {
	id: "polynomial-rings",
	title: "Polynomial Rings",
	chapterId: "rings",
	content: `## Polynomial Rings

The **polynomial ring** $R[x]$ over a ring $R$ consists of all polynomials with coefficients in $R$. We represent a polynomial as a list of coefficients: \`[a0, a1, a2, ...]\` represents $a_0 + a_1 x + a_2 x^2 + \\cdots$.

### Polynomial Arithmetic mod $p$

Over $\\mathbb{Z}_p$, all coefficient arithmetic is done modulo $p$:

\`\`\`python
def poly_add(f, g, p):
    n = max(len(f), len(g))
    result = [0] * n
    for i in range(len(f)):
        result[i] = (result[i] + f[i]) % p
    for i in range(len(g)):
        result[i] = (result[i] + g[i]) % p
    # Strip trailing zeros
    while len(result) > 1 and result[-1] == 0:
        result.pop()
    return result
\`\`\`

### Polynomial Multiplication mod $p$

Standard convolution with coefficients reduced mod $p$:

$$\\left(\\sum_i a_i x^i\\right)\\left(\\sum_j b_j x^j\\right) = \\sum_k \\left(\\sum_{i+j=k} a_i b_j\\right) x^k$$

### Polynomial Evaluation

To evaluate $f(a)$ in $\\mathbb{Z}_p$, use Horner's method:

\`\`\`python
def poly_eval(f, a, p):
    result = 0
    for coeff in reversed(f):
        result = (result * a + coeff) % p
    return result
\`\`\`

### Your Task

Implement \`poly_add(f, g, p)\`, \`poly_mul(f, g, p)\`, and \`poly_eval(f, a, p)\` for polynomials over $\\mathbb{Z}_p$. Strip trailing zeros from results.`,

	starterCode: `def poly_add(f, g, p):
    # Add polynomials f and g mod p
    pass

def poly_mul(f, g, p):
    # Multiply polynomials f and g mod p
    pass

def poly_eval(f, a, p):
    # Evaluate f(a) mod p using Horner's method
    pass

# (1 + 2x) + (3 + x) = 4 + 3x in Z_5[x]
print(poly_add([1, 2], [3, 1], 5))
# (1 + x)(2 + x) = 2 + 3x + x^2 in Z_5[x]
print(poly_mul([1, 1], [2, 1], 5))
# Evaluate x^2 + 1 at x=2 in Z_5: 4 + 1 = 5 = 0
print(poly_eval([1, 0, 1], 2, 5))
`,

	solution: `def poly_add(f, g, p):
    n = max(len(f), len(g))
    result = [0] * n
    for i in range(len(f)):
        result[i] = (result[i] + f[i]) % p
    for i in range(len(g)):
        result[i] = (result[i] + g[i]) % p
    while len(result) > 1 and result[-1] == 0:
        result.pop()
    return result

def poly_mul(f, g, p):
    if not f or not g:
        return [0]
    n = len(f) + len(g) - 1
    result = [0] * n
    for i in range(len(f)):
        for j in range(len(g)):
            result[i + j] = (result[i + j] + f[i] * g[j]) % p
    while len(result) > 1 and result[-1] == 0:
        result.pop()
    return result

def poly_eval(f, a, p):
    result = 0
    for coeff in reversed(f):
        result = (result * a + coeff) % p
    return result

# (1 + 2x) + (3 + x) = 4 + 3x in Z_5[x]
print(poly_add([1, 2], [3, 1], 5))
# (1 + x)(2 + x) = 2 + 3x + x^2 in Z_5[x]
print(poly_mul([1, 1], [2, 1], 5))
# Evaluate x^2 + 1 at x=2 in Z_5: 4 + 1 = 5 = 0
print(poly_eval([1, 0, 1], 2, 5))
`,

	tests: [
		{
			name: "Poly add, mul, eval in Z_5[x]",
			expected: "[4, 3]\n[2, 3, 1]\n0\n",
		},
		{
			name: "(3 + 4x) * (2 + 3x) in Z_7[x]",
			code: `{{FUNC}}
print(poly_mul([3, 4], [2, 3], 7))`,
			expected: "[6, 3, 5]\n",
		},
		{
			name: "Adding inverse polynomials gives zero",
			code: `{{FUNC}}
f = [1, 2, 3]
g = [4, 3, 2]
print(poly_add(f, g, 5))`,
			expected: "[0]\n",
		},
		{
			name: "Eval 2x^3 + x + 3 at x=4 in Z_7",
			code: `{{FUNC}}
print(poly_eval([3, 1, 0, 2], 4, 7))`,
			expected: "2\n",
		},
	],
};
