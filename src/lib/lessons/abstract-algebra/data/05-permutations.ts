import type { Lesson } from "../../types";

export const permutations: Lesson = {
	id: "permutations",
	title: "Permutation Groups",
	chapterId: "groups",
	content: `## Permutation Groups

A **permutation** of a set $\\{0, 1, \\ldots, n-1\\}$ is a bijection from the set to itself. The set of all permutations of $n$ elements forms the **symmetric group** $S_n$ under composition.

### Representing Permutations

We represent a permutation $\\sigma$ as a list where $\\sigma[i]$ is the image of $i$:

\`\`\`python
# sigma: 0->1, 1->2, 2->0
sigma = [1, 2, 0]
\`\`\`

### Composition

The composition $\\sigma \\circ \\tau$ applies $\\tau$ first, then $\\sigma$:

\`\`\`python
def compose(sigma, tau):
    n = len(sigma)
    return [sigma[tau[i]] for i in range(n)]
\`\`\`

### Identity and Inverse

The **identity permutation** is $\\text{id} = [0, 1, 2, \\ldots, n-1]$.

The **inverse** $\\sigma^{-1}$ satisfies $\\sigma \\circ \\sigma^{-1} = \\text{id}$:

\`\`\`python
def inverse(sigma):
    n = len(sigma)
    inv = [0] * n
    for i in range(n):
        inv[sigma[i]] = i
    return inv
\`\`\`

### Order of a Permutation

The order of $\\sigma$ is the smallest $k > 0$ such that $\\sigma^k = \\text{id}$. It equals the **least common multiple** of the lengths of its disjoint cycles.

### Cycle Notation

A permutation can be decomposed into disjoint cycles. For example, $[1, 2, 0, 4, 3]$ decomposes as $(0\\ 1\\ 2)(3\\ 4)$.

### Your Task

Implement \`compose(sigma, tau)\`, \`inverse(sigma)\`, and \`perm_order(sigma)\` (the order of a permutation).`,

	starterCode: `def compose(sigma, tau):
    # Apply tau first, then sigma
    pass

def inverse(sigma):
    # Return the inverse permutation
    pass

def perm_order(sigma):
    # Return the order of the permutation
    pass

sigma = [1, 2, 0]
tau = [0, 2, 1]
print(compose(sigma, tau))
print(inverse(sigma))
print(perm_order(sigma))
print(perm_order([1, 2, 0, 4, 3]))
`,

	solution: `def compose(sigma, tau):
    n = len(sigma)
    return [sigma[tau[i]] for i in range(n)]

def inverse(sigma):
    n = len(sigma)
    inv = [0] * n
    for i in range(n):
        inv[sigma[i]] = i
    return inv

def perm_order(sigma):
    n = len(sigma)
    identity = list(range(n))
    current = list(sigma)
    k = 1
    while current != identity:
        current = compose(sigma, current)
        k += 1
    return k

sigma = [1, 2, 0]
tau = [0, 2, 1]
print(compose(sigma, tau))
print(inverse(sigma))
print(perm_order(sigma))
print(perm_order([1, 2, 0, 4, 3]))
`,

	tests: [
		{
			name: "Compose, inverse, and order",
			expected: "[1, 0, 2]\n[2, 0, 1]\n3\n6\n",
		},
		{
			name: "Composing with identity",
			code: `{{FUNC}}
sigma = [2, 0, 1]
identity = [0, 1, 2]
print(compose(sigma, identity))`,
			expected: "[2, 0, 1]\n",
		},
		{
			name: "Inverse of inverse is original",
			code: `{{FUNC}}
sigma = [2, 0, 1]
print(inverse(inverse(sigma)))`,
			expected: "[2, 0, 1]\n",
		},
		{
			name: "Order of a transposition is 2",
			code: `{{FUNC}}
print(perm_order([1, 0, 2, 3]))`,
			expected: "2\n",
		},
	],
};
