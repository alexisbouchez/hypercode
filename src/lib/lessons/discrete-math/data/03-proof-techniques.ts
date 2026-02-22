import type { Lesson } from "../../types";

export const proofTechniques: Lesson = {
	id: "proof-techniques",
	title: "Proof Techniques",
	chapterId: "logic-proofs",
	content: `## The Art of Mathematical Proof

Mathematics is built on **proofs** — rigorous arguments that a statement is true. The main proof strategies are:

**Direct proof**: Assume $P$, deduce $Q$ step by step.

**Proof by contrapositive**: To prove $P \\Rightarrow Q$, prove instead $\\neg Q \\Rightarrow \\neg P$ (logically equivalent).

**Proof by contradiction**: Assume the negation of what you want to prove, derive a contradiction.

**Proof by induction**: Prove a base case $P(1)$, then prove $P(k) \\Rightarrow P(k+1)$.

### Example: Direct Proof

**Claim**: $1 + 2 + \\cdots + n = \\dfrac{n(n+1)}{2}$

We verify this computationally and use it:

\`\`\`python
def sum_formula(n):
    return n * (n + 1) // 2

print(sum_formula(100))  # 5050
\`\`\`

### Example: Proof by Contradiction — Infinitely Many Primes

Euclid's proof: if only finitely many primes $p_1, \\ldots, p_k$ exist, then $N = p_1 \\cdots p_k + 1$ is not divisible by any of them — a contradiction, since every integer $> 1$ has a prime factor.

\`\`\`python
def euclid_contradiction(primes):
    product = 1
    for p in primes:
        product *= p
    candidate = product + 1
    return all(candidate % p != 0 for p in primes)

print(euclid_contradiction([2, 3, 5]))  # True — 31 is not divisible by 2, 3, or 5
\`\`\`

### Your Task

Implement \`sum_formula(n)\` and \`euclid_contradiction(primes)\` as shown above.`,

	starterCode: `def sum_formula(n):
    # Return n*(n+1)//2
    pass

def euclid_contradiction(primes):
    # Compute product of all primes + 1
    # Return True if it's not divisible by any prime in the list
    pass

print(sum_formula(100))  # 5050
print(euclid_contradiction([2, 3, 5]))  # True
`,

	solution: `def sum_formula(n):
    return n * (n + 1) // 2

def euclid_contradiction(primes):
    product = 1
    for p in primes:
        product *= p
    candidate = product + 1
    return all(candidate % p != 0 for p in primes)

print(sum_formula(100))
print(euclid_contradiction([2, 3, 5]))
`,

	tests: [
		{
			name: "sum_formula(100) = 5050",
			code: `{{FUNC}}
print(sum_formula(100))`,
			expected: "5050\n",
		},
		{
			name: "euclid_contradiction([2,3,5]) = True",
			code: `{{FUNC}}
print(euclid_contradiction([2, 3, 5]))`,
			expected: "True\n",
		},
		{
			name: "verify_induction: sum formula holds up to 1000",
			code: `{{FUNC}}
print(all(sum(range(1, n+1)) == sum_formula(n) for n in range(1, 1001)))`,
			expected: "True\n",
		},
		{
			name: "euclid_contradiction([2,3,5,7,11,13]) = True",
			code: `{{FUNC}}
print(euclid_contradiction([2, 3, 5, 7, 11, 13]))`,
			expected: "True\n",
		},
	],
};
