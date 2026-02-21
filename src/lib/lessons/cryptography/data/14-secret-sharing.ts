import type { Lesson } from "../../types";

export const secretSharing: Lesson = {
	id: "secret-sharing",
	title: "Shamir's Secret Sharing",
	chapterId: "protocols",
	content: `## Shamir's Secret Sharing

**Shamir's Secret Sharing** (Adi Shamir, 1979) splits a secret into $n$ shares such that any $t$ shares can reconstruct the secret, but any $t-1$ shares reveal absolutely nothing. This is called a **$(t, n)$-threshold scheme**.

### Construction

1. Represent the secret as an integer $s$ (the constant term of a polynomial)
2. Choose a random polynomial of degree $t-1$:
$$f(x) = s + a_1 x + a_2 x^2 + \\cdots + a_{t-1} x^{t-1} \\pmod{p}$$
where $p$ is a prime larger than both $s$ and $n$
3. Give party $i$ the share $(i, f(i))$ for $i = 1, 2, \\ldots, n$

### Reconstruction — Lagrange Interpolation

Given $t$ shares $(x_1, y_1), \\ldots, (x_t, y_t)$, reconstruct $f(0) = s$ using **Lagrange interpolation** modulo $p$:

$$s = f(0) = \\sum_{i=1}^{t} y_i \\prod_{j \\neq i} \\frac{-x_j}{x_i - x_j} \\pmod{p}$$

The modular division uses Fermat's little theorem: $a^{-1} \\equiv a^{p-2} \\pmod{p}$.

### Security

With fewer than $t$ shares, the secret is information-theoretically hidden — any value of $s$ is equally likely. This is provably secure, unlike computational security assumptions.

### Applications

- **Distributed key management** — split private keys across multiple servers
- **Disaster recovery** — $m$-of-$n$ backup schemes
- **Secure multi-party computation** — building block for advanced protocols

### Our Deterministic Version

For testability, we use deterministic coefficients: $a_i = (s \\times i \\times 1337) \\bmod p$ (instead of random).

### Your Task

Implement:
- \`shamir_share(secret, n, threshold, prime=2**31-1)\` — generate $n$ shares using a degree-$(t-1)$ polynomial
- \`shamir_reconstruct(shares, prime=2**31-1)\` — Lagrange interpolation to recover the secret`,

	starterCode: `def shamir_share(secret, n, threshold, prime=2**31-1):
    # Build polynomial: coeffs[0]=secret, coeffs[i]=(secret*(i)*1337)%prime for i=1..threshold-1
    # Evaluate at x=1..n to get shares as list of (x, f(x)) pairs
    pass

def shamir_reconstruct(shares, prime=2**31-1):
    # Lagrange interpolation mod prime to find f(0)
    pass

shares = shamir_share(42, 3, 2)
print(shares)
print(shamir_reconstruct(shares[:2]))
print(shamir_reconstruct(shares[1:]))
`,

	solution: `def shamir_share(secret, n, threshold, prime=2**31-1):
    coeffs = [secret]
    for i in range(threshold - 1):
        coeff = (secret * (i + 1) * 1337) % prime
        coeffs.append(coeff)
    shares = []
    for x in range(1, n + 1):
        y = 0
        for j, c in enumerate(coeffs):
            y = (y + c * pow(x, j, prime)) % prime
        shares.append((x, y))
    return shares

def shamir_reconstruct(shares, prime=2**31-1):
    secret = 0
    for i, (xi, yi) in enumerate(shares):
        num = 1
        den = 1
        for j, (xj, _) in enumerate(shares):
            if i != j:
                num = (num * (-xj)) % prime
                den = (den * (xi - xj)) % prime
        lagrange = (yi * num * pow(den, prime - 2, prime)) % prime
        secret = (secret + lagrange) % prime
    return secret

shares = shamir_share(42, 3, 2)
print(shares)
print(shamir_reconstruct(shares[:2]))
print(shamir_reconstruct(shares[1:]))
`,

	tests: [
		{
			name: "shamir_share(42, 3, 2) and reconstruct",
			expected: "[(1, 56196), (2, 112350), (3, 168504)]\n42\n42\n",
		},
		{
			name: "reconstruct from shares [0] and [2]",
			code: `{{FUNC}}
shares = shamir_share(42, 3, 2)
print(shamir_reconstruct([shares[0], shares[2]]))`,
			expected: "42\n",
		},
		{
			name: "shamir works with secret=0",
			code: `{{FUNC}}
shares = shamir_share(0, 3, 2)
print(shamir_reconstruct(shares[:2]))`,
			expected: "0\n",
		},
		{
			name: "shamir works with threshold=3",
			code: `{{FUNC}}
shares = shamir_share(100, 5, 3)
print(shamir_reconstruct(shares[:3]))`,
			expected: "100\n",
		},
	],
};
