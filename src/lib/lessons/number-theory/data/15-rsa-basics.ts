import type { Lesson } from "../../types";

export const rsaBasics: Lesson = {
	id: "rsa-basics",
	title: "RSA Encryption Basics",
	chapterId: "applications",
	content: `## RSA Encryption

**RSA** (Rivest–Shamir–Adleman, 1977) is the most widely used public-key cryptosystem. Its security rests on the difficulty of **factoring large numbers**.

### Key Generation

Given two distinct primes $p$ and $q$:

1. Compute $n = p \\cdot q$ (the **modulus**)
2. Compute $\\varphi(n) = (p-1)(q-1)$ (Euler's totient)
3. Choose $e$ with $1 < e < \\varphi(n)$ and $\\gcd(e, \\varphi(n)) = 1$ (the **public exponent**)
4. Compute $d \\equiv e^{-1} \\pmod{\\varphi(n)}$ using the Extended Euclidean Algorithm (the **private exponent**)

The **public key** is $(e, n)$. The **private key** is $(d, n)$.

### Classic Example

With $p = 61$, $q = 53$, $e = 17$:
- $n = 3233$
- $\\varphi(n) = 3120$
- $d = e^{-1} \\bmod 3120 = 2753$

### Encryption & Decryption

$$\\text{encrypt}(m) = m^e \\bmod n$$
$$\\text{decrypt}(c) = c^d \\bmod n$$

Both operations use fast modular exponentiation:

\`\`\`python
def rsa_encrypt(msg_int, e, n):
    result = 1
    base = msg_int % n
    exp = e
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % n
        exp //= 2
        base = (base * base) % n
    return result

def rsa_decrypt(cipher_int, d, n):
    result = 1
    base = cipher_int % n
    exp = d
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % n
        exp //= 2
        base = (base * base) % n
    return result

# Encrypt message 42 with public key (17, 3233)
cipher = rsa_encrypt(42, 17, 3233)    # 2557
msg    = rsa_decrypt(2557, 2753, 3233) # 42
\`\`\`

### Why It Works

By Euler's theorem: $m^{\\varphi(n)} \\equiv 1 \\pmod{n}$, so $m^{ed} = m^{1 + k\\varphi(n)} \\equiv m \\pmod{n}$.

### Modular Inverse via Extended GCD

\`\`\`python
def mod_inverse(e, phi):
    def extended_gcd(a, b):
        if b == 0: return a, 1, 0
        g, x, y = extended_gcd(b, a % b)
        return g, y, x - (a // b) * y
    g, x, _ = extended_gcd(e, phi)
    return x % phi   # ensure positive

d = mod_inverse(17, 3120)   # 2753
\`\`\`

### Your Task

Implement:
- \`mod_inverse(e, phi)\` using extended GCD — returns $e^{-1} \\bmod \\varphi$
- \`rsa_encrypt(msg_int, e, n)\` — returns $m^e \\bmod n$
- \`rsa_decrypt(cipher_int, d, n)\` — returns $c^d \\bmod n$`,

	starterCode: `def mod_inverse(e, phi):
    # Use extended GCD to find e^(-1) mod phi
    pass

def rsa_encrypt(msg_int, e, n):
    # Return msg_int^e mod n using fast exponentiation
    pass

def rsa_decrypt(cipher_int, d, n):
    # Return cipher_int^d mod n using fast exponentiation
    pass

print(rsa_encrypt(42, 17, 3233))
print(rsa_decrypt(2557, 2753, 3233))
`,

	solution: `def mod_inverse(e, phi):
    def extended_gcd(a, b):
        if b == 0:
            return a, 1, 0
        g, x, y = extended_gcd(b, a % b)
        return g, y, x - (a // b) * y
    g, x, _ = extended_gcd(e, phi)
    return x % phi

def rsa_encrypt(msg_int, e, n):
    result = 1
    base = msg_int % n
    exp = e
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % n
        exp //= 2
        base = (base * base) % n
    return result

def rsa_decrypt(cipher_int, d, n):
    result = 1
    base = cipher_int % n
    exp = d
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % n
        exp //= 2
        base = (base * base) % n
    return result

print(rsa_encrypt(42, 17, 3233))
print(rsa_decrypt(2557, 2753, 3233))
`,

	tests: [
		{
			name: "rsa_encrypt(42, 17, 3233) = 2557, rsa_decrypt(2557, 2753, 3233) = 42",
			expected: "2557\n42\n",
		},
		{
			name: "mod_inverse(17, 3120) = 2753",
			code: `{{FUNC}}
print(mod_inverse(17, 3120))`,
			expected: "2753\n",
		},
		{
			name: "encrypt then decrypt returns original message",
			code: `{{FUNC}}
e, d, n = 17, 2753, 3233
msg = 100
cipher = rsa_encrypt(msg, e, n)
print(rsa_decrypt(cipher, d, n))`,
			expected: "100\n",
		},
		{
			name: "rsa_encrypt(1, 17, 3233) = 1",
			code: `{{FUNC}}
print(rsa_encrypt(1, 17, 3233))`,
			expected: "1\n",
		},
		{
			name: "mod_inverse(3, 10) = 7 (since 3*7=21≡1 mod 10)",
			code: `{{FUNC}}
print(mod_inverse(3, 10))`,
			expected: "7\n",
		},
	],
};
