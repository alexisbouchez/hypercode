import type { Lesson } from "../../types";

export const numbers: Lesson = {
	id: "numbers",
	title: "Numbers",
	chapterId: "foundations",
	content: `## Numbers

Python has three built-in number types: \`int\`, \`float\`, and \`complex\`.

### Integer Operations

\`\`\`python
10 + 3   # 13
10 - 3   # 7
10 * 3   # 30
10 / 3   # 3.3333...  (float division)
10 // 3  # 3          (floor division)
10 % 3   # 1          (modulo / remainder)
2 ** 10  # 1024       (exponentiation)
\`\`\`

### abs(), round(), min(), max()

\`\`\`python
abs(-5)        # 5
round(3.7)     # 4
round(3.14159, 2)  # 3.14
min(3, 1, 4)   # 1
max(3, 1, 4)   # 4
\`\`\`

### Type Conversion

\`\`\`python
int("42")     # 42
float("3.14") # 3.14
str(100)      # "100"
\`\`\`

### The math Module

\`\`\`python
import math
math.sqrt(16)  # 4.0
math.floor(3.9)  # 3
math.ceil(3.1)   # 4
math.pi          # 3.14159...
\`\`\`

### Your Task

Implement \`is_prime(n)\` that returns \`True\` if \`n\` is a prime number, \`False\` otherwise.

A prime is a number greater than 1 with no divisors other than 1 and itself. A fast check: test divisors only up to \`√n\`.`,

	starterCode: `import math

def is_prime(n):
    if n < 2:
        return False
    # Check divisors from 2 to sqrt(n)
    pass

print(is_prime(2))
print(is_prime(7))
print(is_prime(9))
print(is_prime(1))
`,

	solution: `import math

def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(math.sqrt(n)) + 1):
        if n % i == 0:
            return False
    return True

print(is_prime(2))
print(is_prime(7))
print(is_prime(9))
print(is_prime(1))
`,

	tests: [
		{
			name: "2 is prime",
			code: `{{FUNC}}
print(is_prime(2))`,
			expected: "True\n",
		},
		{
			name: "7 is prime",
			code: `{{FUNC}}
print(is_prime(7))`,
			expected: "True\n",
		},
		{
			name: "9 is not prime",
			code: `{{FUNC}}
print(is_prime(9))`,
			expected: "False\n",
		},
		{
			name: "1 is not prime",
			code: `{{FUNC}}
print(is_prime(1))`,
			expected: "False\n",
		},
	],
};
