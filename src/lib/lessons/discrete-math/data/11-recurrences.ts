import type { Lesson } from "../../types";

export const recurrences: Lesson = {
	id: "recurrences",
	title: "Recurrence Relations",
	chapterId: "induction-recurrences",
	content: `## Counting with Recurrences

A **recurrence relation** defines a sequence by expressing $a_n$ in terms of previous terms. Combined with initial conditions, it uniquely determines the sequence.

### Linear Recurrences

A **linear homogeneous recurrence** with constant coefficients:
$$a_n = c_1 a_{n-1} + c_2 a_{n-2} + \\cdots + c_k a_{n-k}$$

**Characteristic equation**: substitute $a_n = r^n$ to get $r^k = c_1 r^{k-1} + \\cdots + c_k$. If roots $r_1, r_2, \\ldots$ are distinct, the solution is:
$$a_n = \\alpha_1 r_1^n + \\alpha_2 r_2^n + \\cdots$$

**Fibonacci**: $F_n = F_{n-1} + F_{n-2}$, $F_0=0$, $F_1=1$.
Characteristic roots $r = \\frac{1 \\pm \\sqrt{5}}{2}$ (golden ratio $\\phi$ and its conjugate).

### Towers of Hanoi

$T(n) = 2T(n-1) + 1$, $T(1)=1$. Closed form: $T(n) = 2^n - 1$.

\`\`\`python
def solve_linear_recurrence(coeffs, initial, n):
    seq = list(initial)
    k = len(coeffs)
    while len(seq) <= n:
        next_val = sum(coeffs[i] * seq[-(i+1)] for i in range(k))
        seq.append(next_val)
    return seq[n]

# Fibonacci: coeffs=[1,1], initial=[0,1]
print(solve_linear_recurrence([1, 1], [0, 1], 10))  # 55
\`\`\`

### Your Task

Implement \`solve_fibonacci\`, \`solve_linear_recurrence\`, and \`towers_of_hanoi_count\`.`,

	starterCode: `def solve_fibonacci(n):
    if n <= 1: return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b

def solve_linear_recurrence(coeffs, initial, n):
    # coeffs = [c1, c2, ...]: a[n] = c1*a[n-1] + c2*a[n-2] + ...
    # initial = [a[0], a[1], ...]
    seq = list(initial)
    k = len(coeffs)
    while len(seq) <= n:
        next_val = sum(coeffs[i] * seq[-(i+1)] for i in range(k))
        seq.append(next_val)
    return seq[n]

def towers_of_hanoi_count(n):
    # Closed form: 2^n - 1
    pass

print(solve_fibonacci(10))  # 55
`,

	solution: `def solve_fibonacci(n):
    if n <= 1: return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b

def solve_linear_recurrence(coeffs, initial, n):
    seq = list(initial)
    k = len(coeffs)
    while len(seq) <= n:
        next_val = sum(coeffs[i] * seq[-(i+1)] for i in range(k))
        seq.append(next_val)
    return seq[n]

def towers_of_hanoi_count(n):
    return 2**n - 1

print(solve_fibonacci(10))
`,

	tests: [
		{
			name: "F(10) = 55",
			expected: "55\n",
		},
		{
			name: "solve_linear_recurrence gives F(15) = 610",
			code: `{{FUNC}}
print(solve_linear_recurrence([1, 1], [0, 1], 15))`,
			expected: "610\n",
		},
		{
			name: "towers_of_hanoi_count(10) = 1023",
			code: `{{FUNC}}
print(towers_of_hanoi_count(10))`,
			expected: "1023\n",
		},
		{
			name: "arithmetic recurrence: a[n]=2a[n-1]-a[n-2], a[0]=1,a[1]=2 gives a[5]=6",
			code: `{{FUNC}}
print(solve_linear_recurrence([2, -1], [1, 2], 5))`,
			expected: "6\n",
		},
	],
};
