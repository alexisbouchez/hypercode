import type { Lesson } from "../../types";

export const deutsch: Lesson = {
	id: "deutsch",
	title: "The Deutsch Algorithm",
	chapterId: "algorithms",
	content: `## Quantum Speedup

The **Deutsch algorithm** was the first quantum algorithm to demonstrate a speedup over classical computation. It solves a specific problem in 1 query that classically requires 2.

**The problem:** Given a function $f: \{0, 1\} \to \{0, 1\}$, determine if $f$ is:
- **Constant**: $f(0) = f(1)$ (always outputs 0, or always outputs 1)
- **Balanced**: $f(0) \neq f(1)$ (outputs 0 for one input and 1 for the other)

Classically, you must evaluate $f(0)$ and $f(1)$ separately — 2 queries.

The Deutsch algorithm evaluates $f$ in **quantum superposition**, getting the answer in 1 query.

The algorithm's outcome is determined by $f(0) \oplus f(1)$:
- 0 → constant
- 1 → balanced

\`\`\`python
def deutsch(f):
    if f(0) == f(1):
        return "constant"
    else:
        return "balanced"
\`\`\`

There are exactly 4 possible functions:

| Function | $f(0)$ | $f(1)$ | Type |
|----------|------|------|------|
| always 0 | 0    | 0    | constant |
| always 1 | 1    | 1    | constant |
| identity | 0    | 1    | balanced |
| NOT      | 1    | 0    | balanced |

### Your Task

Implement \`deutsch(f)\` that classifies the function. Then define the four possible functions and test each one.`,

	starterCode: `def deutsch(f):
    # If f(0) == f(1), return "constant", otherwise "balanced"
    pass

def f_zero(x):    return 0          # always 0
def f_one(x):     return 1          # always 1
def f_id(x):      return x          # identity
def f_not(x):     return 1 - x      # NOT

print(deutsch(f_zero))
print(deutsch(f_one))
print(deutsch(f_id))
print(deutsch(f_not))
`,

	solution: `def deutsch(f):
    if f(0) == f(1):
        return "constant"
    else:
        return "balanced"

def f_zero(x):    return 0
def f_one(x):     return 1
def f_id(x):      return x
def f_not(x):     return 1 - x

print(deutsch(f_zero))
print(deutsch(f_one))
print(deutsch(f_id))
print(deutsch(f_not))
`,

	tests: [
		{
			name: "constant-zero function is classified as constant",
			code: `{{FUNC}}
print(deutsch(lambda x: 0))`,
			expected: "constant\n",
		},
		{
			name: "constant-one function is classified as constant",
			code: `{{FUNC}}
print(deutsch(lambda x: 1))`,
			expected: "constant\n",
		},
		{
			name: "identity function is classified as balanced",
			code: `{{FUNC}}
print(deutsch(lambda x: x))`,
			expected: "balanced\n",
		},
		{
			name: "NOT function is classified as balanced",
			code: `{{FUNC}}
print(deutsch(lambda x: 1 - x))`,
			expected: "balanced\n",
		},
	],
};
