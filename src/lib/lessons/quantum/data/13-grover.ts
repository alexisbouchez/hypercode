import type { Lesson } from "../../types";

export const grover: Lesson = {
	id: "grover",
	title: "Grover's Search",
	chapterId: "algorithms",
	content: `## Quantum Search

**Grover's algorithm** searches an unsorted database of $N$ items in $O(\sqrt{N})$ steps — a quadratic speedup over classical $O(N)$ linear search.

The algorithm works on a uniform superposition of all $N$ states and amplifies the amplitude of the target state through two operations:

**1. Oracle** — Flips the phase of the target state (marks it):
\`\`\`python
def oracle(state, target):
    result = state[:]
    result[target] = -result[target]
    return result
\`\`\`

**2. Diffusion** — "Inversion about the mean" — amplifies the marked state:
\`\`\`python
def diffusion(state):
    n = len(state)
    avg = sum(state) / n
    return [2 * avg - a for a in state]
\`\`\`

For $N = 4$ states, one iteration is enough to find the target with certainty:

\`\`\`python
import math

state = [0.5, 0.5, 0.5, 0.5]  # Uniform superposition
state = oracle(state, 2)        # Mark target = 2
state = diffusion(state)        # Amplify
# → [0.0, 0.0, 1.0, 0.0] — target found!
\`\`\`

### Your Task

Implement \`oracle(state, target)\` and \`diffusion(state)\`. Then run Grover's search and print the amplitudes.`,

	starterCode: `import math

def oracle(state, target):
    # Flip the sign of state[target], leave others unchanged
    pass

def diffusion(state):
    # Compute average, return [2*avg - a for each amplitude a]
    pass

def grover_search(n_states, target):
    s = 1 / math.sqrt(n_states)
    state = [s] * n_states
    state = oracle(state, target)
    state = diffusion(state)
    return state

result = grover_search(4, 2)
for amp in result:
    print(round(amp, 4))
`,

	solution: `import math

def oracle(state, target):
    result = state[:]
    result[target] = -result[target]
    return result

def diffusion(state):
    n = len(state)
    avg = sum(state) / n
    return [2 * avg - a for a in state]

def grover_search(n_states, target):
    s = 1 / math.sqrt(n_states)
    state = [s] * n_states
    state = oracle(state, target)
    state = diffusion(state)
    return state

result = grover_search(4, 2)
for amp in result:
    print(round(amp, 4))
`,

	tests: [
		{
			name: "oracle flips sign of target",
			code: `{{FUNC}}
state = [0.5, 0.5, 0.5, 0.5]
result = oracle(state, 1)
print(result[0])
print(result[1])
print(result[2])`,
			expected: "0.5\n-0.5\n0.5\n",
		},
		{
			name: "diffusion amplifies marked state",
			code: `{{FUNC}}
state = [0.5, -0.5, 0.5, 0.5]
result = diffusion(state)
print(round(result[0], 4))
print(round(result[1], 4))`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "Grover's finds target=2 in 4 states",
			code: `{{FUNC}}
import math
result = grover_search(4, 2)
for amp in result:
    print(round(amp, 4))`,
			expected: "0.0\n0.0\n1.0\n0.0\n",
		},
		{
			name: "Grover's finds target=0 in 4 states",
			code: `{{FUNC}}
import math
result = grover_search(4, 0)
for amp in result:
    print(round(amp, 4))`,
			expected: "1.0\n0.0\n0.0\n0.0\n",
		},
	],
};
