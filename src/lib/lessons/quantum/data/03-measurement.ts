import type { Lesson } from "../../types";

export const measurement: Lesson = {
	id: "measurement",
	title: "Measurement",
	chapterId: "qubits",
	content: `## The Born Rule

When we **measure** a qubit, the superposition collapses to either $|0\rangle$ or $|1\rangle$. The probabilities are given by the **Born rule**:

- $P(|0\rangle) = |\alpha|^2$
- $P(|1\rangle) = |\beta|^2$

For the $|0\rangle$ state [1.0, 0.0]: $P(0) = 1$, $P(1) = 0$ — certain outcome.

For the equal superposition $\left[\frac{1}{\sqrt{2}}, \frac{1}{\sqrt{2}}\right]$: $P(0) = 0.5$, $P(1) = 0.5$ — truly random.

\`\`\`python
import math

def prob_zero(state):
    return state[0] ** 2

def prob_one(state):
    return state[1] ** 2

s = 1 / math.sqrt(2)
superposition = [s, s]

print(round(prob_zero(superposition), 4))  # 0.5
print(round(prob_one(superposition), 4))   # 0.5
\`\`\`

After measurement, the qubit is **irreversibly** in the measured state — the superposition is gone.

### Your Task

Implement \`prob_zero(state)\` and \`prob_one(state)\` using the Born rule.`,

	starterCode: `import math

def prob_zero(state):
    # Return the probability of measuring |0⟩
    pass

def prob_one(state):
    # Return the probability of measuring |1⟩
    pass

s = 1 / math.sqrt(2)
superposition = [s, s]

print(prob_zero([1.0, 0.0]))
print(prob_one([1.0, 0.0]))
print(prob_zero([0.0, 1.0]))
print(prob_one([0.0, 1.0]))
print(round(prob_zero(superposition), 4))
print(round(prob_one(superposition), 4))
`,

	solution: `import math

def prob_zero(state):
    return state[0] ** 2

def prob_one(state):
    return state[1] ** 2

s = 1 / math.sqrt(2)
superposition = [s, s]

print(prob_zero([1.0, 0.0]))
print(prob_one([1.0, 0.0]))
print(prob_zero([0.0, 1.0]))
print(prob_one([0.0, 1.0]))
print(round(prob_zero(superposition), 4))
print(round(prob_one(superposition), 4))
`,

	tests: [
		{
			name: "|0⟩ has probability 1 of measuring 0",
			code: `{{FUNC}}
print(prob_zero([1.0, 0.0]))
print(prob_one([1.0, 0.0]))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "|1⟩ has probability 1 of measuring 1",
			code: `{{FUNC}}
print(prob_zero([0.0, 1.0]))
print(prob_one([0.0, 1.0]))`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "equal superposition gives 50/50 probability",
			code: `{{FUNC}}
import math
s = 1 / math.sqrt(2)
print(round(prob_zero([s, s]), 4))
print(round(prob_one([s, s]), 4))`,
			expected: "0.5\n0.5\n",
		},
		{
			name: "probabilities sum to 1",
			code: `{{FUNC}}
import math
s = 1 / math.sqrt(2)
state = [s, s]
print(round(prob_zero(state) + prob_one(state), 10))`,
			expected: "1.0\n",
		},
	],
};
