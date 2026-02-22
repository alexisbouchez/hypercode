import type { Lesson } from "../../types";

export const weightInit: Lesson = {
	id: "weight-init",
	title: "Weight Initialization",
	chapterId: "training",
	content: `## Why Initialization Matters

All-zero weights are catastrophic: every neuron in a layer computes the same gradient, and all weights update identically — this is the **symmetry problem**. We break symmetry with random initialization.

But the *scale* of initial weights matters too:
- **Too large**: activations saturate (sigmoid outputs ≈ 0 or 1), gradients vanish
- **Too small**: signals shrink through layers, gradients vanish from the other direction

### Xavier / Glorot Initialization

For a layer with $n_{in}$ inputs and $n_{out}$ outputs, **Xavier initialization** draws weights uniformly from:

$$w \\sim \\text{Uniform}\\left(-\\sqrt{\\frac{6}{n_{in} + n_{out}}}, \\;\\sqrt{\\frac{6}{n_{in} + n_{out}}}\\right)$$

This maintains the variance of activations across layers for sigmoid/tanh networks.

### He Initialization

For ReLU networks, **He initialization** uses:

$$w \\sim \\mathcal{N}\\left(0, \\sqrt{\\frac{2}{n_{in}}}\\right)$$

ReLU kills half its inputs (the negatives), so we double the variance to compensate.

### Your Task

Implement \`xavier_init(fan_in, fan_out, seed=42)\` returning a weight matrix of shape \`(fan_out, fan_in)\` using uniform Xavier initialization.`,

	starterCode: `import math
import random

def xavier_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    limit = math.sqrt(6.0 / (fan_in + fan_out))
    # Return list of fan_out rows, each of length fan_in
    # Each weight: random.uniform(-limit, limit)
    return []

weights = xavier_init(4, 3, seed=0)
limit = math.sqrt(6.0 / (4 + 3))
all_in_range = all(-limit <= w <= limit for row in weights for w in row)
print(all_in_range)    # True
print(len(weights))    # 3
print(len(weights[0])) # 4
`,

	solution: `import math
import random

def xavier_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    limit = math.sqrt(6.0 / (fan_in + fan_out))
    return [[random.uniform(-limit, limit) for _ in range(fan_in)] for _ in range(fan_out)]

weights = xavier_init(4, 3, seed=0)
limit = math.sqrt(6.0 / (4 + 3))
all_in_range = all(-limit <= w <= limit for row in weights for w in row)
print(all_in_range)
print(len(weights))
print(len(weights[0]))
`,

	tests: [
		{
			name: "all weights within Xavier bounds, shape correct",
			expected: "True\n3\n4\n",
		},
		{
			name: "larger network still in bounds",
			code: `{{FUNC}}
w = xavier_init(100, 100, seed=7)
import math
limit = math.sqrt(6.0/200)
all_ok = all(-limit <= v <= limit for row in w for v in row)
print(all_ok)
print(len(w))
print(len(w[0]))`,
			expected: "True\n100\n100\n",
		},
		{
			name: "xavier range shrinks as layer size grows",
			code: `{{FUNC}}
import math
w_small = xavier_init(4, 4, seed=0)
w_large = xavier_init(100, 100, seed=0)
limit_small = math.sqrt(6.0/8)
limit_large = math.sqrt(6.0/200)
print(limit_small > limit_large)`,
			expected: "True\n",
		},
	],
};
