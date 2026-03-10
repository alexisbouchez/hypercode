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

### When to Use Which

- **Xavier/Glorot**: Best for sigmoid, tanh, and linear activations — assumes activations are symmetric around zero
- **He**: Best for ReLU and its variants (Leaky ReLU, GELU, SiLU) — accounts for the fact that ReLU zeros out half the inputs

Using He initialization with ReLU networks prevents the "dying neurons" problem where activations shrink to zero in deep networks.

### Your Task

Implement:
- \`xavier_init(fan_in, fan_out, seed=42)\` — weight matrix of shape \`(fan_out, fan_in)\` using uniform Xavier initialization
- \`he_init(fan_in, fan_out, seed=42)\` — weight matrix of shape \`(fan_out, fan_in)\` using He (normal) initialization: \`random.gauss(0, sqrt(2/fan_in))\``,

	starterCode: `import math
import random

def xavier_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    limit = math.sqrt(6.0 / (fan_in + fan_out))
    # Return list of fan_out rows, each of length fan_in
    # Each weight: random.uniform(-limit, limit)
    return []

def he_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    std = math.sqrt(2.0 / fan_in)
    # Return list of fan_out rows, each of length fan_in
    # Each weight: random.gauss(0, std)
    return []

weights = xavier_init(4, 3, seed=0)
limit = math.sqrt(6.0 / (4 + 3))
all_in_range = all(-limit <= w <= limit for row in weights for w in row)
print(all_in_range)    # True
print(len(weights))    # 3
print(len(weights[0])) # 4

he_w = he_init(4, 3, seed=0)
print(len(he_w))       # 3
print(len(he_w[0]))    # 4
`,

	solution: `import math
import random

def xavier_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    limit = math.sqrt(6.0 / (fan_in + fan_out))
    return [[random.uniform(-limit, limit) for _ in range(fan_in)] for _ in range(fan_out)]

def he_init(fan_in, fan_out, seed=42):
    random.seed(seed)
    std = math.sqrt(2.0 / fan_in)
    return [[random.gauss(0, std) for _ in range(fan_in)] for _ in range(fan_out)]

weights = xavier_init(4, 3, seed=0)
limit = math.sqrt(6.0 / (4 + 3))
all_in_range = all(-limit <= w <= limit for row in weights for w in row)
print(all_in_range)
print(len(weights))
print(len(weights[0]))

he_w = he_init(4, 3, seed=0)
print(len(he_w))
print(len(he_w[0]))
`,

	tests: [
		{
			name: "xavier bounds and He shape correct",
			expected: "True\n3\n4\n3\n4\n",
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
		{
			name: "He init has larger variance than Xavier for ReLU layers",
			code: `{{FUNC}}
import math
# For fan_in=100, He std = sqrt(2/100) ≈ 0.1414
# Xavier limit = sqrt(6/200) ≈ 0.1732, but uniform var = limit^2/3 ≈ 0.01
# He normal var = 2/fan_in = 0.02 — larger variance compensates for ReLU
he_w = he_init(100, 100, seed=0)
xavier_w = xavier_init(100, 100, seed=0)
he_var = sum(v**2 for row in he_w for v in row) / 10000
xavier_var = sum(v**2 for row in xavier_w for v in row) / 10000
print(he_var > xavier_var)
print(len(he_w))`,
			expected: "True\n100\n",
		},
		{
			name: "He init std scales with 1/sqrt(fan_in)",
			code: `{{FUNC}}
import math
he_small = he_init(10, 10, seed=0)
he_large = he_init(1000, 10, seed=0)
var_small = sum(v**2 for row in he_small for v in row) / 100
var_large = sum(v**2 for row in he_large for v in row) / 10000
# var_small should be ~0.2, var_large should be ~0.002
print(var_small > var_large)`,
			expected: "True\n",
		},
	],
};
