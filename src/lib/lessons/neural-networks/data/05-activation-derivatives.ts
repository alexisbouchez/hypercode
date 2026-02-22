import type { Lesson } from "../../types";

export const activationDerivatives: Lesson = {
	id: "activation-derivatives",
	title: "Activation Derivatives",
	chapterId: "gradients",
	content: `## Differentiating Activations

Backpropagation requires the derivative of each activation function. These derivatives appear in every gradient computation.

### Sigmoid Derivative

The sigmoid has a beautiful self-referential derivative:

$$\\frac{d\\sigma}{dx} = \\sigma(x)(1 - \\sigma(x))$$

**Derivation**: Let $s = \\sigma(x) = (1 + e^{-x})^{-1}$.

$$\\frac{ds}{dx} = \\frac{e^{-x}}{(1+e^{-x})^2} = \\frac{1}{1+e^{-x}} \\cdot \\frac{e^{-x}}{1+e^{-x}} = s(1-s)$$

Maximum value is $\\frac{1}{4}$ at $x=0$. This "saturation" near 0 or 1 causes the **vanishing gradient** problem in deep networks.

### ReLU Derivative

ReLU has a simple piecewise derivative:

$$\\frac{d\\text{ReLU}}{dx} = \\begin{cases} 1 & x > 0 \\\\ 0 & x \\leq 0 \\end{cases}$$

This is just an indicator function. Units with $x \\leq 0$ contribute **zero gradient** — the "dead ReLU" problem. But for active units, the gradient flows through unchanged.

### Your Task

Implement:
- \`sigmoid_grad(x)\` — derivative of sigmoid at $x$
- \`relu_grad(x)\` — derivative of ReLU at $x$ (return \`0.0\` when $x \\leq 0$)`,

	starterCode: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def sigmoid_grad(x):
    # sigmoid(x) * (1 - sigmoid(x))
    return 0.0

def relu_grad(x):
    # 1.0 if x > 0 else 0.0
    return 0.0

print(round(sigmoid_grad(0), 4))     # 0.25
print(round(sigmoid_grad(1.0), 4))   # 0.1966
print(relu_grad(5.0))                # 1.0
print(relu_grad(-1.0))               # 0.0
`,

	solution: `import math

def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def sigmoid_grad(x):
    s = sigmoid(x)
    return s * (1 - s)

def relu_grad(x):
    return 1.0 if x > 0 else 0.0

print(round(sigmoid_grad(0), 4))
print(round(sigmoid_grad(1.0), 4))
print(relu_grad(5.0))
print(relu_grad(-1.0))
`,

	tests: [
		{
			name: "sigmoid_grad at 0 and 1, relu_grad positive and negative",
			expected: "0.25\n0.1966\n1.0\n0.0\n",
		},
		{
			name: "sigmoid_grad is symmetric",
			code: `{{FUNC}}
print(round(sigmoid_grad(-1.0), 4))`,
			expected: "0.1966\n",
		},
		{
			name: "relu_grad exactly at zero returns 0",
			code: `{{FUNC}}
print(relu_grad(0.0))`,
			expected: "0.0\n",
		},
		{
			name: "sigmoid_grad max is 0.25 at x=0",
			code: `{{FUNC}}
vals = [sigmoid_grad(x) for x in [-5, -1, 0, 1, 5]]
print(max(vals) == sigmoid_grad(0))`,
			expected: "True\n",
		},
	],
};
