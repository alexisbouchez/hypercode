import type { Lesson } from "../../types";

export const lengthContraction: Lesson = {
	id: "length-contraction",
	title: "Length Contraction",
	chapterId: "kinematics",
	content: `## Length Contraction

Special relativity does not only affect time — it also affects space. A moving object is **shorter** along its direction of motion as measured by a stationary observer.

### The Formula

If an object has **rest length** $L_0$ (measured in its own rest frame), then in a frame where the object moves at velocity $v$, its measured length is:

$$L = \\frac{L_0}{\\gamma}$$

Because $\\gamma \\geq 1$, the observed length $L \\leq L_0$. At $v = 0$: $L = L_0$. As $v \\to c$: $L \\to 0$.

Only the dimension **along the direction of motion** contracts. Perpendicular dimensions are completely unchanged.

### Recovering the Rest Length

Inverting the relation:

$$L_0 = L\\,\\gamma$$

If you know the contracted length and the speed, you can recover the rest length.

### Symmetry

Contraction is **reciprocal**: if frame $A$ sees frame $B$'s objects as shortened, then frame $B$ equally sees frame $A$'s objects as shortened by the same factor. Neither frame is privileged. The effect is real but not an optical illusion — it reflects the geometry of spacetime.

### Example: A Spaceship

A spaceship of rest length $L_0 = 100$ m traveling at $v = 0.8c$ (where $\\gamma = 5/3$) appears only $60$ m long to an observer at rest. To the astronauts on board, the ship is still $100$ m.

### Your Task

Implement:
- \`length_contraction(L0, v)\` — returns contracted length $L = L_0 / \\gamma$
- \`rest_length(L, v)\` — returns rest length $L_0 = L \\times \\gamma$

Use $c = 299792458.0$ m/s, defined inside each function.`,

	starterCode: `import math

def length_contraction(L0, v):
    c = 299792458.0
    # TODO: compute gamma and return L0 / gamma
    pass

def rest_length(L, v):
    c = 299792458.0
    # TODO: compute gamma and return L * gamma
    pass

print(round(length_contraction(100.0, 0.6 * 299792458.0), 4))
print(round(length_contraction(100.0, 0.8 * 299792458.0), 4))
print(round(rest_length(80.0, 0.6 * 299792458.0), 4))
print(length_contraction(500.0, 0))
`,

	solution: `import math

def length_contraction(L0, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return L0 / gamma

def rest_length(L, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return L * gamma

print(round(length_contraction(100.0, 0.6 * 299792458.0), 4))
print(round(length_contraction(100.0, 0.8 * 299792458.0), 4))
print(round(rest_length(80.0, 0.6 * 299792458.0), 4))
print(length_contraction(500.0, 0))
`,

	tests: [
		{
			name: "100 m at 0.6c contracts to 80.0 m (γ = 1.25)",
			code: `{{FUNC}}
print(round(length_contraction(100.0, 0.6 * 299792458.0), 4))`,
			expected: "80.0\n",
		},
		{
			name: "100 m at 0.8c contracts to 60.0 m (γ = 5/3)",
			code: `{{FUNC}}
print(round(length_contraction(100.0, 0.8 * 299792458.0), 4))`,
			expected: "60.0\n",
		},
		{
			name: "rest_length(80.0, 0.6c) = 100.0 m (inverse of contraction)",
			code: `{{FUNC}}
print(round(rest_length(80.0, 0.6 * 299792458.0), 4))`,
			expected: "100.0\n",
		},
		{
			name: "no contraction at v=0: length_contraction(500.0, 0) = 500.0",
			code: `{{FUNC}}
print(length_contraction(500.0, 0))`,
			expected: "500.0\n",
		},
	],
};
