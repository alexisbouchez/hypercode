import type { Lesson } from "../../types";

export const rapidity: Lesson = {
	id: "rapidity",
	title: "Rapidity",
	chapterId: "spacetime",
	content: `## Rapidity

Velocity is a poor coordinate for relativistic motion — it is bounded by $c$ and the relativistic addition formula is unwieldy. **Rapidity** $\\phi$ is the natural alternative:

$$\\tanh\\phi = \\beta = \\frac{v}{c}, \\quad \\phi = \\text{arctanh}(\\beta)$$

Rapidity ranges from $-\\infty$ to $+\\infty$, has no upper bound, and makes velocity addition trivially additive:

$$\\phi_\\text{total} = \\phi_1 + \\phi_2$$

Converting back: $v = c\\tanh\\phi$.

### Why It Works

The Lorentz transformation in rapidity form is a hyperbolic rotation — exactly like a Euclidean rotation but with $\\cosh$ and $\\sinh$ instead of $\\cos$ and $\\sin$:

$$\\gamma = \\cosh\\phi, \\quad \\gamma\\beta = \\sinh\\phi$$

### Velocity Addition Example

Two ships each travel at $0.5c$ in the same direction. Classical result: $v = c$. Relativistic result using rapidity:

$$\\phi_\\text{total} = 2\\,\\text{arctanh}(0.5) \\approx 1.0986$$

$$v = c\\tanh(1.0986) = 0.8c$$

No matter how many $0.5c$ boosts you stack, you never reach $c$.

### Your Task

Implement \`rapidity(v)\` returning $\\phi = \\text{arctanh}(v/c)$, \`velocity_from_rapidity(phi)\` returning $v = c\\tanh\\phi$, and \`add_rapidities(phi1, phi2)\` returning the resulting velocity $c\\tanh(\\phi_1 + \\phi_2)$. Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def rapidity(v):
    c = 299792458.0
    # phi = atanh(v / c)
    pass

def velocity_from_rapidity(phi):
    c = 299792458.0
    # v = c * tanh(phi)
    pass

def add_rapidities(phi1, phi2):
    c = 299792458.0
    # total rapidity, then convert back to velocity
    pass

print(round(rapidity(0), 4))
print(round(rapidity(0.6 * 299792458.0), 4))
print(round(velocity_from_rapidity(rapidity(0.8 * 299792458.0)) / 299792458.0, 4))
print(round(add_rapidities(rapidity(0.5 * 299792458.0), rapidity(0.5 * 299792458.0)) / 299792458.0, 4))
`,

	solution: `import math

def rapidity(v):
    c = 299792458.0
    beta = v / c
    return math.atanh(beta)

def velocity_from_rapidity(phi):
    c = 299792458.0
    return c * math.tanh(phi)

def add_rapidities(phi1, phi2):
    c = 299792458.0
    total_phi = phi1 + phi2
    return c * math.tanh(total_phi)

print(round(rapidity(0), 4))
print(round(rapidity(0.6 * 299792458.0), 4))
print(round(velocity_from_rapidity(rapidity(0.8 * 299792458.0)) / 299792458.0, 4))
print(round(add_rapidities(rapidity(0.5 * 299792458.0), rapidity(0.5 * 299792458.0)) / 299792458.0, 4))
`,

	tests: [
		{
			name: "rapidity(0) = 0.0 (at rest)",
			code: `{{FUNC}}
print(round(rapidity(0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "rapidity(0.6c) = atanh(0.6) ≈ 0.6931",
			code: `{{FUNC}}
print(round(rapidity(0.6 * 299792458.0), 4))`,
			expected: "0.6931\n",
		},
		{
			name: "Round-trip: velocity_from_rapidity(rapidity(0.8c)) / c = 0.8",
			code: `{{FUNC}}
print(round(velocity_from_rapidity(rapidity(0.8 * 299792458.0)) / 299792458.0, 4))`,
			expected: "0.8\n",
		},
		{
			name: "0.5c + 0.5c via rapidity addition = 0.8c",
			code: `{{FUNC}}
print(round(add_rapidities(rapidity(0.5 * 299792458.0), rapidity(0.5 * 299792458.0)) / 299792458.0, 4))`,
			expected: "0.8\n",
		},
	],
};
