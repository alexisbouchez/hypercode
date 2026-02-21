import type { Lesson } from "../../types";

export const relativisticMomentum: Lesson = {
	id: "relativistic-momentum",
	title: "Relativistic Momentum",
	chapterId: "dynamics",
	content: `## Relativistic Momentum

Classical momentum $p = mv$ is not conserved in all inertial frames. The relativistic fix multiplies by $\\gamma$:

$$p = \\gamma m v = \\frac{mv}{\\sqrt{1 - v^2/c^2}}$$

where $m$ is the **rest mass** — an invariant property of the particle. At low speeds $\\gamma \\approx 1$ and we recover $p \\approx mv$. As $v \\to c$, $p \\to \\infty$: no finite force can push a massive object to light speed.

### Force and Momentum

Newton's second law keeps the same form in relativity: $F = dp/dt$. But since $p = \\gamma mv$ and $\\gamma$ itself depends on $v$, the force needed to maintain constant acceleration grows without bound as $v \\to c$.

### Momentum Ratio

The ratio of relativistic to classical momentum is simply $\\gamma$:

$$\\frac{p_\\text{rel}}{p_\\text{class}} = \\gamma$$

| $v/c$ | $\\gamma$ | Ratio |
|--------|---------|-------|
| 0.1 | 1.005 | 0.5% correction |
| 0.6 | 1.25 | 25% correction |
| 0.8 | 1.667 | 67% correction |
| 0.99 | ≈7.09 | 609% correction |

### Your Task

Implement \`relativistic_momentum(m, v)\` returning $p = \\gamma mv$, and \`momentum_ratio(v)\` returning $\\gamma$ — the factor by which relativistic momentum exceeds classical momentum. Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def relativistic_momentum(m, v):
    c = 299792458.0
    # gamma = 1 / sqrt(1 - (v/c)^2)
    # p = gamma * m * v
    pass

def momentum_ratio(v):
    c = 299792458.0
    # return gamma = 1 / sqrt(1 - (v/c)^2)
    pass

print(relativistic_momentum(1.0, 0))
print(round(relativistic_momentum(1.0, 0.6 * 299792458.0) / 299792458.0, 4))
print(round(momentum_ratio(0.8 * 299792458.0), 4))
print(round(relativistic_momentum(10.0, 100.0), 4))
`,

	solution: `import math

def relativistic_momentum(m, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return gamma * m * v

def momentum_ratio(v):
    c = 299792458.0
    return 1.0 / math.sqrt(1 - (v / c) ** 2)

print(relativistic_momentum(1.0, 0))
print(round(relativistic_momentum(1.0, 0.6 * 299792458.0) / 299792458.0, 4))
print(round(momentum_ratio(0.8 * 299792458.0), 4))
print(round(relativistic_momentum(10.0, 100.0), 4))
`,

	tests: [
		{
			name: "v=0: relativistic_momentum(1.0, 0) = 0.0",
			code: `{{FUNC}}
print(relativistic_momentum(1.0, 0))`,
			expected: "0.0\n",
		},
		{
			name: "v=0.6c, m=1 kg: p/c = 0.75 (γ=1.25, p=1.25×0.6c)",
			code: `{{FUNC}}
print(round(relativistic_momentum(1.0, 0.6 * 299792458.0) / 299792458.0, 4))`,
			expected: "0.75\n",
		},
		{
			name: "momentum_ratio(0.8c) = 1.6667 (γ = 5/3)",
			code: `{{FUNC}}
print(round(momentum_ratio(0.8 * 299792458.0), 4))`,
			expected: "1.6667\n",
		},
		{
			name: "Classical limit: m=10 kg, v=100 m/s → p ≈ 1000.0 kg·m/s",
			code: `{{FUNC}}
print(round(relativistic_momentum(10.0, 100.0), 4))`,
			expected: "1000.0\n",
		},
	],
};
