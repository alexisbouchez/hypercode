import type { Lesson } from "../../types";

export const radioactiveChains: Lesson = {
	id: "radioactive-chains",
	title: "Radioactive Decay Chains",
	chapterId: "nuclear-energy",
	content: `## Radioactive Decay Chains

Many radioactive nuclei do not decay directly to a stable state — they form a **decay chain** where each daughter nucleus is itself radioactive. Understanding the time evolution of each species requires the **Bateman equations**.

### Two-Member Chain

Let $N_P$ be the number of parent nuclei and $N_D$ the number of daughter nuclei:

$$\\frac{dN_P}{dt} = -\\lambda_P N_P$$

$$\\frac{dN_D}{dt} = \\lambda_P N_P - \\lambda_D N_D$$

The parent decays exponentially:

$$N_P(t) = N_{P,0}\\, e^{-\\lambda_P t}$$

The daughter population (Bateman solution, for $\\lambda_D \\neq \\lambda_P$):

$$N_D(t) = N_{P,0}\\, \\frac{\\lambda_P}{\\lambda_D - \\lambda_P} \\left(e^{-\\lambda_P t} - e^{-\\lambda_D t}\\right)$$

### Secular Equilibrium

When the parent half-life is much longer than the daughter half-life ($t_{1/2,P} \\gg t_{1/2,D}$), after sufficient time the activities become equal — **secular equilibrium**:

$$\\frac{N_D}{N_P} = \\frac{\\lambda_P}{\\lambda_D} = \\frac{t_{1/2,D}}{t_{1/2,P}}$$

This ratio is always less than 1 (daughters are far less abundant than parents in secular equilibrium).

### Your Task

All constants must be defined **inside** each function. Use \`import math\` for \`math.exp\`.

- \`parent_nuclei(N_P0, lambda_P, t)\` — parent population at time $t$
- \`daughter_nuclei(N_P0, lambda_P, lambda_D, t)\` — daughter population via Bateman solution
- \`secular_equilibrium_ratio(lambda_P, lambda_D)\` — equilibrium ratio $\\lambda_P / \\lambda_D$`,

	starterCode: `import math

def parent_nuclei(N_P0, lambda_P, t):
    # N_P0 * exp(-lambda_P * t)
    pass

def daughter_nuclei(N_P0, lambda_P, lambda_D, t):
    # Bateman equation solution
    pass

def secular_equilibrium_ratio(lambda_P, lambda_D):
    # ratio lambda_P / lambda_D
    pass
`,

	solution: `import math

def parent_nuclei(N_P0, lambda_P, t):
    return N_P0 * math.exp(-lambda_P * t)

def daughter_nuclei(N_P0, lambda_P, lambda_D, t):
    return N_P0 * lambda_P / (lambda_D - lambda_P) * (math.exp(-lambda_P * t) - math.exp(-lambda_D * t))

def secular_equilibrium_ratio(lambda_P, lambda_D):
    return lambda_P / lambda_D
`,

	tests: [
		{
			name: "daughter_nuclei(1000, 0.1, 1.0, 5.0) ≈ 66.643635",
			code: `{{FUNC}}
print(round(daughter_nuclei(1000, 0.1, 1.0, 5.0), 6))`,
			expected: "66.643635\n",
		},
		{
			name: "parent_nuclei(1000, 0.1, 5.0) ≈ 606.53066",
			code: `{{FUNC}}
print(round(parent_nuclei(1000, 0.1, 5.0), 6))`,
			expected: "606.53066\n",
		},
		{
			name: "secular_equilibrium_ratio(0.001, 1.0) = 0.001",
			code: `{{FUNC}}
print(secular_equilibrium_ratio(0.001, 1.0))`,
			expected: "0.001\n",
		},
		{
			name: "daughter_nuclei at t=0 is always 0",
			code: `{{FUNC}}
print(daughter_nuclei(1000, 0.1, 1.0, 0))`,
			expected: "0.0\n",
		},
	],
};
