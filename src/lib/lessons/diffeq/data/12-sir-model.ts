import type { Lesson } from "../../types";

export const sirModel: Lesson = {
	id: "sir-model",
	title: "SIR Epidemic Model",
	chapterId: "applications",
	content: `## SIR Epidemic Model

The **SIR model** divides a population into three compartments:

- **S** (Susceptible): can get infected
- **I** (Infected): currently infectious
- **R** (Recovered): immune

### Equations

\`\`\`
dS/dt = -β · S · I / N
dI/dt =  β · S · I / N  -  γ · I
dR/dt =  γ · I
\`\`\`

- \`β\`: transmission rate (contacts per day × probability of transmission)
- \`γ\`: recovery rate (1/days infectious)
- \`N = S + I + R\`: total population (constant)

### Key Metric: Basic Reproduction Number

\`\`\`
R₀ = β / γ
\`\`\`

- \`R₀ > 1\`: epidemic grows (each infected person infects more than one)
- \`R₀ < 1\`: epidemic dies out

COVID-19 had R₀ ≈ 2-3. Measles has R₀ ≈ 12-18.

### Herd Immunity Threshold

An epidemic can't grow if enough people are immune. The threshold:

\`\`\`
fraction immune needed = 1 - 1/R₀
\`\`\`

For R₀ = 3: need 1 - 1/3 = 67% immune.

### Your Task

Implement \`sir(S0, I0, R0, beta, gamma, t_end, n)\` using Euler's method. Return \`(S, I, R)\` at time \`t_end\`.`,

	starterCode: `def sir(S0, I0, R0, beta, gamma, t_end, n):
    h = t_end / n
    S, I, R = float(S0), float(I0), float(R0)
    N = S + I + R
    for _ in range(n):
        dS = -beta * S * I / N
        dI = beta * S * I / N - gamma * I
        dR = gamma * I
        S = S + h * dS
        I = I + h * dI
        R = R + h * dR
    return S, I, R

# No infected → nothing changes
S, I, R = sir(1000, 0, 0, 0.3, 0.1, 10, 100)
print(round(S), round(I), round(R))

# Population conservation
S, I, R = sir(990, 10, 0, 0.3, 0.1, 30, 1000)
print(round(S + I + R))
`,

	solution: `def sir(S0, I0, R0, beta, gamma, t_end, n):
    h = t_end / n
    S, I, R = float(S0), float(I0), float(R0)
    N = S + I + R
    for _ in range(n):
        dS = -beta * S * I / N
        dI = beta * S * I / N - gamma * I
        dR = gamma * I
        S = S + h * dS
        I = I + h * dI
        R = R + h * dR
    return S, I, R

S, I, R = sir(1000, 0, 0, 0.3, 0.1, 10, 100)
print(round(S), round(I), round(R))

S, I, R = sir(990, 10, 0, 0.3, 0.1, 30, 1000)
print(round(S + I + R))
`,

	tests: [
		{
			name: "no infected: everything stays the same",
			code: `{{FUNC}}
S, I, R = sir(1000, 0, 0, 0.3, 0.1, 10, 100)
print(round(S), round(I), round(R))`,
			expected: "1000 0 0\n",
		},
		{
			name: "population is conserved",
			code: `{{FUNC}}
S, I, R = sir(990, 10, 0, 0.3, 0.1, 30, 1000)
print(round(S + I + R))`,
			expected: "1000\n",
		},
		{
			name: "epidemic eventually burns out",
			code: `{{FUNC}}
S, I, R = sir(990, 10, 0, 0.3, 0.1, 200, 10000)
print(round(I) < 1)`,
			expected: "True\n",
		},
		{
			name: "R only increases over time",
			code: `{{FUNC}}
S, I, R = sir(990, 10, 0, 0.3, 0.1, 1, 100)
print(R > 0)`,
			expected: "True\n",
		},
	],
};
