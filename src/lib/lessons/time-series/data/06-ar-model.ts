import type { Lesson } from "../../types";

export const arModelLesson: Lesson = {
  id: "ar-model",
  title: "AR(p) Model",
  chapterId: "stationarity",
  content: `## Autoregressive AR(p) Model

An **Autoregressive** model of order \`p\` expresses each value as a linear combination of its own past values:

\`\`\`
y[t] = φ₁·y[t-1] + φ₂·y[t-2] + ... + φp·y[t-p] + ε[t]
\`\`\`

where \`φ\` are the AR coefficients and \`ε[t]\` is white noise. For simplicity, our simulation omits the noise term.

AR models are stationary when the roots of the characteristic polynomial lie outside the unit circle. For AR(1): |φ₁| < 1.

### One-step Forecast

\`\`\`
ŷ[t+1] = φ₁·y[t] + φ₂·y[t-1] + ... + φp·y[t-p+1]
\`\`\`

### Task

Implement:
- \`ar_simulate(xs_init, phi, n)\` — simulate \`n\` new values starting from initial values \`xs_init\`; return only the new values
- \`ar_forecast(xs, phi)\` — one-step forecast using the last p values
`,
  starterCode: `def ar_simulate(xs_init, phi, n):
    # Simulate n new AR(p) values, return only the new values
    pass

def ar_forecast(xs, phi):
    # One-step forecast: weighted sum of last p values
    pass
`,
  solution: `def ar_simulate(xs_init, phi, n):
    p = len(phi)
    xs = list(xs_init)
    for _ in range(n):
        val = sum(phi[j] * xs[-(j+1)] for j in range(p))
        xs.append(val)
    return xs[len(xs_init):]

def ar_forecast(xs, phi):
    p = len(phi)
    return sum(phi[j] * xs[-(j+1)] for j in range(p))
`,
  tests: [
    {
      name: "ar_simulate AR(1) phi=[0.5] from [1.0], 4 steps",
      code: `{{FUNC}}\nprint([round(v, 4) for v in ar_simulate([1.0], [0.5], 4)])`,
      expected: "[0.5, 0.25, 0.125, 0.0625]\n",
    },
    {
      name: "ar_simulate AR(2) phi=[0.5, 0.2] from [1.0, 0.5], 4 steps",
      code: `{{FUNC}}\nprint([round(v, 4) for v in ar_simulate([1.0, 0.5], [0.5, 0.2], 4)])`,
      expected: "[0.45, 0.325, 0.2525, 0.1913]\n",
    },
    {
      name: "ar_forecast with phi=[0.5]",
      code: `{{FUNC}}\nprint(round(ar_forecast([1.0, 0.8, 0.6, 0.4], [0.5]), 4))`,
      expected: "0.2\n",
    },
    {
      name: "ar_forecast with phi=[0.5, 0.2]",
      code: `{{FUNC}}\nprint(round(ar_forecast([1.0, 0.8, 0.6, 0.4], [0.5, 0.2]), 4))`,
      expected: "0.32\n",
    },
  ],
};
