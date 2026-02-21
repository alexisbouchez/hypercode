import type { Lesson } from "../../types";

export const armaModelLesson: Lesson = {
  id: "arma-model",
  title: "ARMA(p,q) Model",
  chapterId: "stationarity",
  content: `## ARMA(p,q) Model

The **ARMA(p,q)** model combines AR and MA components:

\`\`\`
y[t] = φ₁·y[t-1] + ... + φp·y[t-p] + ε[t] + θ₁·ε[t-1] + ... + θq·ε[t-q]
\`\`\`

It is one of the most widely used models in time series analysis. ARMA models require stationarity — for non-stationary series, we use ARIMA (which adds differencing).

### Simulation

To simulate ARMA(p,q), initialize with \`xs_init\` values and pad past errors with zeros. At each step:
1. AR part: weighted sum of last \`p\` y-values
2. MA part: current error plus weighted sum of last \`q\` errors
3. New value = AR part + MA part

### One-step Forecast

For forecasting, the future error is 0 (expectation):
\`\`\`
ŷ[t+1] = φ₁·y[t] + ... + φp·y[t-p+1] + θ₁·ε[t] + ... + θq·ε[t-q+1]
\`\`\`

### Task

Implement:
- \`arma_simulate(xs_init, phi, theta, errors, n)\`
- \`arma_forecast(xs, errors, phi, theta)\`
`,
  starterCode: `def arma_simulate(xs_init, phi, theta, errors, n):
    # Simulate n steps of ARMA(p,q)
    # Pad past errors with 0s for the initial values
    pass

def arma_forecast(xs, errors, phi, theta):
    # One-step forecast (future error = 0)
    pass
`,
  solution: `def arma_simulate(xs_init, phi, theta, errors, n):
    p = len(phi)
    q = len(theta)
    xs = list(xs_init)
    all_errors = [0.0] * len(xs_init) + list(errors)
    for t in range(n):
        ar_part = sum(phi[j] * xs[-(j+1)] for j in range(min(p, len(xs))))
        e_idx = len(xs_init) + t
        cur_e = all_errors[e_idx] if e_idx < len(all_errors) else 0.0
        ma_part = cur_e + sum(theta[j] * all_errors[e_idx - j - 1] for j in range(min(q, e_idx)))
        xs.append(ar_part + ma_part)
    return xs[len(xs_init):]

def arma_forecast(xs, errors, phi, theta):
    p = len(phi)
    q = len(theta)
    ar_part = sum(phi[j] * xs[-(j+1)] for j in range(min(p, len(xs))))
    ma_part = sum(theta[j] * errors[-(j+1)] for j in range(min(q, len(errors))))
    return ar_part + ma_part
`,
  tests: [
    {
      name: "arma_simulate phi=[0.5], theta=[0.3]",
      code: `{{FUNC}}\nerrors = [0.1, -0.2, 0.3, -0.1]\nprint([round(v, 4) for v in arma_simulate([1.0], [0.5], [0.3], errors, 4)])`,
      expected: "[0.6, 0.13, 0.305, 0.1425]\n",
    },
    {
      name: "arma_forecast phi=[0.5], theta=[0.3]",
      code: `{{FUNC}}\nerrors = [0.1, -0.2, 0.3, -0.1]\nxs = [1.0] + arma_simulate([1.0], [0.5], [0.3], errors, 4)\nprint(round(arma_forecast(xs, errors, [0.5], [0.3]), 4))`,
      expected: "0.0412\n",
    },
    {
      name: "arma_simulate phi=[0.7], theta=[0.4]",
      code: `{{FUNC}}\nerrors = [0.5, -0.3, 0.2]\nprint([round(v, 4) for v in arma_simulate([2.0], [0.7], [0.4], errors, 3)])`,
      expected: "[1.9, 1.23, 0.941]\n",
    },
    {
      name: "arma_forecast phi=[0.7], theta=[0.4]",
      code: `{{FUNC}}\nerrors = [0.5, -0.3, 0.2]\nxs = [2.0] + arma_simulate([2.0], [0.7], [0.4], errors, 3)\nprint(round(arma_forecast(xs, errors, [0.7], [0.4]), 4))`,
      expected: "0.7387\n",
    },
  ],
};
