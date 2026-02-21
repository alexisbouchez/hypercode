import type { Lesson } from "../../types";

export const maModelLesson: Lesson = {
  id: "ma-model",
  title: "MA(q) Model",
  chapterId: "stationarity",
  content: `## Moving Average MA(q) Model

A **Moving Average** model of order \`q\` expresses each value as a linear combination of current and past error terms:

\`\`\`
y[t] = ε[t] + θ₁·ε[t-1] + θ₂·ε[t-2] + ... + θq·ε[t-q]
\`\`\`

where \`ε[t]\` is white noise (the error at time t) and \`θ\` are the MA coefficients.

MA models are always stationary. The ACF of an MA(q) process cuts off after lag \`q\` — a key diagnostic.

### One-step Forecast

The one-step ahead forecast for MA(q) uses past errors only (the future error is zero by expectation):
\`\`\`
ŷ[t+1] = θ₁·ε[t] + θ₂·ε[t-1] + ... + θq·ε[t-q+1]
\`\`\`

### Task

Implement:
- \`ma_simulate(errors, theta)\` — produce the MA(q) series given errors and coefficients
- \`ma_forecast(errors, theta)\` — one-step forecast using past errors
`,
  starterCode: `def ma_simulate(errors, theta):
    # y[t] = errors[t] + theta[0]*errors[t-1] + theta[1]*errors[t-2] + ...
    pass

def ma_forecast(errors, theta):
    # One-step forecast: theta[0]*errors[-1] + theta[1]*errors[-2] + ...
    pass
`,
  solution: `def ma_simulate(errors, theta):
    q = len(theta)
    n = len(errors)
    result = []
    for t in range(n):
        val = errors[t]
        for j in range(q):
            if t - j - 1 >= 0:
                val += theta[j] * errors[t - j - 1]
        result.append(val)
    return result

def ma_forecast(errors, theta):
    q = len(theta)
    forecast = sum(theta[j] * errors[-(j+1)] for j in range(min(q, len(errors))))
    return forecast
`,
  tests: [
    {
      name: "ma_simulate with theta=[0.5, 0.3]",
      code: `{{FUNC}}\nerrors = [0.5, -0.3, 0.2, -0.1, 0.4]\ntheta = [0.5, 0.3]\nprint([round(v, 4) for v in ma_simulate(errors, theta)])`,
      expected: "[0.5, -0.05, 0.2, -0.09, 0.41]\n",
    },
    {
      name: "ma_forecast with theta=[0.5, 0.3]",
      code: `{{FUNC}}\nerrors = [0.5, -0.3, 0.2, -0.1, 0.4]\ntheta = [0.5, 0.3]\nprint(round(ma_forecast(errors, theta), 4))`,
      expected: "0.17\n",
    },
    {
      name: "ma_simulate with theta=[0.6]",
      code: `{{FUNC}}\nerrors = [1.0, -0.5, 0.3, -0.2]\ntheta = [0.6]\nprint([round(v, 4) for v in ma_simulate(errors, theta)])`,
      expected: "[1.0, 0.1, 0.0, -0.02]\n",
    },
    {
      name: "ma_forecast with theta=[0.6]",
      code: `{{FUNC}}\nerrors = [1.0, -0.5, 0.3, -0.2]\ntheta = [0.6]\nprint(round(ma_forecast(errors, theta), 4))`,
      expected: "-0.12\n",
    },
  ],
};
