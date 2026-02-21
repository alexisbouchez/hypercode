import type { Lesson } from "../../types";

export const expSmoothingLesson: Lesson = {
  id: "exp-smoothing",
  title: "Exponential Smoothing",
  chapterId: "arma-models",
  content: `## Exponential Smoothing

**Simple Exponential Smoothing (SES)** forecasts by computing a weighted average where recent observations get higher weight. The weights decay exponentially into the past.

The smoothed series \`s\` is computed as:
\`\`\`
s[0] = xs[0]
s[t] = α · xs[t] + (1 - α) · s[t-1]
\`\`\`

where \`α ∈ (0, 1)\` is the smoothing parameter:
- **α close to 1**: reacts quickly to recent data
- **α close to 0**: heavy smoothing, slow to react

### Forecast

The one-step ahead forecast is simply the last smoothed value:
\`\`\`
ŷ[t+1] = s[t]
\`\`\`

### Task

Implement:
- \`exp_smooth(xs, alpha)\` → list of smoothed values (same length as xs)
- \`exp_smooth_forecast(xs, alpha)\` → one-step ahead forecast
`,
  starterCode: `def exp_smooth(xs, alpha):
    # s[0] = xs[0]; s[t] = alpha*xs[t] + (1-alpha)*s[t-1]
    pass

def exp_smooth_forecast(xs, alpha):
    # Forecast = last smoothed value
    pass
`,
  solution: `def exp_smooth(xs, alpha):
    if not xs:
        return []
    s = [xs[0]]
    for i in range(1, len(xs)):
        s.append(alpha * xs[i] + (1 - alpha) * s[-1])
    return s

def exp_smooth_forecast(xs, alpha):
    s = exp_smooth(xs, alpha)
    return s[-1]
`,
  tests: [
    {
      name: "exp_smooth([1,2,3,4,5], alpha=0.3)",
      code: `{{FUNC}}\nprint([round(v, 4) for v in exp_smooth([1.0, 2.0, 3.0, 4.0, 5.0], 0.3)])`,
      expected: "[1.0, 1.3, 1.81, 2.467, 3.2269]\n",
    },
    {
      name: "exp_smooth_forecast([1,2,3,4,5], alpha=0.3)",
      code: `{{FUNC}}\nprint(round(exp_smooth_forecast([1.0, 2.0, 3.0, 4.0, 5.0], 0.3), 4))`,
      expected: "3.2269\n",
    },
    {
      name: "exp_smooth([10,12,9,11,13], alpha=0.5)",
      code: `{{FUNC}}\nprint([round(v, 4) for v in exp_smooth([10.0, 12.0, 9.0, 11.0, 13.0], 0.5)])`,
      expected: "[10.0, 11.0, 10.0, 10.5, 11.75]\n",
    },
    {
      name: "exp_smooth_forecast([10,12,9,11,13], alpha=0.5)",
      code: `{{FUNC}}\nprint(round(exp_smooth_forecast([10.0, 12.0, 9.0, 11.0, 13.0], 0.5), 4))`,
      expected: "11.75\n",
    },
  ],
};
