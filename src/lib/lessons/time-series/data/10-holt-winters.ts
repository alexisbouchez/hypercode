import type { Lesson } from "../../types";

export const holtWintersLesson: Lesson = {
  id: "holt-winters",
  title: "Holt-Winters Method",
  chapterId: "arma-models",
  content: `## Holt-Winters (Holt's Linear Trend Method)

**Holt's linear trend method** extends simple exponential smoothing to capture trends by maintaining two components: a **level** and a **trend**.

### Update equations

Initialize:
\`\`\`
l[0] = xs[0]
b[0] = xs[1] - xs[0]
\`\`\`

For t = 1, 2, ...:
\`\`\`
l[t] = α · xs[t] + (1 - α) · (l[t-1] + b[t-1])
b[t] = β · (l[t] - l[t-1]) + (1 - β) · b[t-1]
\`\`\`

- \`α\` — smoothing parameter for the level
- \`β\` — smoothing parameter for the trend

### Forecasting h steps ahead

\`\`\`
ŷ[T+h] = l[T] + h · b[T]
\`\`\`

### Task

Implement:
- \`holt_linear(xs, alpha, beta)\` → returns (levels, trends) as two lists
- \`holt_forecast(xs, alpha, beta, h)\` → h-step ahead forecast
`,
  starterCode: `def holt_linear(xs, alpha, beta):
    # Returns (levels, trends) lists of same length as xs
    # Initialize: l[0] = xs[0], b[0] = xs[1] - xs[0]
    pass

def holt_forecast(xs, alpha, beta, h):
    # h-step forecast: l[-1] + h * b[-1]
    pass
`,
  solution: `def holt_linear(xs, alpha, beta):
    if len(xs) < 2:
        return [], []
    l = [xs[0]]
    b = [xs[1] - xs[0]]
    for i in range(1, len(xs)):
        l_new = alpha * xs[i] + (1 - alpha) * (l[-1] + b[-1])
        b_new = beta * (l_new - l[-1]) + (1 - beta) * b[-1]
        l.append(l_new)
        b.append(b_new)
    return l, b

def holt_forecast(xs, alpha, beta, h):
    l, b = holt_linear(xs, alpha, beta)
    return l[-1] + h * b[-1]
`,
  tests: [
    {
      name: "holt_linear on perfectly linear series",
      code: `{{FUNC}}\nl, b = holt_linear([3.0, 5.0, 7.0, 9.0, 11.0], 0.5, 0.3)\nprint([round(v, 4) for v in l])`,
      expected: "[3.0, 5.0, 7.0, 9.0, 11.0]\n",
    },
    {
      name: "holt_forecast on linear series 1-step",
      code: `{{FUNC}}\nprint(round(holt_forecast([3.0, 5.0, 7.0, 9.0, 11.0], 0.5, 0.3, 1), 4))`,
      expected: "13.0\n",
    },
    {
      name: "holt_forecast on non-linear series 1-step",
      code: `{{FUNC}}\nprint(round(holt_forecast([3.0, 5.0, 8.0, 10.0, 14.0], 0.5, 0.3, 1), 4))`,
      expected: "15.5121\n",
    },
    {
      name: "holt_forecast on non-linear series 2-step",
      code: `{{FUNC}}\nprint(round(holt_forecast([3.0, 5.0, 8.0, 10.0, 14.0], 0.5, 0.3, 2), 4))`,
      expected: "18.0105\n",
    },
  ],
};
