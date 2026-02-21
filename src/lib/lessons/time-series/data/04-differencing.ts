import type { Lesson } from "../../types";

export const differencingLesson: Lesson = {
  id: "differencing",
  title: "Differencing & Integration",
  chapterId: "autocorrelation",
  content: `## Differencing & Integration

**Differencing** transforms a non-stationary series into a stationary one by computing successive differences. A series differenced \`d\` times is said to be integrated of order \`d\`, written I(d).

First-order differencing:
\`\`\`
Δxs[t] = xs[t] - xs[t-1]
\`\`\`

Second-order differencing applies it twice:
\`\`\`
Δ²xs[t] = Δxs[t] - Δxs[t-1]
\`\`\`

**Undifferencing** (integration) reconstructs the original series from differences and an initial value:
\`\`\`
xs[0] = x0
xs[t] = xs[t-1] + dxs[t-1]
\`\`\`

### Task

Implement:
- \`difference(xs, d=1)\` — apply d-th order differencing
- \`undifference(dxs, x0)\` — reconstruct series from first differences and starting value
`,
  starterCode: `def difference(xs, d=1):
    # Apply d-th order differencing
    pass

def undifference(dxs, x0):
    # Reconstruct series: start at x0, accumulate dxs
    pass
`,
  solution: `def difference(xs, d=1):
    result = list(xs)
    for _ in range(d):
        result = [result[i+1] - result[i] for i in range(len(result)-1)]
    return result

def undifference(dxs, x0):
    result = [x0]
    for dx in dxs:
        result.append(result[-1] + dx)
    return result
`,
  tests: [
    {
      name: "first-order difference of [1,3,6,10,15]",
      code: `{{FUNC}}\nprint(difference([1,3,6,10,15]))`,
      expected: "[2, 3, 4, 5]\n",
    },
    {
      name: "second-order difference of [1,3,6,10,15]",
      code: `{{FUNC}}\nprint(difference([1,3,6,10,15], 2))`,
      expected: "[1, 1, 1]\n",
    },
    {
      name: "undifference reconstructs original series",
      code: `{{FUNC}}\nprint(undifference([2,3,4,5], 1))`,
      expected: "[1, 3, 6, 10, 15]\n",
    },
    {
      name: "difference then undifference is identity",
      code: `{{FUNC}}\nxs = [1, 4, 9, 16, 25]\ndxs = difference(xs)\nprint(undifference(dxs, xs[0]))`,
      expected: "[1, 4, 9, 16, 25]\n",
    },
  ],
};
