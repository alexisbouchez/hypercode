import type { Lesson } from "../../types";

export const normalDistribution: Lesson = {
  id: "normal-distribution",
  title: "Normal Distribution & Z-scores",
  chapterId: "risk-metrics",
  content: `## Normal Distribution & Z-scores

The **normal distribution** (Gaussian) is the cornerstone of statistical finance. Many return distributions are approximately normal, and it underpins tools like Value-at-Risk and option pricing.

**Z-score** standardizes a value relative to a distribution:
$$z = \\frac{x - \\mu}{\\sigma}$$

A z-score tells you how many standard deviations $x$ is from the mean. Z-scores above 2 or below -2 are considered unusual (occurring ~5% of the time under normality).

**Probability Density Function (PDF)**:
$$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} \\exp\\left(-\\frac{1}{2}\\left(\\frac{x-\\mu}{\\sigma}\\right)^2\\right)$$

The PDF gives the relative likelihood of observing a value $x$. It is not a probability itself (it can exceed 1 for narrow distributions), but its integral over an interval gives a probability.

### Your Task

Implement:
- \`z_score(x, mu, sigma)\` — standardized score
- \`normal_pdf(x, mu, sigma)\` — Gaussian probability density`,
  starterCode: `import math

def z_score(x, mu, sigma):
    # (x - mu) / sigma
    pass

def normal_pdf(x, mu, sigma):
    # (1 / (sigma * sqrt(2*pi))) * exp(-0.5 * ((x - mu) / sigma)^2)
    pass`,
  solution: `import math

def z_score(x, mu, sigma):
    return (x - mu) / sigma

def normal_pdf(x, mu, sigma):
    return (1 / (sigma * math.sqrt(2 * math.pi))) * math.exp(-0.5 * ((x - mu) / sigma) ** 2)`,
  tests: [
    {
      name: "z_score(75, 70, 10) equals 0.5",
      code: `{{FUNC}}
print(round(z_score(75, 70, 10), 4))`,
      expected: "0.5\n",
    },
    {
      name: "z_score(50, 70, 10) equals -2.0",
      code: `{{FUNC}}
print(round(z_score(50, 70, 10), 4))`,
      expected: "-2.0\n",
    },
    {
      name: "normal_pdf(0, 0, 1) equals ~0.3989",
      code: `{{FUNC}}
print(round(normal_pdf(0, 0, 1), 4))`,
      expected: "0.3989\n",
    },
    {
      name: "normal_pdf(1, 0, 1) equals ~0.242",
      code: `{{FUNC}}
print(round(normal_pdf(1, 0, 1), 4))`,
      expected: "0.242\n",
    },
  ],
};
