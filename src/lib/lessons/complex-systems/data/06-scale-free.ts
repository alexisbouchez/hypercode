import type { Lesson } from "../../types";

export const scaleFree: Lesson = {
  id: "scale-free",
  title: "Scale-Free Networks",
  chapterId: "networks",
  content: `# Scale-Free Networks

Many real-world networks — the internet, social networks, citation graphs — share a striking property: a few nodes have enormous numbers of connections while most nodes have very few. This is the hallmark of a **scale-free network**.

## Power Law Distributions

In a scale-free network, the probability that a node has degree $k$ follows a **power law**:

$$P(k) \\sim k^{-\\gamma}$$

where $\\gamma$ is the degree exponent (typically $2 < \\gamma < 3$). Unlike a Poisson distribution, a power law has no characteristic scale — hence "scale-free."

### PDF and CDF

The normalised power law PDF for $k \\geq k_{\\min}$ is:

$$P(k) = \\frac{\\gamma - 1}{k_{\\min}} \\left(\\frac{k}{k_{\\min}}\\right)^{-\\gamma}$$

The complementary CDF (probability that degree exceeds $k$) is:

$$P(K > k) = \\left(\\frac{k}{k_{\\min}}\\right)^{-(\\gamma-1)}$$

So the CDF is:

$$P(K \\leq k) = 1 - \\left(\\frac{k}{k_{\\min}}\\right)^{-(\\gamma-1)}$$

### Mean Degree

For $\\gamma > 2$, the mean degree is finite:

$$\\langle k \\rangle = k_{\\min} \\cdot \\frac{\\gamma - 1}{\\gamma - 2}$$

For $\\gamma \\leq 2$, the mean diverges — an important result for network robustness.

## The Barabási-Albert Model

The most famous mechanism generating scale-free networks is **preferential attachment**: when a new node joins, it connects to existing nodes with probability proportional to their current degree. Rich nodes get richer, producing a power law with $\\gamma = 3$.

## Fitting the Exponent

Given a set of observed degrees $\\{k_i\\}$, the maximum likelihood estimate of $\\gamma$ is:

$$\\hat{\\gamma} = 1 + N \\left[ \\sum_{i=1}^{N} \\ln\\frac{k_i}{k_{\\min}} \\right]^{-1}$$

where $k_{\\min} = \\min(k_i)$ and $N$ is the number of observations.

## Your Task

Implement four functions:
- \`power_law_pdf(k, gamma, k_min=1)\` — evaluate the power law PDF at degree $k$
- \`power_law_cdf(k, gamma, k_min=1)\` — evaluate the CDF at degree $k$
- \`mean_degree_power_law(gamma, k_min=1)\` — return the mean degree (for $\\gamma > 2$)
- \`fit_power_law_exponent(degrees)\` — fit $\\gamma$ by maximum likelihood given a list of degrees
`,
  starterCode: `import math

def power_law_pdf(k, gamma, k_min=1):
    pass

def power_law_cdf(k, gamma, k_min=1):
    pass

def mean_degree_power_law(gamma, k_min=1):
    pass

def fit_power_law_exponent(degrees):
    pass
`,
  solution: `import math

def power_law_pdf(k, gamma, k_min=1):
    return (gamma - 1) / k_min * (k / k_min) ** (-gamma)

def power_law_cdf(k, gamma, k_min=1):
    return 1 - (k / k_min) ** (-(gamma - 1))

def mean_degree_power_law(gamma, k_min=1):
    return k_min * (gamma - 1) / (gamma - 2)

def fit_power_law_exponent(degrees):
    k_min = min(degrees)
    N = len(degrees)
    log_sum = sum(math.log(k / k_min) for k in degrees)
    return 1 + N / log_sum
`,
  tests: [
    {
      name: "power_law_pdf(1, 3.0, 1) equals 2.0",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{power_law_pdf(1, 3.0, 1):.4f}")`,
    },
    {
      name: "power_law_cdf(2, 3.0, 1) equals 0.75",
      expected: "0.7500\n",
      code: `{{FUNC}}
print(f"{power_law_cdf(2, 3.0, 1):.4f}")`,
    },
    {
      name: "mean_degree_power_law(3.0, 1) equals 2.0",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{mean_degree_power_law(3.0, 1):.4f}")`,
    },
    {
      name: "fit_power_law_exponent fits gamma from sample degrees",
      expected: "2.2772\n",
      code: `{{FUNC}}
print(f"{fit_power_law_exponent([1, 1, 2, 2, 3, 4, 5]):.4f}")`,
    },
  ],
};
