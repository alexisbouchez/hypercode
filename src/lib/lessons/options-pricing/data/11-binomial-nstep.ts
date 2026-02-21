import type { Lesson } from "../../types";

export const binomialNstep: Lesson = {
  id: "binomial-nstep",
  title: "Binomial Tree (N-Step)",
  chapterId: "numerical-methods",
  content: `## N-Step Binomial Tree

The 1-step model is too coarse. With n steps, we subdivide T into n intervals of length Δt = T/n, giving a much more accurate approximation of the continuous Black-Scholes model.

### CRR Parameters for N Steps

$$\\Delta t = T/n$$
$$u = e^{\\sigma\\sqrt{\\Delta t}}, \\quad d = 1/u$$
$$p = \\frac{e^{r\\Delta t} - d}{u - d}$$

### Tree Structure

At step n, the stock can be at any of n+1 values:

$$S_{n,j} = S \\cdot u^{n-2j} \\quad \\text{for } j = 0, 1, \\ldots, n$$

### Backward Induction Algorithm

1. **Compute terminal payoffs** at time n for all n+1 nodes
2. **Work backwards**: at each step, the value at node j is:
   $$V_j = e^{-r\\Delta t}(p \\cdot V_j^{\\text{up}} + (1-p) \\cdot V_j^{\\text{down}})$$
3. Repeat until you reach the root node (t=0)

### Convergence

As n → ∞, the binomial price converges to the Black-Scholes price. With n = 100 steps, the error is typically less than 0.05%.

The key insight: more steps = finer time grid = better approximation of continuous-time Brownian motion.`,
  starterCode: `import math

def binomial_call_nstep(S, K, T, r, sigma, n):
    """Price a European call using an n-step CRR binomial tree."""
    pass`,
  solution: `import math

def binomial_call_nstep(S, K, T, r, sigma, n):
    """Price a European call using an n-step CRR binomial tree."""
    dt = T / n
    u = math.exp(sigma * math.sqrt(dt))
    d = 1 / u
    p = (math.exp(r * dt) - d) / (u - d)
    disc = math.exp(-r * dt)
    # Terminal payoffs
    values = [max(S * u**(n - 2*j) - K, 0) for j in range(n + 1)]
    # Backward induction
    for i in range(n - 1, -1, -1):
        values = [disc * (p * values[j] + (1 - p) * values[j + 1]) for j in range(i + 1)]
    return values[0]`,
  tests: [
    {
      name: "100-step call converges near BS price (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(binomial_call_nstep(100, 100, 1, 0.05, 0.2, 100), 4))`,
      expected: "10.4306\n",
    },
    {
      name: "50-step call (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(binomial_call_nstep(100, 100, 1, 0.05, 0.2, 50), 4))`,
      expected: "10.4107\n",
    },
    {
      name: "100-step ITM call (S=110, K=100, T=1, r=0.05, sigma=0.25)",
      code: `{{FUNC}}\nprint(round(binomial_call_nstep(110, 100, 1, 0.05, 0.25, 100), 4))`,
      expected: "19.2978\n",
    },
    {
      name: "more steps gives closer approximation to BS",
      code: `{{FUNC}}\nimport math\ndef norm_cdf(x):\n    return 0.5 * math.erfc(-x / math.sqrt(2))\ndef bs_call(S, K, T, r, sigma):\n    d1 = (math.log(S/K) + (r + sigma**2/2)*T) / (sigma*math.sqrt(T))\n    d2 = d1 - sigma*math.sqrt(T)\n    return S*norm_cdf(d1) - K*math.exp(-r*T)*norm_cdf(d2)\nbs = bs_call(100, 100, 1, 0.05, 0.2)\nb10 = binomial_call_nstep(100, 100, 1, 0.05, 0.2, 10)\nb100 = binomial_call_nstep(100, 100, 1, 0.05, 0.2, 100)\nprint(abs(b100 - bs) < abs(b10 - bs))`,
      expected: "True\n",
    },
  ],
};
