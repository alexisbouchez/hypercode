import type { Lesson } from "../../types";

export const vegaRho: Lesson = {
  id: "vega-rho",
  title: "Vega & Rho",
  chapterId: "greeks",
  content: `## Vega & Rho

### Vega

**Vega** (ν) measures sensitivity of the option price to changes in **volatility**:

$$\\nu = \\frac{\\partial V}{\\partial \\sigma}$$

For both European calls and puts (they have the same vega):

$$\\nu = S \\cdot N'(d_1) \\cdot \\sqrt{T}$$

Vega is always **positive** — higher volatility means higher option prices (more chance of large moves). A vega of 0.375 means the option gains $0.375 for each 1% increase in volatility.

### Rho

**Rho** (ρ) measures sensitivity to changes in the **risk-free interest rate**:

$$\\rho_{\\text{call}} = \\frac{\\partial C}{\\partial r} = K \\cdot T \\cdot e^{-rT} \\cdot N(d_2)$$

Rho for calls is positive — higher interest rates increase call values (the present value of the strike decreases). Rho for puts is negative.

### Practical Importance

| Greek | Most sensitive when | Used for |
|-------|-------------------|---------|
| Vega | ATM, long expiry | Volatility trading |
| Rho | High rates, long expiry | Rate-sensitive portfolios |

Vega is typically the most important Greek for options traders after delta, as volatility changes can have large effects on option prices.`,
  starterCode: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def norm_pdf(x):
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_vega(S, K, T, r, sigma):
    """Vega of a European option (same for calls and puts)."""
    pass

def bs_rho_call(S, K, T, r, sigma):
    """Rho of a European call option."""
    pass`,
  solution: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def norm_pdf(x):
    return math.exp(-x**2 / 2) / math.sqrt(2 * math.pi)

def bs_vega(S, K, T, r, sigma):
    """Vega of a European option (same for calls and puts)."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    return S * norm_pdf(d1) * math.sqrt(T)

def bs_rho_call(S, K, T, r, sigma):
    """Rho of a European call option."""
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return K * T * math.exp(-r * T) * norm_cdf(d2)`,
  tests: [
    {
      name: "vega ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_vega(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "37.524\n",
    },
    {
      name: "rho call ATM (S=K=100, T=1, r=0.05, sigma=0.2)",
      code: `{{FUNC}}\nprint(round(bs_rho_call(100, 100, 1, 0.05, 0.2), 4))`,
      expected: "53.2325\n",
    },
    {
      name: "vega ITM call (S=110, K=100, T=1, r=0.05, sigma=0.25)",
      code: `{{FUNC}}\nprint(round(bs_vega(110, 100, 1, 0.05, 0.25), 4))`,
      expected: "34.1975\n",
    },
    {
      name: "rho call shorter expiry (T=0.5)",
      code: `{{FUNC}}\nprint(round(bs_rho_call(100, 100, 0.5, 0.05, 0.2), 4))`,
      expected: "26.4424\n",
    },
  ],
};
