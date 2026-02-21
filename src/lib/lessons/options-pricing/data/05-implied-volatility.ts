import type { Lesson } from "../../types";

export const impliedVolatility: Lesson = {
  id: "implied-volatility",
  title: "Implied Volatility (Bisection)",
  chapterId: "black-scholes",
  content: `## Implied Volatility

The **implied volatility** (IV) is the volatility value σ that, when plugged into Black-Scholes, produces the observed market price of the option.

Unlike the other Black-Scholes inputs (S, K, T, r), volatility is not directly observable. Traders quote options prices in terms of their implied volatility.

### The Inverse Problem

We want to find σ such that:
$$\\text{BS}(S, K, T, r, \\sigma) = \\text{market price}$$

There is no closed-form solution, so we use a **root-finding algorithm**.

### Bisection Method

The bisection method works on a function f(σ) = BS(σ) - market_price:

1. Start with a bracket [lo, hi] = [0.001, 5.0] — we know IV lies in this range
2. Compute mid = (lo + hi) / 2
3. If f(mid) > 0, the true σ is lower: hi = mid
4. If f(mid) < 0, the true σ is higher: lo = mid
5. Repeat until |f(mid)| < 1e-6 or 100 iterations

This works because BS price is **monotonically increasing** in σ.

### Example

If the market shows a call at 10.4506 for S=K=100, T=1, r=0.05, then the implied vol is approximately 0.2000 (20%).`,
  starterCode: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_call(S, K, T, r, sigma):
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return S * norm_cdf(d1) - K * math.exp(-r * T) * norm_cdf(d2)

def bs_put(S, K, T, r, sigma):
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return K * math.exp(-r * T) * norm_cdf(-d2) - S * norm_cdf(-d1)

def implied_vol(market_price, S, K, T, r, option_type='call'):
    """Find implied volatility using bisection search."""
    pass`,
  solution: `import math

def norm_cdf(x):
    return 0.5 * math.erfc(-x / math.sqrt(2))

def bs_call(S, K, T, r, sigma):
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return S * norm_cdf(d1) - K * math.exp(-r * T) * norm_cdf(d2)

def bs_put(S, K, T, r, sigma):
    d1 = (math.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma * math.sqrt(T)
    return K * math.exp(-r * T) * norm_cdf(-d2) - S * norm_cdf(-d1)

def implied_vol(market_price, S, K, T, r, option_type='call'):
    """Find implied volatility using bisection search."""
    def price(sigma):
        if option_type == 'call':
            return bs_call(S, K, T, r, sigma)
        return bs_put(S, K, T, r, sigma)
    lo, hi = 0.001, 5.0
    mid = (lo + hi) / 2
    for _ in range(100):
        mid = (lo + hi) / 2
        diff = price(mid) - market_price
        if abs(diff) < 1e-6:
            break
        if diff > 0:
            hi = mid
        else:
            lo = mid
    return mid`,
  tests: [
    {
      name: "implied vol for ATM call (should recover 0.2)",
      code: `{{FUNC}}\nprint(round(implied_vol(10.4506, 100, 100, 1, 0.05), 4))`,
      expected: "0.2\n",
    },
    {
      name: "implied vol for ATM put (should recover 0.2)",
      code: `{{FUNC}}\nprint(round(implied_vol(5.5735, 100, 100, 1, 0.05, 'put'), 4))`,
      expected: "0.2\n",
    },
    {
      name: "implied vol for ITM call (should recover 0.25)",
      code: `{{FUNC}}\nprint(round(implied_vol(19.3051, 110, 100, 1, 0.05), 4))`,
      expected: "0.25\n",
    },
    {
      name: "IV roundtrip: BS price -> IV -> BS price",
      code: `{{FUNC}}\noriginal_sigma = 0.3\nprice = bs_call(100, 100, 1, 0.05, original_sigma)\nrecovered = implied_vol(price, 100, 100, 1, 0.05)\nprint(round(abs(recovered - original_sigma) < 1e-4))`,
      expected: "1\n",
    },
  ],
};
