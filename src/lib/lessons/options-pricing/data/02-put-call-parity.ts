import type { Lesson } from "../../types";

export const putCallParity: Lesson = {
  id: "put-call-parity",
  title: "Put-Call Parity",
  chapterId: "basics",
  content: `## Put-Call Parity

**Put-call parity** is a fundamental relationship between European call and put prices on the same underlying asset with the same strike and expiry.

### The Relationship

$$C - P = S - K e^{-rT}$$

Rearranging:

- **Call from put:** $C = P + S - K e^{-rT}$
- **Put from call:** $P = C - S + K e^{-rT}$

Where:
- C = call price
- P = put price
- S = current stock price
- K = strike price
- r = risk-free rate (continuously compounded)
- T = time to expiry (in years)

### Why Does It Hold?

Consider two portfolios:
1. Long call + cash $K e^{-rT}$
2. Long put + long stock

Both have identical payoffs at expiry, so they must have the same price today (no arbitrage).

### Example

Given: S = 100, K = 95, r = 5%, T = 1 year, P = 5.00

$$C = 5 + 100 - 95 \\cdot e^{-0.05} = 14.6332$$

Implement both conversion functions using Python's \`math.exp\`.`,
  starterCode: `import math

def pcp_call_from_put(put_price, S, K, r, T):
    """Derive call price from put price using put-call parity."""
    pass

def pcp_put_from_call(call_price, S, K, r, T):
    """Derive put price from call price using put-call parity."""
    pass`,
  solution: `import math

def pcp_call_from_put(put_price, S, K, r, T):
    """Derive call price from put price using put-call parity."""
    return put_price + S - K * math.exp(-r * T)

def pcp_put_from_call(call_price, S, K, r, T):
    """Derive put price from call price using put-call parity."""
    return call_price - S + K * math.exp(-r * T)`,
  tests: [
    {
      name: "call from put (S=100, K=95, r=0.05, T=1)",
      code: `{{FUNC}}\nprint(round(pcp_call_from_put(5, 100, 95, 0.05, 1.0), 4))`,
      expected: "14.6332\n",
    },
    {
      name: "put from call (S=100, K=100, r=0.05, T=1)",
      code: `{{FUNC}}\nprint(round(pcp_put_from_call(10.4506, 100, 100, 0.05, 1.0), 4))`,
      expected: "5.5735\n",
    },
    {
      name: "call from put (S=50, K=50, r=0.03, T=0.5)",
      code: `{{FUNC}}\nprint(round(pcp_call_from_put(3, 50, 50, 0.03, 0.5), 4))`,
      expected: "3.7444\n",
    },
    {
      name: "parity holds: call - put = S - K*exp(-rT)",
      code: `{{FUNC}}\nimport math\nC = pcp_call_from_put(5, 100, 95, 0.05, 1.0)\nP = 5.0\nprint(round(C - P, 4) == round(100 - 95*math.exp(-0.05), 4))`,
      expected: "True\n",
    },
  ],
};
