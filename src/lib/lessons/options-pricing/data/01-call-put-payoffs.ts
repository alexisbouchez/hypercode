import type { Lesson } from "../../types";

export const callPutPayoffs: Lesson = {
  id: "call-put-payoffs",
  title: "Call & Put Payoffs",
  chapterId: "basics",
  content: `## Call & Put Payoffs

Options are financial contracts that give the holder the right (but not obligation) to buy or sell an asset at a predetermined price called the **strike price** (K).

### Call Option Payoff

A **call option** gives the right to *buy* the asset. At expiration, with the asset at price S:

- If S > K: exercise the option, profit = S - K
- If S ≤ K: let the option expire worthless, profit = 0

$$\\text{Call Payoff} = \\max(S - K, 0)$$

### Put Option Payoff

A **put option** gives the right to *sell* the asset. At expiration:

- If S < K: exercise the option, profit = K - S
- If S ≥ K: let the option expire worthless, profit = 0

$$\\text{Put Payoff} = \\max(K - S, 0)$$

### Example

If K = 100:
- Stock at 110 → call pays 10, put pays 0
- Stock at 90 → call pays 0, put pays 10

Implement both payoff functions below.`,
  starterCode: `def call_payoff(S, K):
    """Return the payoff of a call option at expiration."""
    pass

def put_payoff(S, K):
    """Return the payoff of a put option at expiration."""
    pass`,
  solution: `def call_payoff(S, K):
    """Return the payoff of a call option at expiration."""
    return max(S - K, 0)

def put_payoff(S, K):
    """Return the payoff of a put option at expiration."""
    return max(K - S, 0)`,
  tests: [
    {
      name: "call payoff in-the-money",
      code: `{{FUNC}}\nprint(call_payoff(110, 100))`,
      expected: "10\n",
    },
    {
      name: "put payoff in-the-money",
      code: `{{FUNC}}\nprint(put_payoff(90, 100))`,
      expected: "10\n",
    },
    {
      name: "call payoff out-of-the-money",
      code: `{{FUNC}}\nprint(call_payoff(90, 100))`,
      expected: "0\n",
    },
    {
      name: "put payoff out-of-the-money",
      code: `{{FUNC}}\nprint(put_payoff(110, 100))`,
      expected: "0\n",
    },
  ],
};
