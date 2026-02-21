import type { Lesson } from "../../types";

export const intrinsicTimeValue: Lesson = {
  id: "intrinsic-time-value",
  title: "Intrinsic vs Time Value",
  chapterId: "basics",
  content: `## Intrinsic vs Time Value

An option's price (called the **premium**) has two components:

$$\\text{Option Price} = \\text{Intrinsic Value} + \\text{Time Value}$$

### Intrinsic Value

The **intrinsic value** is the payoff if the option were exercised right now — it's the "real" value based on the current stock price versus the strike.

For a call option:
$$\\text{Intrinsic Value} = \\max(S - K, 0)$$

An option is:
- **In-the-money (ITM):** intrinsic value > 0
- **At-the-money (ATM):** S ≈ K
- **Out-of-the-money (OTM):** intrinsic value = 0

### Time Value

The **time value** is the extra premium above intrinsic value. It reflects the possibility that the option could become more valuable before expiry.

$$\\text{Time Value} = \\text{Option Price} - \\text{Intrinsic Value}$$

Time value is always ≥ 0 for a European option. It:
- Decreases as expiry approaches (called **theta decay**)
- Is highest for ATM options
- Depends on volatility, time to expiry, and interest rates

### Example

A call with S = 105, K = 100, market price = 8.00:
- Intrinsic value = max(105 - 100, 0) = 5
- Time value = 8 - 5 = 3`,
  starterCode: `def intrinsic_value_call(S, K):
    """Return the intrinsic value of a call option."""
    pass

def time_value(option_price, intrinsic):
    """Return the time value of an option given its price and intrinsic value."""
    pass`,
  solution: `def intrinsic_value_call(S, K):
    """Return the intrinsic value of a call option."""
    return max(S - K, 0)

def time_value(option_price, intrinsic):
    """Return the time value of an option given its price and intrinsic value."""
    return option_price - intrinsic`,
  tests: [
    {
      name: "intrinsic value ITM call",
      code: `{{FUNC}}\nprint(intrinsic_value_call(105, 100))`,
      expected: "5\n",
    },
    {
      name: "time value with positive intrinsic",
      code: `{{FUNC}}\nprint(time_value(15, 10))`,
      expected: "5\n",
    },
    {
      name: "intrinsic value OTM call is zero",
      code: `{{FUNC}}\nprint(intrinsic_value_call(95, 100))`,
      expected: "0\n",
    },
    {
      name: "time value when option is OTM (all time value)",
      code: `{{FUNC}}\nprint(time_value(7, 0))`,
      expected: "7\n",
    },
  ],
};
