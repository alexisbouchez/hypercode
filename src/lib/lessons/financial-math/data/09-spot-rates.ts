import type { Lesson } from "../../types";

export const spotRates: Lesson = {
  id: "spot-rates",
  title: "Spot Rates & Zero Curve",
  chapterId: "interest-rate-models",
  content: `## Spot Rates & Zero Curve

A **spot rate** (or zero rate) is the yield on a zero-coupon bond — the return on a single cash flow at a specific future date.

### Zero-Coupon Bond Pricing

A zero-coupon bond pays only face value $F$ at maturity, with no coupons:

$$P = \\frac{F}{(1 + s_n)^n}$$

Solving for the spot rate $s_n$:

$$s_n = \\left(\\frac{F}{P}\\right)^{1/n} - 1$$

### Zero Curve

The **zero curve** (or spot curve) plots spot rates across maturities. It shows the pure time value of money at each horizon, free of coupon reinvestment assumptions.

Unlike YTM (which blends rates across periods), each spot rate is a clean rate for exactly that time horizon.

### Examples

| Price | Face | Maturity | Spot Rate |
|-------|------|----------|-----------|
| 975   | 1000 | 1 year   | 2.56%     |
| 950   | 1000 | 2 years  | 2.60%     |
| 900   | 1000 | 5 years  | 2.13%     |`,
  starterCode: `def spot_rate(price, face, n):
    # Compute the n-period spot rate from a zero-coupon bond price
    pass`,
  solution: `def spot_rate(price, face, n):
    return (face / price) ** (1 / n) - 1`,
  tests: [
    {
      name: "spot_rate(950, 1000, 2) ≈ 0.026",
      code: `{{FUNC}}\nprint(round(spot_rate(950, 1000, 2), 4))`,
      expected: "0.026\n",
    },
    {
      name: "spot_rate(900, 1000, 5) ≈ 0.0213",
      code: `{{FUNC}}\nprint(round(spot_rate(900, 1000, 5), 4))`,
      expected: "0.0213\n",
    },
    {
      name: "spot_rate(800, 1000, 10) ≈ 0.0226",
      code: `{{FUNC}}\nprint(round(spot_rate(800, 1000, 10), 4))`,
      expected: "0.0226\n",
    },
    {
      name: "spot_rate(975, 1000, 1) ≈ 0.0256",
      code: `{{FUNC}}\nprint(round(spot_rate(975, 1000, 1), 4))`,
      expected: "0.0256\n",
    },
  ],
};
