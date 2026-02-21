import type { Lesson } from "../../types";

export const ytm: Lesson = {
  id: "ytm",
  title: "Yield to Maturity",
  chapterId: "bond-math",
  content: `## Yield to Maturity

**Yield to Maturity (YTM)** is the single discount rate that equates a bond's present value of cash flows to its market price. It is the bond's implied rate of return if held to maturity.

$$P = \\sum_{t=1}^{n} \\frac{C}{(1+y)^t} + \\frac{F}{(1+y)^n}$$

There is no algebraic solution for $y$, so we solve numerically using **bisection search**.

### Bisection Algorithm

1. Set bounds: \`lo = 0.0001\`, \`hi = 0.9999\`
2. Compute \`mid = (lo + hi) / 2\`
3. If \`bond_price(mid) > market_price\`: yield must be higher → \`lo = mid\`
4. If \`bond_price(mid) < market_price\`: yield must be lower → \`hi = mid\`
5. Repeat ~1000 times

### Known Relationships

- If price = face value → YTM = coupon rate
- If price < face value → YTM > coupon rate
- If price > face value → YTM < coupon rate

These help sanity-check your implementation.`,
  starterCode: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price

def ytm(price, face, coupon_rate, n):
    # Find the yield where bond_price equals the given market price
    # Use bisection with lo=0.0001, hi=0.9999, 1000 iterations
    pass`,
  solution: `def bond_price(face, coupon_rate, y, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + y) ** t for t in range(1, n + 1))
    price += face / (1 + y) ** n
    return price

def ytm(price, face, coupon_rate, n):
    lo, hi = 0.0001, 0.9999
    for _ in range(1000):
        mid = (lo + hi) / 2
        p = bond_price(face, coupon_rate, mid, n)
        if abs(p - price) < 1e-10:
            break
        if p > price:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2`,
  tests: [
    {
      name: "ytm(1000, 1000, 0.05, 10) = 0.05 (at par)",
      code: `{{FUNC}}\nprint(round(ytm(1000, 1000, 0.05, 10), 4))`,
      expected: "0.05\n",
    },
    {
      name: "ytm(926.3991, 1000, 0.05, 10) = 0.06 (discount)",
      code: `{{FUNC}}\nprint(round(ytm(926.3991, 1000, 0.05, 10), 4))`,
      expected: "0.06\n",
    },
    {
      name: "ytm(1081.109, 1000, 0.05, 10) = 0.04 (premium)",
      code: `{{FUNC}}\nprint(round(ytm(1081.109, 1000, 0.05, 10), 4))`,
      expected: "0.04\n",
    },
    {
      name: "ytm(1084.2473, 1000, 0.08, 5) = 0.06",
      code: `{{FUNC}}\nprint(round(ytm(1084.2473, 1000, 0.08, 5), 4))`,
      expected: "0.06\n",
    },
  ],
};
