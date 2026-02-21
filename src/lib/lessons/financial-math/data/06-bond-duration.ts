import type { Lesson } from "../../types";

export const bondDuration: Lesson = {
  id: "bond-duration",
  title: "Bond Duration",
  chapterId: "bond-math",
  content: `## Bond Duration

**Duration** measures a bond's sensitivity to interest rate changes and its weighted-average time to receive cash flows.

### Macaulay Duration

$$D_{Mac} = \\frac{\\sum_{t=1}^{n} t \\cdot \\frac{CF_t}{(1+y)^t}}{P}$$

This is the weighted average of when cash flows arrive, weighted by their present values.

### Modified Duration

$$D_{Mod} = \\frac{D_{Mac}}{1 + y}$$

Modified duration approximates the **percentage price change** for a 1% change in yield:

$$\\frac{\\Delta P}{P} \\approx -D_{Mod} \\cdot \\Delta y$$

### Example

A 10-year, 5% coupon bond at 5% YTM has:
- Macaulay Duration ≈ 8.11 years
- Modified Duration ≈ 7.72

So if yields rise by 1%, the bond price falls by approximately 7.72%.

### Key Insights

- Zero-coupon bonds have duration = maturity
- Higher coupon → shorter duration (more early cash flows)
- Higher yield → shorter duration`,
  starterCode: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price

def macaulay_duration(face, coupon_rate, ytm, n):
    # Weighted average time to receive cash flows, weighted by PV
    pass

def modified_duration(face, coupon_rate, ytm, n):
    # Macaulay duration / (1 + ytm)
    pass`,
  solution: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price

def macaulay_duration(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = bond_price(face, coupon_rate, ytm, n)
    weighted = sum(t * coupon / (1 + ytm) ** t for t in range(1, n + 1))
    weighted += n * face / (1 + ytm) ** n
    return weighted / price

def modified_duration(face, coupon_rate, ytm, n):
    return macaulay_duration(face, coupon_rate, ytm, n) / (1 + ytm)`,
  tests: [
    {
      name: "macaulay_duration(1000, 0.05, 0.05, 10) ≈ 8.1078",
      code: `{{FUNC}}\nprint(round(macaulay_duration(1000, 0.05, 0.05, 10), 4))`,
      expected: "8.1078\n",
    },
    {
      name: "modified_duration(1000, 0.05, 0.05, 10) ≈ 7.7217",
      code: `{{FUNC}}\nprint(round(modified_duration(1000, 0.05, 0.05, 10), 4))`,
      expected: "7.7217\n",
    },
    {
      name: "macaulay_duration(1000, 0.08, 0.06, 5) ≈ 4.3422",
      code: `{{FUNC}}\nprint(round(macaulay_duration(1000, 0.08, 0.06, 5), 4))`,
      expected: "4.3422\n",
    },
    {
      name: "modified_duration(1000, 0.08, 0.06, 5) ≈ 4.0964",
      code: `{{FUNC}}\nprint(round(modified_duration(1000, 0.08, 0.06, 5), 4))`,
      expected: "4.0964\n",
    },
  ],
};
