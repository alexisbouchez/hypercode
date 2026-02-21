import type { Lesson } from "../../types";

export const bondConvexity: Lesson = {
  id: "bond-convexity",
  title: "Bond Convexity",
  chapterId: "bond-math",
  content: `## Bond Convexity

**Convexity** is the second-order measure of a bond's price sensitivity to yield changes. It captures the curvature in the price-yield relationship.

### Full Price Change Approximation

$$\\frac{\\Delta P}{P} \\approx -D_{Mod} \\cdot \\Delta y + \\frac{1}{2} \\cdot C \\cdot (\\Delta y)^2$$

The convexity term always adds value (positive for standard bonds), meaning bonds gain more when yields fall than they lose when yields rise.

### Convexity Formula

$$C = \\frac{1}{P} \\sum_{t=1}^{n} \\frac{t(t+1) \\cdot CF_t}{(1+y)^{t+2}}$$

For a coupon bond:

$$C = \\frac{1}{P} \\left[ \\sum_{t=1}^{n} \\frac{t(t+1) \\cdot C}{(1+y)^{t+2}} + \\frac{n(n+1) \\cdot F}{(1+y)^{n+2}} \\right]$$

### Properties

- Longer maturity → higher convexity
- Lower coupon → higher convexity
- Lower yield → higher convexity
- Convexity is always positive for standard bonds`,
  starterCode: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price

def bond_convexity(face, coupon_rate, ytm, n):
    # Second-order sensitivity: sum of t*(t+1)*CF/(1+y)^(t+2) divided by price
    pass`,
  solution: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price

def bond_convexity(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = bond_price(face, coupon_rate, ytm, n)
    conv = sum(t * (t + 1) * coupon / (1 + ytm) ** (t + 2) for t in range(1, n + 1))
    conv += n * (n + 1) * face / (1 + ytm) ** (n + 2)
    return conv / price`,
  tests: [
    {
      name: "bond_convexity(1000, 0.05, 0.05, 10) ≈ 74.9977",
      code: `{{FUNC}}\nprint(round(bond_convexity(1000, 0.05, 0.05, 10), 4))`,
      expected: "74.9977\n",
    },
    {
      name: "bond_convexity(1000, 0.08, 0.06, 5) ≈ 22.05",
      code: `{{FUNC}}\nprint(round(bond_convexity(1000, 0.08, 0.06, 5), 4))`,
      expected: "22.05\n",
    },
    {
      name: "bond_convexity(1000, 0.05, 0.06, 10) ≈ 72.5693",
      code: `{{FUNC}}\nprint(round(bond_convexity(1000, 0.05, 0.06, 10), 4))`,
      expected: "72.5693\n",
    },
  ],
};
