import type { Lesson } from "../../types";

export const bondPricing: Lesson = {
  id: "bond-pricing",
  title: "Bond Pricing",
  chapterId: "bond-math",
  content: `## Bond Pricing

A **bond** is a fixed-income instrument that pays periodic coupon payments and returns the face value at maturity.

### Bond Price Formula

$$P = \\sum_{t=1}^{n} \\frac{C}{(1+y)^t} + \\frac{F}{(1+y)^n}$$

Where:
- $P$ = bond price
- $C$ = coupon payment = Face × Coupon Rate
- $F$ = face value (par value)
- $y$ = yield to maturity (YTM)
- $n$ = number of periods

### Price vs. Yield Relationship

- **At par**: Coupon rate = YTM → Price = Face value
- **At discount**: Coupon rate < YTM → Price < Face value
- **At premium**: Coupon rate > YTM → Price > Face value

### Example

Bond: Face = $1000, Coupon = 5%, YTM = 6%, 10 years:
$$P = \\sum_{t=1}^{10} \\frac{50}{1.06^t} + \\frac{1000}{1.06^{10}} = \\$926.40$$

The bond trades at a discount because its coupon (5%) is below the market yield (6%).`,
  starterCode: `def bond_price(face, coupon_rate, ytm, n):
    # Price a coupon bond
    # Coupon = face * coupon_rate paid each period
    # Face value returned at period n
    pass`,
  solution: `def bond_price(face, coupon_rate, ytm, n):
    coupon = face * coupon_rate
    price = sum(coupon / (1 + ytm) ** t for t in range(1, n + 1))
    price += face / (1 + ytm) ** n
    return price`,
  tests: [
    {
      name: "bond_price(1000, 0.05, 0.05, 10) = 1000.0 (at par)",
      code: `{{FUNC}}\nprint(round(bond_price(1000, 0.05, 0.05, 10), 4))`,
      expected: "1000.0\n",
    },
    {
      name: "bond_price(1000, 0.05, 0.06, 10) ≈ 926.3991 (discount)",
      code: `{{FUNC}}\nprint(round(bond_price(1000, 0.05, 0.06, 10), 4))`,
      expected: "926.3991\n",
    },
    {
      name: "bond_price(1000, 0.05, 0.04, 10) ≈ 1081.109 (premium)",
      code: `{{FUNC}}\nprint(round(bond_price(1000, 0.05, 0.04, 10), 4))`,
      expected: "1081.109\n",
    },
    {
      name: "bond_price(1000, 0.08, 0.06, 5) ≈ 1084.2473",
      code: `{{FUNC}}\nprint(round(bond_price(1000, 0.08, 0.06, 5), 4))`,
      expected: "1084.2473\n",
    },
  ],
};
