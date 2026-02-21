import type { Lesson } from "../../types";

export const continuousCompounding: Lesson = {
  id: "continuous-compounding",
  title: "Continuous Compounding",
  chapterId: "interest-rate-models",
  content: `## Continuous Compounding

**Continuous compounding** is the limit of compounding $n$ times per year as $n \\to \\infty$:

$$\\lim_{n \\to \\infty} \\left(1 + \\frac{r}{n}\\right)^{nt} = e^{rt}$$

### Continuous Future Value & Present Value

$$FV = PV \\cdot e^{rt}$$
$$PV = FV \\cdot e^{-rt}$$

This is widely used in derivatives pricing, stochastic calculus, and option theory.

### Converting Between Rates

A discrete rate $r_d$ compounded $n$ times per year is equivalent to continuous rate $r_c$:

$$r_c = n \\cdot \\ln\\left(1 + \\frac{r_d}{n}\\right)$$

For annual compounding ($n = 1$):
$$r_c = \\ln(1 + r_d)$$

### Example

$1000 invested at 5% continuously for 2 years:
$$FV = 1000 \\cdot e^{0.05 \\times 2} = 1000 \\cdot e^{0.1} \\approx \\$1105.17$$

Use Python's \`math.exp()\` and \`math.log()\` functions.`,
  starterCode: `import math

def cont_fv(pv, r, t):
    # Future value with continuous compounding
    pass

def cont_pv(fv, r, t):
    # Present value with continuous compounding
    pass

def disc_to_cont(r_disc, n):
    # Convert discrete rate (compounded n times/year) to equivalent continuous rate
    pass`,
  solution: `import math

def cont_fv(pv, r, t):
    return pv * math.exp(r * t)

def cont_pv(fv, r, t):
    return fv * math.exp(-r * t)

def disc_to_cont(r_disc, n):
    return n * math.log(1 + r_disc / n)`,
  tests: [
    {
      name: "cont_fv(1000, 0.05, 2) ≈ 1105.1709",
      code: `{{FUNC}}\nprint(round(cont_fv(1000, 0.05, 2), 4))`,
      expected: "1105.1709\n",
    },
    {
      name: "cont_pv(1000, 0.05, 2) ≈ 904.8374",
      code: `{{FUNC}}\nprint(round(cont_pv(1000, 0.05, 2), 4))`,
      expected: "904.8374\n",
    },
    {
      name: "disc_to_cont(0.05, 1) ≈ 0.04879",
      code: `{{FUNC}}\nprint(round(disc_to_cont(0.05, 1), 6))`,
      expected: "0.04879\n",
    },
    {
      name: "disc_to_cont(0.05, 12) ≈ 0.049896",
      code: `{{FUNC}}\nprint(round(disc_to_cont(0.05, 12), 6))`,
      expected: "0.049896\n",
    },
  ],
};
