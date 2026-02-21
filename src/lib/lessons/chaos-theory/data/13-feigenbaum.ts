import type { Lesson } from "../../types";

export const feigenbaum: Lesson = {
  id: "feigenbaum",
  title: "Feigenbaum Constants",
  chapterId: "routes-to-chaos",
  content: `## Feigenbaum Constants

One of the most surprising discoveries in chaos theory is that the **period-doubling route to chaos** is universal — the same quantitative ratios appear in virtually every system that undergoes this transition, from logistic maps to dripping faucets to electronic circuits.

As the parameter r of the logistic map increases, the stable period doubles: 1 → 2 → 4 → 8 → … Each period-doubling occurs at a specific value r_n. The **Feigenbaum constant** δ is defined by:

$$\\delta = \\lim_{n \\to \\infty} \\frac{r_n - r_{n-1}}{r_{n+1} - r_n} \\approx 4.6692$$

This constant appears universally in one-dimensional maps with a quadratic maximum. It was discovered by Mitchell Feigenbaum in 1975 and was one of the first indicators that chaos has deep mathematical structure.

**Implement the following functions:**
- \`find_period(r, x0, n_transient, n_test, tol)\` — find the period of the logistic map orbit at the given r value, after discarding n_transient transient iterations
- \`find_bifurcation(p_target, r_lo, r_hi, x0, n_transient, n_test, tol, n_bisect)\` — find the bifurcation point where the period first reaches p_target, using bisection
- \`feigenbaum_ratio(r1, r2, r3)\` — compute δ ≈ (r2 - r1) / (r3 - r2) from three consecutive bifurcation points`,
  starterCode: `def find_period(r, x0, n_transient, n_test, tol):
    # Iterate n_transient times to settle, then find period of orbit
    pass

def find_bifurcation(p_target, r_lo, r_hi, x0, n_transient, n_test, tol, n_bisect):
    # Find r where period first reaches p_target via bisection
    pass

def feigenbaum_ratio(r1, r2, r3):
    # Approximate delta = (r2 - r1) / (r3 - r2)
    pass`,
  solution: `def find_period(r, x0, n_transient, n_test, tol):
    x = x0
    for _ in range(n_transient):
        x = r * x * (1.0 - x)
    orbit = []
    for _ in range(n_test):
        orbit.append(x)
        x = r * x * (1.0 - x)
    x_ref = orbit[0]
    for p in range(1, n_test):
        if abs(orbit[p] - x_ref) < tol:
            return p
    return n_test

def find_bifurcation(p_target, r_lo, r_hi, x0, n_transient, n_test, tol, n_bisect):
    for _ in range(n_bisect):
        r_mid = (r_lo + r_hi) / 2.0
        p = find_period(r_mid, x0, n_transient, n_test, tol)
        if p >= p_target:
            r_hi = r_mid
        else:
            r_lo = r_mid
    return (r_lo + r_hi) / 2.0

def feigenbaum_ratio(r1, r2, r3):
    return (r2 - r1) / (r3 - r2)`,
  tests: [
    {
      name: "r=2 period-1",
      code: `{{FUNC}}\nprint(find_period(2.0, 0.5, 500, 100, 1e-6))`,
      expected: `1\n`,
    },
    {
      name: "r=3.2 period-2",
      code: `{{FUNC}}\nprint(find_period(3.2, 0.5, 500, 100, 1e-6))`,
      expected: `2\n`,
    },
    {
      name: "feigenbaum ratio approximation",
      code: `{{FUNC}}\nratio = feigenbaum_ratio(3.0, 3.449, 3.5441)\nprint(round(ratio, 1))`,
      expected: `4.7\n`,
    },
  ],
};
