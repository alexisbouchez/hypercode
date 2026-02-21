import type { Lesson } from "../../types";

export const logisticBifurcation: Lesson = {
  id: "logistic-bifurcation",
  title: "Bifurcation Diagram",
  chapterId: "routes-to-chaos",
  content: `## Bifurcation Diagram

The **bifurcation diagram** of the logistic map is one of the most iconic images in chaos theory. It plots the long-term values of x_{n+1} = r · x_n · (1 - x_n) as r sweeps from about 2.4 to 4.

At low r, the orbit converges to a single fixed point. As r increases past 3, the fixed point becomes unstable and a period-2 cycle appears. This period doubles again at r ≈ 3.449 (period 4), then at r ≈ 3.544 (period 8), and so on, with doublings accumulating faster and faster until r ≈ 3.5699, beyond which **chaotic** behavior appears — the orbit fills a continuous interval rather than visiting a finite set of points.

Embedded within the chaotic regime are periodic **windows** — narrow bands of r where the orbit is periodic again before returning to chaos.

**Implement the following functions:**
- \`logistic_attractor(r, x0, n_transient, n_keep)\` — return the n_keep attractor values after discarding n_transient transients
- \`attractor_range(r, x0, n_transient, n_keep)\` — return (min, max) of the attractor values
- \`count_attractor_points(r, x0, n_transient, n_keep, tol)\` — count distinct attractor points (within tolerance tol)`,
  starterCode: `def logistic_attractor(r, x0, n_transient, n_keep):
    # Discard n_transient steps, then collect n_keep values
    pass

def attractor_range(r, x0, n_transient, n_keep):
    # Return (min, max) of attractor values
    pass

def count_attractor_points(r, x0, n_transient, n_keep, tol):
    # Count distinct attractor points within tolerance tol
    pass`,
  solution: `def logistic_attractor(r, x0, n_transient, n_keep):
    x = x0
    for _ in range(n_transient):
        x = r * x * (1.0 - x)
    values = []
    for _ in range(n_keep):
        values.append(round(x, 8))
        x = r * x * (1.0 - x)
    return values

def attractor_range(r, x0, n_transient, n_keep):
    values = logistic_attractor(r, x0, n_transient, n_keep)
    return min(values), max(values)

def count_attractor_points(r, x0, n_transient, n_keep, tol):
    values = logistic_attractor(r, x0, n_transient, n_keep)
    distinct = []
    for v in values:
        found = False
        for d in distinct:
            if abs(v - d) < tol:
                found = True
                break
        if not found:
            distinct.append(v)
    return len(distinct)`,
  tests: [
    {
      name: "r=2.5 fixed point",
      code: `{{FUNC}}\nvals = logistic_attractor(2.5, 0.5, 1000, 10)\nprint(round(vals[0], 4))`,
      expected: `0.6\n`,
    },
    {
      name: "r=3.2 period 2",
      code: `{{FUNC}}\nprint(count_attractor_points(3.2, 0.5, 1000, 50, 1e-5))`,
      expected: `2\n`,
    },
    {
      name: "r=4 chaotic attractor spans wide range",
      code: `{{FUNC}}\nlo,hi = attractor_range(4.0, 0.1, 100, 1000)\nprint(lo < 0.1, hi > 0.9)`,
      expected: `True True\n`,
    },
  ],
};
