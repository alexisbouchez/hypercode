import type { Lesson } from "../../types";

export const poincareMap: Lesson = {
  id: "poincare-map",
  title: "Poincaré Maps",
  chapterId: "lyapunov",
  content: `## Poincaré Maps

A **Poincaré map** (or return map) reduces the study of a continuous dynamical system to a discrete one by recording where a trajectory crosses a chosen surface of section. For a 1D discrete map like the logistic map, the Poincaré map is simply the orbit itself — plotting (xₙ, xₙ₊₁) reveals the structure of the attractor.

After discarding a **transient** (warm-up iterations to reach the attractor), the remaining orbit reveals the system's asymptotic behaviour. A **period-1** fixed point produces a single value; a **period-4** orbit cycles through exactly 4 distinct values; a chaotic orbit never repeats.

Return maps are central to bifurcation diagrams: sweeping over the parameter r and plotting the attractor values produces the famous logistic map bifurcation diagram.

**Implement the following functions:**
- \`logistic_orbit(r, x0, n_transient, n_keep)\` — discard \`n_transient\` transients, then collect \`n_keep\` orbit values (each rounded to 8 decimal places)
- \`orbit_period(orbit, tol=1e-6)\` — detect the number of distinct values in the orbit (the period)
- \`return_map_pairs(xs)\` — return a list of consecutive pairs [(x0,x1), (x1,x2), …]`,
  starterCode: `def logistic_orbit(r, x0, n_transient, n_keep):
    # Burn n_transient steps, then collect n_keep values
    pass

def orbit_period(orbit, tol=1e-6):
    # Count distinct values (within tolerance tol)
    pass

def return_map_pairs(xs):
    # Return list of (xs[i], xs[i+1]) tuples
    pass`,
  solution: `def logistic_orbit(r, x0, n_transient, n_keep):
    x = x0
    for _ in range(n_transient):
        x = r * x * (1.0 - x)
    orbit = []
    for _ in range(n_keep):
        orbit.append(round(x, 8))
        x = r * x * (1.0 - x)
    return orbit

def orbit_period(orbit, tol=1e-6):
    distinct = []
    for v in orbit:
        new = True
        for d in distinct:
            if abs(v - d) < tol:
                new = False
                break
        if new:
            distinct.append(v)
    return len(distinct)

def return_map_pairs(xs):
    return [(xs[i], xs[i+1]) for i in range(len(xs)-1)]`,
  tests: [
    {
      name: "r=2 period-1 fixed point",
      code: `{{FUNC}}\norbit = logistic_orbit(2.0, 0.1, 1000, 10)\nprint(orbit_period(orbit))`,
      expected: "1\n",
    },
    {
      name: "r=3.5 period-4 orbit",
      code: `{{FUNC}}\norbit = logistic_orbit(3.5, 0.1, 1000, 20)\nprint(orbit_period(orbit))`,
      expected: "4\n",
    },
    {
      name: "return_map_pairs",
      code: `{{FUNC}}\nprint(return_map_pairs([1.0, 2.0, 3.0]))`,
      expected: "[(1.0, 2.0), (2.0, 3.0)]\n",
    },
  ],
};
