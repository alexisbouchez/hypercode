import type { Lesson } from "../../types";

export const juliaSets: Lesson = {
  id: "julia-sets",
  title: "Julia Sets",
  chapterId: "fractal-geometry",
  content: `## Julia Sets

For every complex constant **c**, the **Julia set** J_c is the boundary between starting points z that escape to infinity and those that remain bounded under repeated application of f(z) = z² + c.

Unlike the Mandelbrot set — where c varies and z starts at 0 — for a Julia set we **fix c** and vary the starting point z. This gives a different fractal for every value of c.

A key theorem connects Julia sets to the Mandelbrot set:
- If **c is inside** the Mandelbrot set, then J_c is **connected** (one piece)
- If **c is outside** the Mandelbrot set, then J_c is a **Cantor dust** (infinitely disconnected)

The **filled Julia set** K_c consists of all z that do not escape — the interior plus the boundary. Computing the fraction of a grid that lies in K_c gives a measure of the "filled" area.

**Implement the following functions:**
- \`julia_iter(zr, zi, cr, ci, max_iter)\` — return the escape time for starting point (zr, zi) with constant c = (cr, ci)
- \`julia_connected(cr, ci, max_iter)\` — return \`True\` if the Julia set is connected (c is in the Mandelbrot set)
- \`julia_fill_ratio(cr, ci, zr_min, zr_max, zi_min, zi_max, n, max_iter)\` — fraction of n×n grid points that do not escape`,
  starterCode: `def julia_iter(zr, zi, cr, ci, max_iter):
    # Returns escape time for starting point (zr, zi) with constant c=(cr, ci)
    pass

def julia_connected(cr, ci, max_iter):
    # Julia set is connected iff 0 does not escape under z -> z^2 + c
    pass

def julia_fill_ratio(cr, ci, zr_min, zr_max, zi_min, zi_max, n, max_iter):
    # Fraction of n x n grid points that do NOT escape (filled Julia set)
    pass`,
  solution: `def julia_iter(zr, zi, cr, ci, max_iter):
    for i in range(max_iter):
        zr2, zi2 = zr*zr, zi*zi
        if zr2 + zi2 > 4.0:
            return i
        zi = 2.0 * zr * zi + ci
        zr = zr2 - zi2 + cr
    return max_iter

def julia_connected(cr, ci, max_iter):
    return julia_iter(0.0, 0.0, cr, ci, max_iter) == max_iter

def julia_fill_ratio(cr, ci, zr_min, zr_max, zi_min, zi_max, n, max_iter):
    count = 0
    total = n * n
    for i in range(n):
        zi_val = zi_min + (zi_max - zi_min) * i / (n - 1)
        for j in range(n):
            zr_val = zr_min + (zr_max - zr_min) * j / (n - 1)
            if julia_iter(zr_val, zi_val, cr, ci, max_iter) == max_iter:
                count += 1
    return count / total`,
  tests: [
    {
      name: "c=0 unit disk stays bounded",
      code: `{{FUNC}}\nprint(julia_iter(0.5, 0.0, 0.0, 0.0, 100))`,
      expected: `100\n`,
    },
    {
      name: "c=0 outside escapes",
      code: `{{FUNC}}\nprint(julia_iter(2.0, 0.0, 0.0, 0.0, 100))`,
      expected: `1\n`,
    },
    {
      name: "c=-1 julia is connected",
      code: `{{FUNC}}\nprint(julia_connected(-1.0, 0.0, 100))`,
      expected: `True\n`,
    },
  ],
};
