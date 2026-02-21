import type { Lesson } from "../../types";

export const mandelbrot: Lesson = {
  id: "mandelbrot",
  title: "Mandelbrot Set",
  chapterId: "fractal-geometry",
  content: `## Mandelbrot Set

The **Mandelbrot set** is one of the most famous fractals in mathematics. A complex number c belongs to the set if the sequence defined by:

$$z_0 = 0, \\quad z_{n+1} = z_n^2 + c$$

remains **bounded** (never escapes to infinity). In practice, we check whether |z| ever exceeds 2 — if it does, the sequence will diverge to infinity.

The **escape time algorithm** counts how many iterations it takes for |z| to exceed 2. Points inside the set never escape; points outside escape after some number of steps. The number of iterations before escape gives information about how "close to the boundary" a point is, and is used to color the famous fractal images.

**Implement the following functions:**
- \`mandelbrot_iter(cr, ci, max_iter)\` — return the number of iterations before |z|² > 4, or \`max_iter\` if the point is in the set. The complex number c = cr + ci·i, z starts at 0.
- \`in_mandelbrot(cr, ci, max_iter)\` — return \`True\` if the point is in the Mandelbrot set (did not escape within \`max_iter\` iterations)
- \`mandelbrot_grid(cr_min, cr_max, ci_min, ci_max, nx, ny, max_iter)\` — count how many grid points in the given rectangle are in the Mandelbrot set`,
  starterCode: `def mandelbrot_iter(cr, ci, max_iter):
    # Returns number of iterations before |z|^2 > 4, or max_iter if in set
    # z starts at (0, 0); update: zi = 2*zr*zi + ci, zr = zr^2 - zi^2 + cr
    pass

def in_mandelbrot(cr, ci, max_iter):
    # True if the point did not escape within max_iter iterations
    pass

def mandelbrot_grid(cr_min, cr_max, ci_min, ci_max, nx, ny, max_iter):
    # Count how many evenly-spaced grid points are in the Mandelbrot set
    pass`,
  solution: `def mandelbrot_iter(cr, ci, max_iter):
    zr, zi = 0.0, 0.0
    for i in range(max_iter):
        zr2, zi2 = zr*zr, zi*zi
        if zr2 + zi2 > 4.0:
            return i
        zi = 2.0 * zr * zi + ci
        zr = zr2 - zi2 + cr
    return max_iter

def in_mandelbrot(cr, ci, max_iter):
    return mandelbrot_iter(cr, ci, max_iter) == max_iter

def mandelbrot_grid(cr_min, cr_max, ci_min, ci_max, nx, ny, max_iter):
    count = 0
    for i in range(ny):
        ci_val = ci_min + (ci_max - ci_min) * i / (ny - 1)
        for j in range(nx):
            cr_val = cr_min + (cr_max - cr_min) * j / (nx - 1)
            if in_mandelbrot(cr_val, ci_val, max_iter):
                count += 1
    return count`,
  tests: [
    {
      name: "origin is in set",
      code: `{{FUNC}}\nprint(in_mandelbrot(0.0, 0.0, 100))`,
      expected: `True\n`,
    },
    {
      name: "far point escapes quickly",
      code: `{{FUNC}}\nprint(mandelbrot_iter(2.0, 2.0, 100))`,
      expected: `1\n`,
    },
    {
      name: "c=-1 is in set",
      code: `{{FUNC}}\nprint(in_mandelbrot(-1.0, 0.0, 100))`,
      expected: `True\n`,
    },
  ],
};
