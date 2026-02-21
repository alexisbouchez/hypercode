import type { Lesson } from "../../types";

export const fractals: Lesson = {
  id: "fractals",
  title: "Fractals and Hausdorff Dimension",
  chapterId: "emergence",
  content: `# Fractals and Hausdorff Dimension

Fractals are geometric objects that exhibit self-similarity at every scale. Unlike smooth curves (dimension 1) or filled surfaces (dimension 2), fractals can have non-integer dimensions — the **Hausdorff dimension**.

## Box-Counting Dimension

The box-counting (Hausdorff) dimension is defined as:

$$D = -\\lim_{\\varepsilon \\to 0} \\frac{\\log N(\\varepsilon)}{\\log \\varepsilon}$$

where $N(\\varepsilon)$ is the number of boxes of side length $\\varepsilon$ needed to cover the fractal.

## Classic Fractal Dimensions

| Fractal | Dimension | Formula |
|---------|-----------|---------|
| Cantor Set | ≈ 0.6309 | $\\log(2) / \\log(3)$ |
| Koch Curve | ≈ 1.2619 | $\\log(4) / \\log(3)$ |
| Sierpiński Triangle | ≈ 1.5850 | $\\log(3) / \\log(2)$ |

### Cantor Set
Start with [0,1]. At each iteration, remove the middle third from every interval. After $n$ iterations: $2^n$ intervals of length $(1/3)^n$ remain. Self-similarity ratio $r = 1/3$, copies $N = 2$, so $D = \\log(2)/\\log(3)$.

### Koch Curve
Start with a line segment. Replace each segment with four segments of length $1/3$. Self-similarity: $N=4$ copies at ratio $r=1/3$, so $D = \\log(4)/\\log(3)$.

### Sierpiński Triangle
Start with a filled triangle. Remove the central triangle at each step, leaving 3 copies at half the scale. $D = \\log(3)/\\log(2)$.

## Box-Counting in Practice

Given a set of points, estimate $D$ by counting how many grid boxes of size $\\varepsilon$ contain at least one point. Plot $\\log N$ vs $\\log(1/\\varepsilon)$; the slope is $D$.

## Implementation

Implement the following functions:

\`\`\`python
import math

def hausdorff_dim_cantor():
    # Return log(2)/log(3)
    ...

def hausdorff_dim_koch():
    # Return log(4)/log(3)
    ...

def hausdorff_dim_sierpinski():
    # Return log(3)/log(2)
    ...

def cantor_set_count(n_iterations):
    # Return number of intervals after n iterations = 2^n
    ...

def box_count_estimate(points_x, epsilon):
    # Count distinct grid boxes of size epsilon containing points
    ...
\`\`\`
`,
  starterCode: `import math

def hausdorff_dim_cantor():
    pass

def hausdorff_dim_koch():
    pass

def hausdorff_dim_sierpinski():
    pass

def cantor_set_count(n_iterations):
    pass

def box_count_estimate(points_x, epsilon):
    pass
`,
  solution: `import math

def hausdorff_dim_cantor():
    return math.log(2) / math.log(3)

def hausdorff_dim_koch():
    return math.log(4) / math.log(3)

def hausdorff_dim_sierpinski():
    return math.log(3) / math.log(2)

def cantor_set_count(n_iterations):
    return 2 ** n_iterations

def box_count_estimate(points_x, epsilon):
    boxes = set()
    for x in points_x:
        box = int(x / epsilon)
        boxes.add(box)
    return len(boxes)
`,
  tests: [
    {
      name: "Hausdorff dimension of Cantor set",
      expected: "0.6309\n",
      code: `{{FUNC}}\nprint(f"{hausdorff_dim_cantor():.4f}")`,
    },
    {
      name: "Hausdorff dimension of Koch curve",
      expected: "1.2619\n",
      code: `{{FUNC}}\nprint(f"{hausdorff_dim_koch():.4f}")`,
    },
    {
      name: "Hausdorff dimension of Sierpinski triangle",
      expected: "1.5850\n",
      code: `{{FUNC}}\nprint(f"{hausdorff_dim_sierpinski():.4f}")`,
    },
    {
      name: "Cantor set interval count at 5 iterations",
      expected: "32\n",
      code: `{{FUNC}}\nprint(cantor_set_count(5))`,
    },
  ],
};
