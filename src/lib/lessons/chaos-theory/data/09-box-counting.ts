import type { Lesson } from "../../types";

export const boxCounting: Lesson = {
  id: "box-counting",
  title: "Box-Counting Dimension",
  chapterId: "fractal-geometry",
  content: `## Box-Counting Dimension

The **box-counting dimension** (also called Minkowski dimension) is the most practical way to measure the fractal dimension of a set. It quantifies how detail changes with scale.

### Definition

Cover the set with boxes (intervals) of size ε. Let N(ε) be the minimum number of boxes needed. The box-counting dimension is:

\`\`\`
D = lim_{ε→0} log(N(ε)) / log(1/ε)
\`\`\`

### Examples

| Shape | Dimension |
|-------|-----------|
| Point | 0 |
| Line segment | 1 |
| Filled square | 2 |
| Cantor set | log(2)/log(3) ≈ 0.631 |
| Sierpiński triangle | log(3)/log(2) ≈ 1.585 |

### The Cantor Set

The **middle-thirds Cantor set** is constructed by repeatedly removing the middle third of each interval:
- Start: [0, 1]
- Step 1: [0, 1/3] ∪ [2/3, 1]
- Step 2: [0, 1/9] ∪ [2/9, 1/3] ∪ [2/3, 7/9] ∪ [8/9, 1]
- ...

After n steps, there are 2ⁿ intervals each of length 3⁻ⁿ.

### Computing Box-Counting Dimension

In practice, we estimate D as the slope of log N(ε) vs log(1/ε) using linear regression on several values of ε.

### Your Task

Implement:
1. \`cantor_set(n)\` — return list of (a, b) intervals after n iterations
2. \`box_count_cantor(n, epsilon)\` — count boxes of size epsilon covering the Cantor set
3. \`box_counting_dimension(counts, epsilons)\` — estimate dimension via linear regression`,
  starterCode: `import math

def cantor_set(n):
    # Start with [(0.0, 1.0)]
    # Each step: replace each (a,b) with (a, a+third) and (b-third, b)
    # where third = (b-a)/3
    pass

def box_count_cantor(n, epsilon):
    # Get cantor_set(n) intervals
    # Count distinct box indices of size epsilon that overlap any interval
    pass

def box_counting_dimension(counts, epsilons):
    # counts[i] = N(epsilons[i])
    # Fit slope of log(N) vs log(1/epsilon) using least squares
    pass`,
  solution: `import math

def cantor_set(n):
    intervals = [(0.0, 1.0)]
    for _ in range(n):
        new_intervals = []
        for (a, b) in intervals:
            third = (b - a) / 3.0
            new_intervals.append((a, a + third))
            new_intervals.append((b - third, b))
        intervals = new_intervals
    return intervals

def box_count_cantor(n, epsilon):
    intervals = cantor_set(n)
    boxes = set()
    for (a, b) in intervals:
        start = int(a / epsilon)
        end = int((b - 1e-12) / epsilon)
        for i in range(start, end + 1):
            boxes.add(i)
    return len(boxes)

def box_counting_dimension(counts, epsilons):
    log_inv_eps = [math.log(1.0 / e) for e in epsilons]
    log_n = [math.log(c) for c in counts]
    n = len(counts)
    mean_x = sum(log_inv_eps) / n
    mean_y = sum(log_n) / n
    num = sum((log_inv_eps[i] - mean_x) * (log_n[i] - mean_y) for i in range(n))
    den = sum((log_inv_eps[i] - mean_x) ** 2 for i in range(n))
    return num / den`,
  tests: [
    {
      name: "cantor set iteration 0",
      code: `{{FUNC}}\nprint(cantor_set(0))`,
      expected: "[(0.0, 1.0)]\n",
    },
    {
      name: "cantor set iteration 1",
      code: `{{FUNC}}\nresult = cantor_set(1)\nprint([(round(a,4),round(b,4)) for a,b in result])`,
      expected: "[(0.0, 0.3333), (0.6667, 1.0)]\n",
    },
    {
      name: "cantor dimension",
      code: `{{FUNC}}\ncounts = [box_count_cantor(8, 3**(-k)) for k in range(1,5)]\nepsilons = [3**(-k) for k in range(1,5)]\nd = box_counting_dimension(counts, epsilons)\nprint(round(d,2))`,
      expected: "0.63\n",
    },
  ],
};
