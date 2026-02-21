import type { Lesson } from "../../types";

export const moments: Lesson = {
  id: "moments",
  title: "Mean, Variance, Skewness, Kurtosis",
  chapterId: "return-analysis",
  content: `## Statistical Moments

The **moments** of a distribution describe its shape. The first four moments are the foundation of quantitative analysis:

**Mean** — the average (first moment):
$$\\mu = \\frac{1}{n} \\sum_{i=1}^{n} x_i$$

**Variance** — spread around the mean (second moment, sample):
$$s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\mu)^2$$

**Skewness** — asymmetry (third standardized moment):
$$\\text{skew} = \\frac{\\frac{1}{n} \\sum (x_i - \\mu)^3}{\\sigma^3}$$

where $\\sigma$ is the **population** standard deviation.

**Excess Kurtosis** — tail heaviness (fourth standardized moment minus 3):
$$\\text{kurt} = \\frac{\\frac{1}{n} \\sum (x_i - \\mu)^4}{\\sigma^4} - 3$$

A normal distribution has skewness = 0 and excess kurtosis = 0.

### Your Task

Implement all four moment functions. Use **sample variance** (divide by n-1) but **population std** (divide by n) for skewness and kurtosis normalization.`,
  starterCode: `import math

def mean(xs):
    # sum(xs) / len(xs)
    pass

def variance(xs):
    # Sample variance: sum((x - mean)^2) / (n - 1)
    pass

def skewness(xs):
    # (1/n) * sum((x - mean)^3) / population_std^3
    pass

def kurtosis(xs):
    # (1/n) * sum((x - mean)^4) / population_std^4 - 3
    pass`,
  solution: `import math

def mean(xs):
    return sum(xs) / len(xs)

def variance(xs):
    n = len(xs)
    m = mean(xs)
    return sum((x - m) ** 2 for x in xs) / (n - 1)

def skewness(xs):
    n = len(xs)
    m = mean(xs)
    pop_std = math.sqrt(sum((x - m) ** 2 for x in xs) / n)
    return (1 / n) * sum((x - m) ** 3 for x in xs) / pop_std ** 3

def kurtosis(xs):
    n = len(xs)
    m = mean(xs)
    pop_std = math.sqrt(sum((x - m) ** 2 for x in xs) / n)
    return (1 / n) * sum((x - m) ** 4 for x in xs) / pop_std ** 4 - 3`,
  tests: [
    {
      name: "mean([1..10]) equals 5.5",
      code: `{{FUNC}}
print(round(mean([1,2,3,4,5,6,7,8,9,10]), 4))`,
      expected: "5.5\n",
    },
    {
      name: "variance([1..10]) equals 9.1667",
      code: `{{FUNC}}
print(round(variance([1,2,3,4,5,6,7,8,9,10]), 4))`,
      expected: "9.1667\n",
    },
    {
      name: "skewness([1..10]) equals 0.0 (symmetric)",
      code: `{{FUNC}}
print(round(skewness([1,2,3,4,5,6,7,8,9,10]), 4))`,
      expected: "0.0\n",
    },
    {
      name: "kurtosis([1..10]) equals -1.2242 (platykurtic)",
      code: `{{FUNC}}
print(round(kurtosis([1,2,3,4,5,6,7,8,9,10]), 4))`,
      expected: "-1.2242\n",
    },
  ],
};
