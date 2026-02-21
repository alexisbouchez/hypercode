import type { Lesson } from "../../types";

export const compose: Lesson = {
  id: "compose",
  title: "Function Composition",
  chapterId: "functional-foundations",
  content: `# Function Composition

In Functional Differential Geometry, mathematical objects like derivatives, coordinate transformations, and vector fields are all **functions**. The most fundamental operation is **composition**: chaining functions together.

If $f$ and $g$ are functions, their composition is:

$$(f \\circ g)(x) = f(g(x))$$

Apply $g$ first, then $f$.

## Why This Matters

Differential geometry is built on composing transformations. A coordinate change on a manifold is a composition of two coordinate functions. The chain rule — the heart of calculus on manifolds — is a rule for differentiating compositions.

Gerald Jay Sussman and Jack Wisdom's *Functional Differential Geometry* represents geometric objects as functions and operators as higher-order functions. Every concept in this course will build on composition.

## In Python

Python's first-class functions let you write composition directly:

\`\`\`python
def compose(f, g):
    return lambda x: f(g(x))

double = lambda x: 2 * x
inc    = lambda x: x + 1

print(compose(double, inc)(3))   # 2*(3+1) = 8
print(compose(inc, double)(3))   # 2*3 + 1 = 7
\`\`\`

## Your Task

Implement \`compose(f, g)\` that returns the function $f \\circ g$.

Then use it to build a **triple composition** helper that chains three functions: \`compose3(f, g, h)\` returns $f \\circ g \\circ h$.
`,
  starterCode: `def compose(f, g):
    pass

def compose3(f, g, h):
    pass

double = lambda x: 2 * x
square = lambda x: x * x
inc    = lambda x: x + 1

print(compose(double, inc)(3))        # 8
print(compose(inc, double)(3))        # 7
print(compose3(inc, double, square)(3))  # 2*9 + 1 = 19
`,
  solution: `def compose(f, g):
    return lambda x: f(g(x))

def compose3(f, g, h):
    return compose(f, compose(g, h))

double = lambda x: 2 * x
square = lambda x: x * x
inc    = lambda x: x + 1

print(compose(double, inc)(3))
print(compose(inc, double)(3))
print(compose3(inc, double, square)(3))
`,
  tests: [
    {
      name: "compose(square, inc)(3) = 16",
      code: `{{FUNC}}
print(compose(lambda x: x**2, lambda x: x+1)(3))`,
      expected: "16\n",
    },
    {
      name: "compose(inc, square)(3) = 10",
      code: `{{FUNC}}
print(compose(lambda x: x+1, lambda x: x**2)(3))`,
      expected: "10\n",
    },
    {
      name: "compose(double, inc)(5) = 12",
      code: `{{FUNC}}
print(compose(lambda x: 2*x, lambda x: x+1)(5))`,
      expected: "12\n",
    },
    {
      name: "compose3(inc, double, square)(3) = 19",
      code: `{{FUNC}}
print(compose3(lambda x: x+1, lambda x: 2*x, lambda x: x**2)(3))`,
      expected: "19\n",
    },
  ],
};
