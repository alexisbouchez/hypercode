import type { Lesson } from "../../types";

export const cellularAutomata: Lesson = {
  id: "cellular-automata",
  title: "Cellular Automata",
  chapterId: "emergence",
  content: `# Cellular Automata

**Elementary cellular automata** (ECAs) are the simplest class of systems that exhibit emergence: local, deterministic rules give rise to globally complex patterns.

## Setup

A 1D ECA consists of a row of cells, each either \`0\` or \`1\`. At each time step, every cell's new state is determined by looking at a **neighbourhood** of three cells: the cell itself and its two immediate neighbours.

With three binary cells, there are $2^3 = 8$ possible neighbourhood patterns. Mapping each pattern to a 0 or 1 output requires $2^8 = 256$ bits — so there are exactly **256 distinct rules**, numbered 0–255 (Wolfram's numbering scheme).

## Rule Encoding

For a neighbourhood $(l, c, r)$ — left, centre, right — the index into the rule is:

$$\\text{index} = 4l + 2c + r$$

and the output bit is:

$$\\text{output} = \\left(\\text{rule\\_number} \\gg \\text{index}\\right) \\mathbin{\\&} 1$$

For example, Rule 110 in binary is \`01101110\`:

| Pattern | 111 | 110 | 101 | 100 | 011 | 010 | 001 | 000 |
|---------|-----|-----|-----|-----|-----|-----|-----|-----|
| Output  |  0  |  1  |  1  |  0  |  1  |  1  |  1  |  0  |

## Notable Rules

- **Rule 110**: Turing complete — capable of universal computation.
- **Rule 30**: Chaotic — used by Mathematica's \`Random[]\` function for years.
- **Rule 90**: Produces the Sierpiński triangle fractal.

## Boundary Conditions

We use **periodic boundary conditions**: the leftmost and rightmost cells are treated as neighbours of each other. This avoids edge effects.

## Your Task

Implement three functions:
- \`rule_lookup(rule_number)\` — return a dict mapping each \`(l, c, r)\` tuple to its output bit
- \`apply_rule(state, rule_number)\` — apply one step of the CA, returning the new state list (same length, periodic BC)
- \`run_ca(initial_state, rule_number, n_steps)\` — return a list of \`n_steps + 1\` states (including the initial state)
`,
  starterCode: `def rule_lookup(rule_number):
    pass

def apply_rule(state, rule_number):
    pass

def run_ca(initial_state, rule_number, n_steps):
    pass
`,
  solution: `def rule_lookup(rule_number):
    lookup = {}
    for i in range(8):
        l = (i >> 2) & 1
        c = (i >> 1) & 1
        r = i & 1
        lookup[(l, c, r)] = (rule_number >> i) & 1
    return lookup

def apply_rule(state, rule_number):
    lookup = rule_lookup(rule_number)
    N = len(state)
    new_state = []
    for i in range(N):
        l = state[(i - 1) % N]
        c = state[i]
        r = state[(i + 1) % N]
        new_state.append(lookup[(l, c, r)])
    return new_state

def run_ca(initial_state, rule_number, n_steps):
    states = [initial_state[:]]
    current = initial_state[:]
    for _ in range(n_steps):
        current = apply_rule(current, rule_number)
        states.append(current[:])
    return states
`,
  tests: [
    {
      name: "apply_rule([0,0,0,1,0,0,0], 110)[3] equals 1",
      expected: "1\n",
      code: `{{FUNC}}
print(apply_rule([0, 0, 0, 1, 0, 0, 0], 110)[3])`,
    },
    {
      name: "apply_rule([0,0,0,1,0,0,0], 110)[2] equals 1",
      expected: "1\n",
      code: `{{FUNC}}
print(apply_rule([0, 0, 0, 1, 0, 0, 0], 110)[2])`,
    },
    {
      name: "run_ca([0,0,0,1,0,0,0], 30, 3)[2][3] equals 0",
      expected: "0\n",
      code: `{{FUNC}}
print(run_ca([0, 0, 0, 1, 0, 0, 0], 30, 3)[2][3])`,
    },
    {
      name: "sum of apply_rule([1,0,1,0,1,0,1], 110) equals 7",
      expected: "7\n",
      code: `{{FUNC}}
print(sum(apply_rule([1, 0, 1, 0, 1, 0, 1], 110)))`,
    },
  ],
};
