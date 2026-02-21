import type { Lesson } from "../../types";

export const pressure: Lesson = {
  id: "pressure",
  title: "Pressure and the Atmosphere",
  chapterId: "fundamentals",
  content: `# Pressure

**Pressure** is the normal force exerted per unit area:

$$P = \\frac{F}{A}$$

where $F$ is force in Newtons and $A$ is area in m². The SI unit of pressure is the **Pascal** (Pa = N/m²).

## Standard Atmosphere

Standard atmospheric pressure at sea level is defined as:

$$P_{\\text{atm}} = 101\\,325\\,\\text{Pa} = 101.325\\,\\text{kPa} \\approx 1\\,\\text{bar}$$

## Absolute vs Gauge Pressure

Most pressure gauges measure **gauge pressure** — pressure relative to the local atmosphere. **Absolute pressure** is the total pressure including the atmosphere:

$$P_{\\text{abs}} = P_{\\text{gauge}} + P_{\\text{atm}}$$

$$P_{\\text{gauge}} = P_{\\text{abs}} - P_{\\text{atm}}$$

A gauge reading of zero means the pressure equals atmospheric — not a vacuum.

### Conversion Examples

| Gauge (Pa) | Absolute (Pa) |
|-----------|--------------|
| 0 | 101 325 |
| 100 000 | 201 325 |
| −50 000 | 51 325 |

## Your Task

Implement:
- \`pressure(F, A)\` — force divided by area (Pa)
- \`gauge_to_absolute(P_gauge)\` — add atmospheric pressure
- \`absolute_to_gauge(P_abs)\` — subtract atmospheric pressure
`,

  starterCode: `def pressure(F, A):
    # Return F / A in Pascals
    pass

def gauge_to_absolute(P_gauge):
    # P_atm = 101325.0 Pa
    pass

def absolute_to_gauge(P_abs):
    # P_atm = 101325.0 Pa
    pass
`,

  solution: `def pressure(F, A):
    return F / A

def gauge_to_absolute(P_gauge):
    P_atm = 101325.0
    return P_gauge + P_atm

def absolute_to_gauge(P_abs):
    P_atm = 101325.0
    return P_abs - P_atm
`,

  tests: [
    {
      name: "pressure(100.0, 0.01) = 10000.0 Pa",
      code: `{{FUNC}}
print(pressure(100.0, 0.01))`,
      expected: "10000.0\n",
    },
    {
      name: "gauge_to_absolute(0) = 101325.0 Pa (zero gauge = 1 atm)",
      code: `{{FUNC}}
print(gauge_to_absolute(0))`,
      expected: "101325.0\n",
    },
    {
      name: "gauge_to_absolute(100000) = 201325.0 Pa",
      code: `{{FUNC}}
print(gauge_to_absolute(100000))`,
      expected: "201325.0\n",
    },
    {
      name: "absolute_to_gauge(101325.0) = 0.0 Pa",
      code: `{{FUNC}}
print(absolute_to_gauge(101325.0))`,
      expected: "0.0\n",
    },
  ],
};
