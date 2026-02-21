import type { Lesson } from "../../types";

export const machNumber: Lesson = {
  id: "mach-number",
  title: "Mach Number and Compressible Flow",
  chapterId: "dynamics",
  content: `# Mach Number and Compressible Flow

At high speeds, compressibility effects become important. The **Mach number** characterises how fast a flow moves relative to the local speed of sound.

## Speed of Sound

In an ideal gas, the speed of sound depends only on temperature:

$$c = \\sqrt{\\gamma R T}$$

where:
- $\\gamma = 1.4$ — ratio of specific heats for air (diatomic ideal gas)
- $R = 287\\,\\text{J/(kg·K)}$ — specific gas constant for air
- $T$ — absolute temperature in kelvin (K)

At 20 °C (293.15 K), $c \\approx 343$ m/s. Sound travels faster in warmer air because higher temperature means faster molecular motion.

## Mach Number

The **Mach number** $M$ is the ratio of flow speed $v$ to the local speed of sound $c$:

$$M = \\frac{v}{c}$$

| Regime | Mach Range |
|--------|-----------|
| Subsonic | $M < 1$ |
| Transonic | $M \\approx 1$ |
| Supersonic | $M > 1$ |
| Hypersonic | $M > 5$ |

## Mach Cone

When an object moves at supersonic speed ($M > 1$), it outruns the pressure waves it creates. These waves pile up into a **Mach cone** (shock wave). The half-angle $\\alpha$ of the cone satisfies:

$$\\sin \\alpha = \\frac{1}{M} \\quad (M > 1)$$

$$\\alpha = \\arcsin\\!\\left(\\frac{1}{M}\\right)$$

At $M = 2$, $\\alpha = 30°$. Higher Mach numbers produce narrower cones.

## Your Task

Implement:

- \`speed_of_sound(T)\` — speed of sound in m/s for air at temperature $T$ (K). Use $\\gamma = 1.4$, $R = 287$ J/(kg·K).
- \`mach_number(v, T)\` — Mach number for flow speed $v$ (m/s) at temperature $T$ (K).
- \`mach_angle_deg(M)\` — Mach cone half-angle in degrees for $M > 1$; return \`None\` if $M \\leq 1$.
`,

  starterCode: `import math

def speed_of_sound(T):
    # gamma = 1.4, R = 287 J/(kg·K)
    # c = sqrt(gamma * R * T)
    pass

def mach_number(v, T):
    # M = v / speed_of_sound(T)
    pass

def mach_angle_deg(M):
    # sin(alpha) = 1/M for M > 1; return None if M <= 1
    pass
`,

  solution: `import math

def speed_of_sound(T):
    gamma = 1.4
    R = 287.0
    return math.sqrt(gamma * R * T)

def mach_number(v, T):
    return v / speed_of_sound(T)

def mach_angle_deg(M):
    if M <= 1:
        return None
    return math.degrees(math.asin(1.0 / M))
`,

  tests: [
    {
      name: "speed_of_sound(293.15) ≈ 343.2021 m/s (20 °C)",
      code: `{{FUNC}}
print(round(speed_of_sound(293.15), 4))`,
      expected: "343.2021\n",
    },
    {
      name: "speed_of_sound(273.15) ≈ 331.2879 m/s (0 °C)",
      code: `{{FUNC}}
print(round(speed_of_sound(273.15), 4))`,
      expected: "331.2879\n",
    },
    {
      name: "mach_number(686.0, 293.15) ≈ 1.9988 (roughly Mach 2 at 20 °C)",
      code: `{{FUNC}}
print(round(mach_number(686.0, 293.15), 4))`,
      expected: "1.9988\n",
    },
    {
      name: "mach_number(100.0, 300.0) ≈ 0.288 (subsonic)",
      code: `{{FUNC}}
print(round(mach_number(100.0, 300.0), 4))`,
      expected: "0.288\n",
    },
    {
      name: "mach_angle_deg(2.0) = 30.0° (Mach 2 cone half-angle)",
      code: `{{FUNC}}
print(round(mach_angle_deg(2.0), 4))`,
      expected: "30.0\n",
    },
    {
      name: "mach_angle_deg(1.5) ≈ 41.8103°",
      code: `{{FUNC}}
print(round(mach_angle_deg(1.5), 4))`,
      expected: "41.8103\n",
    },
    {
      name: "mach_angle_deg(3.0) ≈ 19.4712°",
      code: `{{FUNC}}
print(round(mach_angle_deg(3.0), 4))`,
      expected: "19.4712\n",
    },
    {
      name: "mach_angle_deg(0.8) returns None (subsonic — no Mach cone)",
      code: `{{FUNC}}
print(mach_angle_deg(0.8))`,
      expected: "None\n",
    },
  ],
};
