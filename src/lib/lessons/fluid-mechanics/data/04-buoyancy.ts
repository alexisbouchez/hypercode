import type { Lesson } from "../../types";

export const buoyancy: Lesson = {
  id: "buoyancy",
  title: "Buoyancy and Archimedes' Principle",
  chapterId: "statics",
  content: `# Buoyancy and Archimedes' Principle

**Archimedes' Principle** states that any object fully or partially submerged in a fluid experiences an upward buoyant force equal to the weight of the fluid it displaces:

$$F_b = \\rho_{\\text{fluid}} \\cdot g \\cdot V_{\\text{submerged}}$$

where $g = 9.81\\,\\text{m/s}^2$.

## Floating and Sinking

An object **floats** when the buoyant force equals or exceeds its weight ($F_b \\geq mg$). For a floating object in equilibrium, the fraction of its volume that is submerged equals the ratio of its density to the fluid density:

$$\\frac{V_{\\text{sub}}}{V_{\\text{total}}} = \\frac{\\rho_{\\text{object}}}{\\rho_{\\text{fluid}}}$$

If this ratio exceeds 1, the object sinks (it cannot displace enough fluid to support its own weight).

## Net Force

The **net vertical force** on a fully submerged object (positive = upward) acting downward is:

$$F_{\\text{net}} = W - F_b = mg - \\rho_{\\text{fluid}} \\cdot g \\cdot V_{\\text{total}}$$

A negative net force means the buoyant force exceeds gravity — the object will rise or float.

### Examples

| Object | $\\rho_{\\text{obj}}$ | $\\rho_{\\text{fluid}}$ | Submerged fraction |
|--------|---------|---------|---------|
| Wood | 800 kg/m³ | 1000 (water) | 0.80 |
| Ice | 917 kg/m³ | 1000 (water) | 0.917 |
| Mercury | 13 546 kg/m³ | 1000 (water) | 13.546 (sinks) |

## Your Task

Implement:
- \`buoyancy_force(rho_fluid, V_submerged)\` — upward buoyant force (N)
- \`submerged_fraction(rho_object, rho_fluid)\` — fraction of volume submerged
- \`net_force(mass, rho_fluid, V_total)\` — net downward force on a fully submerged object (N)
`,

  starterCode: `def buoyancy_force(rho_fluid, V_submerged):
    # g = 9.81; return rho_fluid * g * V_submerged
    pass

def submerged_fraction(rho_object, rho_fluid):
    # return rho_object / rho_fluid
    pass

def net_force(mass, rho_fluid, V_total):
    # g = 9.81; return weight - buoyancy
    pass
`,

  solution: `def buoyancy_force(rho_fluid, V_submerged):
    g = 9.81
    return rho_fluid * g * V_submerged

def submerged_fraction(rho_object, rho_fluid):
    return rho_object / rho_fluid

def net_force(mass, rho_fluid, V_total):
    g = 9.81
    weight = mass * g
    buoyancy = rho_fluid * g * V_total
    return weight - buoyancy
`,

  tests: [
    {
      name: "buoyancy_force(1000.0, 0.001) = 9.81 N (1 litre of water displaced)",
      code: `{{FUNC}}
print(buoyancy_force(1000.0, 0.001))`,
      expected: "9.81\n",
    },
    {
      name: "submerged_fraction(800.0, 1000.0) = 0.8 (wood floats 80% submerged)",
      code: `{{FUNC}}
print(submerged_fraction(800.0, 1000.0))`,
      expected: "0.8\n",
    },
    {
      name: "submerged_fraction(13546.0, 1000.0) = 13.546 (mercury sinks in water)",
      code: `{{FUNC}}
print(submerged_fraction(13546.0, 1000.0))`,
      expected: "13.546\n",
    },
    {
      name: "net_force(0.5, 1000.0, 0.001) = -4.905 N (buoyancy exceeds weight)",
      code: `{{FUNC}}
print(round(net_force(0.5, 1000.0, 0.001), 4))`,
      expected: "-4.905\n",
    },
  ],
};
