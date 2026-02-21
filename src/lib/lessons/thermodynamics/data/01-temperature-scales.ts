import type { Lesson } from "../../types";

export const temperatureScales: Lesson = {
  id: "temperature-scales",
  title: "Temperature Scales",
  chapterId: "laws",
  content: `# Temperature Scales

Temperature is a measure of the average kinetic energy of particles in a substance. Three major scales are used in science and everyday life: Celsius, Fahrenheit, and Kelvin.

## The Kelvin Scale

The Kelvin scale is the SI unit of temperature and is fundamental to thermodynamics. It starts at **absolute zero**, the lowest theoretically possible temperature, where all thermal motion ceases.

$$K = °C + 273.15$$

Absolute zero is defined as:

$$0\\,\\text{K} = -273.15°C = -459.67°F$$

## Celsius and Fahrenheit

The Celsius scale sets 0°C as the freezing point of water and 100°C as its boiling point at standard pressure.

The Fahrenheit scale sets 32°F as freezing and 212°F as boiling.

Converting from Fahrenheit to Celsius:

$$°C = \\frac{(°F - 32) \\times 5}{9}$$

Converting from Celsius to Fahrenheit:

$$°F = °C \\times \\frac{9}{5} + 32$$

## Converting Kelvin to Fahrenheit

Combining the two formulas above:

$$°F = (K - 273.15) \\times \\frac{9}{5} + 32$$

## Key Reference Points

| Temperature | Kelvin | Celsius | Fahrenheit |
|---|---|---|---|
| Absolute zero | 0 K | −273.15°C | −459.67°F |
| Water freezes | 273.15 K | 0°C | 32°F |
| Body temperature | ~310 K | ~37°C | ~98.6°F |
| Water boils | 373.15 K | 100°C | 212°F |

## Your Task

Implement the three conversion functions below.
`,
  starterCode: `def celsius_to_kelvin(c):
    # Return the Kelvin equivalent of c degrees Celsius
    pass

def fahrenheit_to_celsius(f):
    # Return the Celsius equivalent of f degrees Fahrenheit
    pass

def kelvin_to_fahrenheit(k):
    # Return the Fahrenheit equivalent of k Kelvin
    pass
`,
  solution: `def celsius_to_kelvin(c):
    return c + 273.15

def fahrenheit_to_celsius(f):
    return (f - 32) * 5 / 9

def kelvin_to_fahrenheit(k):
    return (k - 273.15) * 9 / 5 + 32
`,
  tests: [
    {
      name: "celsius_to_kelvin(0) = 273.15",
      code: `{{FUNC}}
print(celsius_to_kelvin(0))`,
      expected: "273.15\n",
    },
    {
      name: "celsius_to_kelvin(100) = 373.15",
      code: `{{FUNC}}
print(celsius_to_kelvin(100))`,
      expected: "373.15\n",
    },
    {
      name: "fahrenheit_to_celsius(32) = 0.0",
      code: `{{FUNC}}
print(fahrenheit_to_celsius(32))`,
      expected: "0.0\n",
    },
    {
      name: "kelvin_to_fahrenheit(373.15) = 212.0",
      code: `{{FUNC}}
print(round(kelvin_to_fahrenheit(373.15), 2))`,
      expected: "212.0\n",
    },
  ],
};
