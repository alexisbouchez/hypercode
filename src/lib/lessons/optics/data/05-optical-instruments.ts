import type { Lesson } from "../../types";

export const opticalInstruments: Lesson = {
  id: "optical-instruments",
  title: "Optical Instruments",
  chapterId: "geometric",
  content: `# Optical Instruments

Optical instruments use combinations of lenses and mirrors to extend the capabilities of the human eye. The two most important compound optical instruments are the **microscope** and the **telescope**.

## The Compound Microscope

A compound microscope uses two converging lenses:
- **Objective lens**: short focal length $f_{\\text{obj}}$, creates a magnified real image of the specimen
- **Eyepiece lens**: acts as a magnifying glass, enlarges the intermediate image

The total magnification is:

$$M_{\\text{microscope}} = \\frac{L}{f_{\\text{obj}}} \\times \\frac{D}{f_{\\text{eye}}}$$

Where:
- $L$ is the **tube length** — the distance between the objective's back focal point and the eyepiece's front focal point (typically 160 mm)
- $D$ is the **near-point distance** — the closest comfortable viewing distance for the human eye (typically 250 mm)
- $f_{\\text{obj}}$ and $f_{\\text{eye}}$ are the focal lengths of the objective and eyepiece

**Example:** Objective $f_{\\text{obj}} = 4\\,\\text{mm}$, eyepiece $f_{\\text{eye}} = 25\\,\\text{mm}$, $L = 160\\,\\text{mm}$, $D = 250\\,\\text{mm}$:

$$M = \\frac{160}{4} \\times \\frac{250}{25} = 40 \\times 10 = 400$$

## The Refracting Telescope

A refracting telescope also uses two lenses, but designed for distant objects:

$$M_{\\text{telescope}} = -\\frac{f_{\\text{obj}}}{f_{\\text{eye}}}$$

The negative sign indicates an inverted image (astronomical telescopes give an inverted view). The **angular magnification** magnitude is:

$$|M| = \\frac{f_{\\text{obj}}}{f_{\\text{eye}}}$$

A large objective focal length and short eyepiece focal length gives high magnification. Telescopes also gather more light with larger objective diameters.

## Your Task

Implement the magnification formulas for both instruments. For the microscope, use a default near-point distance of 250 mm.
`,
  starterCode: `def microscope_magnification(f_obj, f_eye, L, near_point=250):
    # Return total magnification of compound microscope
    # f_obj, f_eye, L, near_point all in the same units (e.g. mm)
    pass

def telescope_magnification(f_obj, f_eye):
    # Return the angular magnification magnitude of a refracting telescope
    pass
`,
  solution: `def microscope_magnification(f_obj, f_eye, L, near_point=250):
    return (L / f_obj) * (near_point / f_eye)

def telescope_magnification(f_obj, f_eye):
    return f_obj / f_eye
`,
  tests: [
    {
      name: "microscope_magnification(4, 25, 160) = 400x",
      code: `{{FUNC}}
print(round(microscope_magnification(4, 25, 160), 4))`,
      expected: "400.0\n",
    },
    {
      name: "telescope_magnification(1000, 25) = 40x",
      code: `{{FUNC}}
print(round(telescope_magnification(1000, 25), 4))`,
      expected: "40.0\n",
    },
    {
      name: "telescope_magnification(500, 20) = 25x",
      code: `{{FUNC}}
print(round(telescope_magnification(500, 20), 4))`,
      expected: "25.0\n",
    },
    {
      name: "microscope_magnification(4, 10, 160) = 1000x",
      code: `{{FUNC}}
print(round(microscope_magnification(4, 10, 160), 4))`,
      expected: "1000.0\n",
    },
  ],
};
