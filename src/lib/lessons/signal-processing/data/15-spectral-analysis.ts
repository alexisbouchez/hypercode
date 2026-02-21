import type { Lesson } from "../../types";

export const spectralAnalysis: Lesson = {
	id: "spectral-analysis",
	title: "Spectral Analysis",
	chapterId: "applications",
	content: `## Spectral Analysis

Spectral analysis extracts perceptual and statistical features from the frequency representation of a signal. Two fundamental descriptors are the spectral centroid and spectral flatness.

### Spectral Centroid

The **spectral centroid** is the "center of mass" of the spectrum. It correlates with perceived **brightness** — a higher centroid sounds brighter.

$$C = \\frac{\\sum_k f_k \\cdot |X[k]|}{\\sum_k |X[k]|}$$

where $f_k$ is the frequency (in Hz) of bin $k$ and $|X[k]|$ is its magnitude.

**Example:** With magnitudes $[1, 2, 3, 4]$ at frequencies $[0, 1, 2, 3]$ Hz:

$$C = \\frac{0 \\cdot 1 + 1 \\cdot 2 + 2 \\cdot 3 + 3 \\cdot 4}{1+2+3+4} = \\frac{20}{10} = 2.0 \\text{ Hz}$$

### Spectral Flatness

**Spectral flatness** (also called Wiener entropy) measures how **noise-like** vs **tonal** a signal is:

$$F = \\frac{\\text{geometric mean of } |X[k]|}{\\text{arithmetic mean of } |X[k]|}$$

- $F \\approx 1$: white noise (flat spectrum, all bins equal)
- $F \\approx 0$: pure tone (energy concentrated in one bin)

If any magnitude is zero or the arithmetic mean is zero, return $0$.

### Your Task

Implement:
- \`spectral_centroid(magnitudes, freqs)\` — weighted average frequency
- \`spectral_flatness(magnitudes)\` — geometric mean / arithmetic mean

\`\`\`python
import math

def spectral_centroid(magnitudes, freqs):
    total = sum(magnitudes)
    if total == 0:
        return 0.0
    return sum(f * m for f, m in zip(freqs, magnitudes)) / total

def spectral_flatness(magnitudes):
    n = len(magnitudes)
    arith = sum(magnitudes) / n
    if arith == 0:
        return 0.0
    if any(m <= 0 for m in magnitudes):
        return 0.0
    geo = math.exp(sum(math.log(m) for m in magnitudes) / n)
    return geo / arith
\`\`\``,

	starterCode: `import math

def spectral_centroid(magnitudes, freqs):
    # Return sum(f*|X|) / sum(|X|); return 0 if sum=0
    pass

def spectral_flatness(magnitudes):
    # Return geometric_mean / arithmetic_mean; return 0 if arith=0 or any m<=0
    pass

mags = [1.0, 2.0, 3.0, 4.0]
freqs = [0.0, 1.0, 2.0, 3.0]
print(round(spectral_centroid(mags, freqs), 4))

mags2 = [1.0, 1.0, 1.0, 1.0]
print(round(spectral_flatness(mags2), 4))
`,

	solution: `import math

def spectral_centroid(magnitudes, freqs):
    total = sum(magnitudes)
    if total == 0:
        return 0.0
    return sum(f * m for f, m in zip(freqs, magnitudes)) / total

def spectral_flatness(magnitudes):
    n = len(magnitudes)
    arith = sum(magnitudes) / n
    if arith == 0:
        return 0.0
    if any(m <= 0 for m in magnitudes):
        return 0.0
    geo = math.exp(sum(math.log(m) for m in magnitudes) / n)
    return geo / arith

mags = [1.0, 2.0, 3.0, 4.0]
freqs = [0.0, 1.0, 2.0, 3.0]
print(round(spectral_centroid(mags, freqs), 4))

mags2 = [1.0, 1.0, 1.0, 1.0]
print(round(spectral_flatness(mags2), 4))
`,

	tests: [
		{
			name: "spectral_centroid([1,2,3,4], [0,1,2,3]) = 2.0",
			code: `{{FUNC}}
print(round(spectral_centroid([1.0, 2.0, 3.0, 4.0], [0.0, 1.0, 2.0, 3.0]), 4))`,
			expected: "2.0\n",
		},
		{
			name: "spectral_centroid with single peak = peak frequency",
			code: `{{FUNC}}
print(round(spectral_centroid([0.0, 1.0, 0.0, 0.0], [100.0, 200.0, 300.0, 400.0]), 4))`,
			expected: "200.0\n",
		},
		{
			name: "spectral_flatness of uniform spectrum = 1.0",
			code: `{{FUNC}}
print(round(spectral_flatness([1.0, 1.0, 1.0, 1.0]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "spectral_flatness of single spike = 0.0",
			code: `{{FUNC}}
print(round(spectral_flatness([0.0, 0.0, 10.0, 0.0]), 4))`,
			expected: "0.0\n",
		},
		{
			name: "spectral_flatness([1,2,4,8]) = 0.7542",
			code: `{{FUNC}}
print(round(spectral_flatness([1.0, 2.0, 4.0, 8.0]), 4))`,
			expected: "0.7542\n",
		},
	],
};
