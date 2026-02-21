import type { Lesson } from "../../types";

export const crossSectionUnits: Lesson = {
	id: "cross-section-units",
	title: "Cross Section Units and Luminosity",
	chapterId: "scattering",
	content: `## Cross Section Units and Luminosity

In particle physics experiments the interaction probability between two particles is quantified by the **cross section** $\\sigma$ — a measure of the effective area for a given process.

### The Barn and its Submultiples

The natural unit for cross sections is the **barn**:

$$1 \\text{ b} = 10^{-28} \\text{ m}^2 = 10^{-24} \\text{ cm}^2$$

Modern collider physics works with much smaller cross sections:

| Unit | Value |
|------|-------|
| 1 mb (millibarn) | $10^{-3}$ b |
| 1 μb (microbarn) | $10^{-6}$ b |
| 1 nb (nanobarn) | $10^{-9}$ b |
| 1 pb (picobarn) | $10^{-12}$ b |
| 1 fb (femtobarn) | $10^{-15}$ b |

### Conversion from Natural Units

In natural units (where $\\hbar = c = 1$), cross sections are computed in GeV$^{-2}$. The conversion to physical units uses:

$$1 \\text{ GeV}^{-2} = 0.3894 \\text{ mb} = 3.894 \\times 10^8 \\text{ pb}$$

### Luminosity and Event Rates

The **instantaneous luminosity** $\\mathcal{L}$ [cm$^{-2}$s$^{-1}$] relates the event rate $R$ to the cross section:

$$R = \\mathcal{L} \\cdot \\sigma$$

The **integrated luminosity** $\\mathcal{L}_{\\text{int}}$ [fb$^{-1}$] gives the total number of events:

$$N_{\\text{events}} = \\mathcal{L}_{\\text{int}} \\cdot \\sigma$$

At the LHC, Run 2 (2015–2018) delivered about **140 fb$^{-1}$** of integrated luminosity at 13 TeV. Key benchmark cross sections:

- $pp \\to H$ (Higgs production via gluon fusion): $\\sigma \\approx 55$ pb at 13 TeV
- $pp \\to Z$: $\\sigma \\approx 60$ nb $= 60{,}000$ pb

### Your Task

Implement:
- \`GeV2_to_pb(sigma_GeV2)\` — convert cross section from GeV$^{-2}$ to pb
- \`events_expected(sigma_pb, luminosity_fb_inv)\` — number of events (1 fb$^{-1}$ = 1000 pb$^{-1}$)
- \`luminosity_from_rate(event_rate_per_s, sigma_cm2)\` — instantaneous luminosity in cm$^{-2}$s$^{-1}$

All constants must be defined **inside** each function body.`,

	starterCode: `import math

def GeV2_to_pb(sigma_GeV2):
    # 1 GeV^-2 = 3.894e8 pb
    pass

def events_expected(sigma_pb, luminosity_fb_inv):
    # N = sigma[pb] * L[fb^-1] * 1000  (since 1 fb^-1 = 1000 pb^-1)
    pass

def luminosity_from_rate(event_rate_per_s, sigma_cm2):
    # L = R / sigma
    pass
`,

	solution: `import math

def GeV2_to_pb(sigma_GeV2):
    return sigma_GeV2 * 3.894e8

def events_expected(sigma_pb, luminosity_fb_inv):
    return sigma_pb * luminosity_fb_inv * 1e3

def luminosity_from_rate(event_rate_per_s, sigma_cm2):
    return event_rate_per_s / sigma_cm2
`,

	tests: [
		{
			name: "GeV2_to_pb(1.0) = 3.894e8 pb",
			code: `{{FUNC}}
print(GeV2_to_pb(1.0))`,
			expected: "389400000.0\n",
		},
		{
			name: "events_expected(55.0, 140.0) = 7.7M Higgs events at LHC Run 2",
			code: `{{FUNC}}
print(round(events_expected(55.0, 140.0), 0))`,
			expected: "7700000.0\n",
		},
		{
			name: "events_expected(60000.0, 140.0) = 8.4B Z events at LHC Run 2",
			code: `{{FUNC}}
print(round(events_expected(60000.0, 140.0), 0))`,
			expected: "8400000000.0\n",
		},
		{
			name: "luminosity_from_rate(100.0, 1e-36) = 1e+38 cm⁻²s⁻¹",
			code: `{{FUNC}}
print(luminosity_from_rate(100.0, 1e-36))`,
			expected: "1e+38\n",
		},
	],
};
