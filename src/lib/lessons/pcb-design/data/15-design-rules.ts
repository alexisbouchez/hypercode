import type { Lesson } from "../../types";

export const designRules: Lesson = {
	id: "design-rules",
	title: "Design Rule Checking",
	chapterId: "signal-integrity",
	content: `## Design Rule Checking (DRC)

**Design rules** are minimum constraints that PCB manufacturers impose based on their process capabilities. Violating them makes the board unfabricable.

Common PCB design rules (for standard 2-layer boards):

| Rule | Typical Value |
|------|--------------|
| Minimum trace width | 0.1 mm (4 mil) |
| Minimum trace spacing | 0.1 mm (4 mil) |
| Minimum drill diameter | 0.2 mm |
| Minimum annular ring | 0.1 mm |
| Minimum copper to board edge | 0.3 mm |

The **annular ring** is the copper ring around a drilled hole. For a via with pad diameter D_pad and drill diameter D_drill:
\`\`\`
annularRing = (D_pad - D_drill) / 2
\`\`\`

A **clearance check** verifies that two nets don't come closer than the minimum spacing.

### Your Task

Implement a DRC checker function \`checkDesignRules(rules)\` that takes an array of rule objects and returns an array of violations.

Each rule object has: \`{ name, value, min }\`

Return violation objects: \`{ name, value, min, violation: true }\` for rules where \`value < min\`.`,

	starterCode: `function checkDesignRules(rules) {
  // Return array of violations where value < min
}

function annularRing(padDiameterMm, drillDiameterMm) {
  // Return annular ring size in mm
}

const rules = [
  { name: "trace width", value: 0.15, min: 0.1 },
  { name: "trace spacing", value: 0.08, min: 0.1 },
  { name: "drill diameter", value: 0.25, min: 0.2 },
  { name: "annular ring", value: annularRing(0.5, 0.3), min: 0.1 },
  { name: "edge clearance", value: 0.25, min: 0.3 },
];

const violations = checkDesignRules(rules);
console.log(violations.length);
violations.forEach(v => console.log(v.name));
`,

	solution: `function checkDesignRules(rules) {
  return rules.filter(r => r.value < r.min);
}

function annularRing(padDiameterMm, drillDiameterMm) {
  return (padDiameterMm - drillDiameterMm) / 2;
}

const rules = [
  { name: "trace width", value: 0.15, min: 0.1 },
  { name: "trace spacing", value: 0.08, min: 0.1 },
  { name: "drill diameter", value: 0.25, min: 0.2 },
  { name: "annular ring", value: annularRing(0.5, 0.3), min: 0.1 },
  { name: "edge clearance", value: 0.25, min: 0.3 },
];

const violations = checkDesignRules(rules);
console.log(violations.length);
violations.forEach(v => console.log(v.name));
`,

	tests: [
		{
			name: "detects 2 violations: trace spacing and edge clearance",
			expected: "2\ntrace spacing\nedge clearance\n",
		},
		{
			name: "no violations when all rules pass",
			code: `{{FUNC}}
const passing = [
  { name: "trace width", value: 0.2, min: 0.1 },
  { name: "drill", value: 0.3, min: 0.2 },
];
console.log(checkDesignRules(passing).length);`,
			expected: "0\n",
		},
		{
			name: "annularRing(0.5, 0.3) returns 0.1",
			code: `{{FUNC}}
console.log(annularRing(0.5, 0.3).toFixed(1));`,
			expected: "0.1\n",
		},
	],
};
