import type { Lesson } from "../../types";

export const deMorgansLaw: Lesson = {
	id: "de-morgans-law",
	title: "De Morgan's Laws",
	chapterId: "boolean-algebra",
	content: `## De Morgan's Laws

De Morgan's laws are two identities that connect AND, OR, and NOT:

**Law 1:** NOT(A AND B) = NOT(A) OR NOT(B)
**Law 2:** NOT(A OR B) = NOT(A) AND NOT(B)

These are essential for simplifying Boolean expressions and for circuit optimization. For example, a NAND gate directly implements Law 1.

### Verifying Law 1

| A | B | NOT(A AND B) | NOT(A) OR NOT(B) |
|---|---|-------------|-----------------|
| 0 | 0 |      1      |        1        |
| 0 | 1 |      1      |        1        |
| 1 | 0 |      1      |        1        |
| 1 | 1 |      0      |        0        |

Both columns are identical — the law holds for all inputs.

### Your Task

Implement two functions:
- \`deMorgan1(a, b)\` — computes NOT(A AND B) and NOT(A) OR NOT(B), then returns \`1\` if they are equal (law verified), \`0\` otherwise
- \`deMorgan2(a, b)\` — same verification for Law 2: NOT(A OR B) vs NOT(A) AND NOT(B)

Print verification for all four input combinations for each law.`,

	starterCode: `function not(a) { return a ^ 1; }
function and(a, b) { return a & b; }
function or(a, b) { return a | b; }

function deMorgan1(a, b) {
  // Return 1 if NOT(A AND B) === NOT(A) OR NOT(B)
}

function deMorgan2(a, b) {
  // Return 1 if NOT(A OR B) === NOT(A) AND NOT(B)
}

for (let a = 0; a <= 1; a++)
  for (let b = 0; b <= 1; b++)
    console.log(deMorgan1(a, b), deMorgan2(a, b));
`,

	solution: `function not(a) { return a ^ 1; }
function and(a, b) { return a & b; }
function or(a, b) { return a | b; }

function deMorgan1(a, b) {
  return not(and(a, b)) === or(not(a), not(b)) ? 1 : 0;
}

function deMorgan2(a, b) {
  return not(or(a, b)) === and(not(a), not(b)) ? 1 : 0;
}

for (let a = 0; a <= 1; a++)
  for (let b = 0; b <= 1; b++)
    console.log(deMorgan1(a, b), deMorgan2(a, b));
`,

	tests: [
		{
			name: "both laws hold for all inputs",
			expected: "1 1\n1 1\n1 1\n1 1\n",
		},
		{
			name: "deMorgan1(1,1) is 1",
			code: `{{FUNC}}
console.log(deMorgan1(1, 1));`,
			expected: "1\n",
		},
		{
			name: "deMorgan2(0,0) is 1",
			code: `{{FUNC}}
console.log(deMorgan2(0, 0));`,
			expected: "1\n",
		},
	],
};
