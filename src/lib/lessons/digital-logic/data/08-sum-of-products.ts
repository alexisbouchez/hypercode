import type { Lesson } from "../../types";

export const sumOfProducts: Lesson = {
	id: "sum-of-products",
	title: "Sum of Products",
	chapterId: "boolean-algebra",
	content: `## Sum of Products (SOP) Form

Any Boolean function can be expressed as a **Sum of Products** (SOP) — an OR of AND terms.

Each AND term is called a **minterm**. A minterm corresponds to a row in the truth table where the output is \`1\`. It's the product (AND) of all input variables, where each variable appears either uncomplemented or complemented.

For example, if a 2-input function outputs \`1\` only at row \`10\` (A=1, B=0):
- Minterm = A AND NOT(B) = A·B'

The **canonical SOP** is formed by ORing all minterms.

### Finding Minterms

Given the truth table output array \`[0, 0, 1, 1]\` for inputs ordered 00, 01, 10, 11:
- Row 0 (00) → 0 (not a minterm)
- Row 1 (01) → 0 (not a minterm)
- Row 2 (10) → 1 → **minterm 2**
- Row 3 (11) → 1 → **minterm 3**

Minterms: [2, 3]

### Your Task

Implement \`minterms(outputs)\` that takes an array of truth-table outputs (length must be a power of 2) and returns an array of minterm indices (rows where output is \`1\`).

Then implement \`evalSOP(minterms, n, inputs)\` that evaluates a function at given inputs using its minterm list: return \`1\` if the input combination index is in minterms, else \`0\`.`,

	starterCode: `function minterms(outputs) {
  // Return indices where outputs[i] === 1
}

function evalSOP(minterms, n, ...inputs) {
  // Convert inputs to row index, return 1 if in minterms
}

const outputs = [0, 0, 1, 1]; // A OR B truth table starting from 00
console.log(minterms(outputs).join(", "));
console.log(evalSOP(minterms(outputs), 2, 1, 0));
console.log(evalSOP(minterms(outputs), 2, 0, 0));
`,

	solution: `function minterms(outputs) {
  return outputs.reduce((acc, v, i) => {
    if (v === 1) acc.push(i);
    return acc;
  }, []);
}

function evalSOP(mts, n, ...inputs) {
  let idx = 0;
  for (let i = 0; i < n; i++) {
    idx = (idx << 1) | inputs[i];
  }
  return mts.includes(idx) ? 1 : 0;
}

const outputs = [0, 0, 1, 1];
console.log(minterms(outputs).join(", "));
console.log(evalSOP(minterms(outputs), 2, 1, 0));
console.log(evalSOP(minterms(outputs), 2, 0, 0));
`,

	tests: [
		{
			name: "minterms of [0,0,1,1]",
			expected: "2, 3\n1\n0\n",
		},
		{
			name: "minterms of XOR",
			code: `{{FUNC}}
const xorOutputs = [0, 1, 1, 0];
console.log(minterms(xorOutputs).join(", "));`,
			expected: "1, 2\n",
		},
		{
			name: "evalSOP for AND function",
			code: `{{FUNC}}
const andMts = [3];
console.log(evalSOP(andMts, 2, 1, 1));
console.log(evalSOP(andMts, 2, 1, 0));`,
			expected: "1\n0\n",
		},
	],
};
