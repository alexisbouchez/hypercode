import type { Lesson } from "../../types";

export const varianceStd: Lesson = {
	id: "variance-std",
	title: "Variance & Standard Deviation",
	chapterId: "descriptive",
	content: `## Spread of Data

**Variance** and **standard deviation** measure how spread out data is around the mean.

\`\`\`python
import statistics

data = [1, 2, 3, 4, 5]

var = statistics.variance(data)   # sample variance (ddof=1)
std = statistics.stdev(data)      # sample std deviation

print(round(var, 2))   # 2.5
print(round(std, 2))   # 1.58
\`\`\`

### Population vs Sample

Use \`statistics.pvariance\` / \`statistics.pstdev\` for the **population** — when you have all the data.
Use \`statistics.variance\` / \`statistics.stdev\` for a **sample** — when your data is a subset of a larger population. This gives an unbiased estimate (ddof=1).

### Standard Deviation

The standard deviation is the square root of variance — it has the **same units as the data**, making it easier to interpret.

For normally distributed data, approximately:
- **68%** of values fall within ±1 std
- **95%** of values fall within ±2 std
- **99.7%** of values fall within ±3 std

### Your Task

Implement \`spread(data)\` that prints the **sample variance** (rounded to 2 decimal places) and **sample standard deviation** (rounded to 2 decimal places).`,

	starterCode: `import statistics

def spread(data):
    # Print sample variance (round 2) and sample std (round 2)
    pass

spread([1, 2, 3, 4, 5])
`,

	solution: `import statistics

def spread(data):
    print(round(float(statistics.variance(data)), 2))
    print(round(float(statistics.stdev(data)), 2))

spread([1, 2, 3, 4, 5])
`,

	tests: [
		{
			name: "spread([1,2,3,4,5]) → var=2.5, std=1.58",
			expected: "2.5\n1.58\n",
		},
		{
			name: "spread([10,10,10]) → var=0.0, std=0.0",
			code: `{{FUNC}}
spread([10, 10, 10])`,
			expected: "0.0\n0.0\n",
		},
		{
			name: "spread([2,4,6,8,10]) → var=10.0, std=3.16",
			code: `{{FUNC}}
spread([2, 4, 6, 8, 10])`,
			expected: "10.0\n3.16\n",
		},
		{
			name: "spread([0,0,0,0,10]) → var=20.0, std=4.47",
			code: `{{FUNC}}
spread([0, 0, 0, 0, 10])`,
			expected: "20.0\n4.47\n",
		},
	],
};
