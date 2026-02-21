import type { Lesson } from "../../types";

export const meanMedianMode: Lesson = {
	id: "mean-median-mode",
	title: "Mean, Median, Mode",
	chapterId: "descriptive",
	content: `## Central Tendency

The three most important measures of central tendency:

- **Mean** — the arithmetic average: $\bar{x} = \frac{1}{n}\sum_{i=1}^n x_i$
- **Median** — the middle value when data is sorted
- **Mode** — the most frequently occurring value

\`\`\`python
import statistics

data = [2, 4, 4, 4, 5, 5, 7, 9]

print(round(statistics.fmean(data), 1))   # 5.0
print(round(statistics.median(data), 1))  # 4.5
print(statistics.mode(data))              # 4
\`\`\`

### When to Use Each

| Measure | Best for | Sensitive to outliers? |
|---------|----------|----------------------|
| Mean | Symmetric distributions | Yes |
| Median | Skewed data, outliers present | No |
| Mode | Categorical data, most common | No |

### Your Task

Implement \`central_tendency(data)\` that prints the mean (rounded to 1 decimal), median (rounded to 1 decimal), and mode of the input list.`,

	starterCode: `import statistics

def central_tendency(data):
    # Print mean (round 1 decimal), median (round 1 decimal), mode
    pass

central_tendency([2, 4, 4, 4, 5, 5, 7, 9])
`,

	solution: `import statistics

def central_tendency(data):
    print(round(statistics.fmean(data), 1))
    print(round(float(statistics.median(data)), 1))
    print(statistics.mode(data))

central_tendency([2, 4, 4, 4, 5, 5, 7, 9])
`,

	tests: [
		{
			name: "central_tendency([2,4,4,4,5,5,7,9]) → mean=5.0, median=4.5, mode=4",
			expected: "5.0\n4.5\n4\n",
		},
		{
			name: "central_tendency([1,2,2,3,4]) → mean=2.4, median=2.0, mode=2",
			code: `{{FUNC}}
central_tendency([1, 2, 2, 3, 4])`,
			expected: "2.4\n2.0\n2\n",
		},
		{
			name: "central_tendency([10,10,20,30]) → mean=17.5, median=15.0, mode=10",
			code: `{{FUNC}}
central_tendency([10, 10, 20, 30])`,
			expected: "17.5\n15.0\n10\n",
		},
		{
			name: "central_tendency([5,5,5,5]) → mean=5.0, median=5.0, mode=5",
			code: `{{FUNC}}
central_tendency([5, 5, 5, 5])`,
			expected: "5.0\n5.0\n5\n",
		},
	],
};
