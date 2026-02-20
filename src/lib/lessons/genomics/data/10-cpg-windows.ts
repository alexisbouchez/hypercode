import type { Lesson } from "../../types";

export const cpgWindows: Lesson = {
	id: "cpg-windows",
	title: "CpG Islands",
	chapterId: "regulation",
	content: `## Promoter Landmarks

A **CpG island** is a stretch of DNA with unusually high GC content and an abundance of **CpG dinucleotides** (cytosine followed by guanine). About 70% of human gene promoters — the regulatory switches that turn genes on — overlap with CpG islands.

To find CpG islands, we use a **sliding window**: compute GC content in a small window, then slide it one position at a time across the sequence:

\`\`\`python
def sliding_window_gc(seq, window_size):
    results = []
    for i in range(len(seq) - window_size + 1):
        window = seq[i:i+window_size]
        gc = (window.count("G") + window.count("C")) / window_size
        results.append(round(gc, 2))
    return results
\`\`\`

This produces a **GC profile** of the sequence — peaks in the profile mark candidate promoter regions.

AlphaGenome's convolutional layers implicitly learn these kinds of local composition patterns across thousands of training examples, and its transformer layers connect these local patterns to long-range regulatory context.

### Your Task

Implement \`sliding_window_gc(seq, window_size)\` that returns GC fractions (0.0–1.0) for each window position.`,

	starterCode: `def sliding_window_gc(seq, window_size):
    # For each position i, compute GC fraction of seq[i:i+window_size]
    # Round to 2 decimal places
    pass

seq = "ATATATGCGCGCATAT"
windows = sliding_window_gc(seq, 4)
print(windows)
print(max(windows))
print(windows.index(max(windows)))
`,

	solution: `def sliding_window_gc(seq, window_size):
    results = []
    for i in range(len(seq) - window_size + 1):
        window = seq[i:i+window_size]
        gc = (window.count("G") + window.count("C")) / window_size
        results.append(round(gc, 2))
    return results

seq = "ATATATGCGCGCATAT"
windows = sliding_window_gc(seq, 4)
print(windows)
print(max(windows))
print(windows.index(max(windows)))
`,

	tests: [
		{
			name: "sliding_window_gc produces correct profile",
			code: `{{FUNC}}
print(sliding_window_gc("ATATATGCGCGCATAT", 4))`,
			expected: "[0.0, 0.0, 0.0, 0.25, 0.5, 0.75, 1.0, 1.0, 1.0, 0.75, 0.5, 0.25, 0.0]\n",
		},
		{
			name: "max GC in profile is 1.0",
			code: `{{FUNC}}
windows = sliding_window_gc("ATATATGCGCGCATAT", 4)
print(max(windows))`,
			expected: "1.0\n",
		},
		{
			name: "GC-rich region starts at index 6",
			code: `{{FUNC}}
windows = sliding_window_gc("ATATATGCGCGCATAT", 4)
print(windows.index(max(windows)))`,
			expected: "6\n",
		},
		{
			name: "all-GC sequence gives all 1.0 windows",
			code: `{{FUNC}}
windows = sliding_window_gc("GCGCGC", 2)
print(windows)`,
			expected: "[1.0, 1.0, 1.0, 1.0, 1.0]\n",
		},
	],
};
