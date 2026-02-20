import type { Lesson } from "../../types";

export const gcContent: Lesson = {
	id: "gc-content",
	title: "GC Content",
	chapterId: "dna-basics",
	content: `## Measuring Sequence Composition

**GC content** is the percentage of bases in a DNA sequence that are either Guanine (G) or Cytosine (C). It is a basic but informative measure of genomic composition.

\`\`\`
GC% = (count(G) + count(C)) / total_length × 100
\`\`\`

GC content matters for several reasons:

- **Stability**: G–C base pairs have three hydrogen bonds (vs. two for A–T), making GC-rich regions more thermally stable.
- **CpG islands**: Regions with unusually high GC content and many C–G dinucleotides. They typically mark **gene promoters** — the switches that turn genes on.
- **Genome variation**: The human genome averages ~41% GC, but individual regions vary widely.

\`\`\`python
def gc_content(seq):
    if len(seq) == 0:
        return 0.0
    gc = seq.count("G") + seq.count("C")
    return round(gc / len(seq) * 100, 2)

print(gc_content("ATCG"))    # 50.0  (2 out of 4)
print(gc_content("GCGCGC"))  # 100.0
print(gc_content("ATATAT"))  # 0.0
\`\`\`

AlphaGenome's architecture uses convolutional layers to detect GC-rich regions and CpG islands among thousands of other short sequence patterns.

### Your Task

Implement \`gc_content(seq)\` that returns GC percentage rounded to 2 decimal places.`,

	starterCode: `def gc_content(seq):
    # Return (G + C) / length * 100, rounded to 2 decimal places
    # Return 0.0 for empty sequences
    pass

print(gc_content("ATCG"))
print(gc_content("GCGCGC"))
print(gc_content("ATATAT"))
print(gc_content("ATGCATGC"))
`,

	solution: `def gc_content(seq):
    if len(seq) == 0:
        return 0.0
    gc = seq.count("G") + seq.count("C")
    return round(gc / len(seq) * 100, 2)

print(gc_content("ATCG"))
print(gc_content("GCGCGC"))
print(gc_content("ATATAT"))
print(gc_content("ATGCATGC"))
`,

	tests: [
		{
			name: "ATCG has 50% GC content",
			code: `{{FUNC}}
print(gc_content("ATCG"))`,
			expected: "50.0\n",
		},
		{
			name: "GCGCGC has 100% GC content",
			code: `{{FUNC}}
print(gc_content("GCGCGC"))`,
			expected: "100.0\n",
		},
		{
			name: "ATATAT has 0% GC content",
			code: `{{FUNC}}
print(gc_content("ATATAT"))`,
			expected: "0.0\n",
		},
		{
			name: "empty sequence returns 0.0",
			code: `{{FUNC}}
print(gc_content(""))`,
			expected: "0.0\n",
		},
	],
};
