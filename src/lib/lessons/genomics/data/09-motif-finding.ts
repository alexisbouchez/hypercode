import type { Lesson } from "../../types";

export const motifFinding: Lesson = {
	id: "motif-finding",
	title: "Motif Finding",
	chapterId: "regulation",
	content: `## Transcription Factor Binding

Most of the genome does not code for proteins — it is **regulatory DNA**. These regions contain short sequences called **motifs** that serve as landing pads for **transcription factors** (TF) — proteins that bind DNA and switch genes on or off.

A classic example: the **TATA box** (motif: TATA) is found about 30 bases before many gene start sites. It recruits the transcription machinery.

Finding where a motif occurs in a genome is a fundamental bioinformatics operation:

\`\`\`python
def find_motif(seq, motif):
    positions = []
    for i in range(len(seq) - len(motif) + 1):
        if seq[i:i+len(motif)] == motif:
            positions.append(i)
    return positions

seq = "AATATAATATA"
print(find_motif(seq, "TATA"))  # [2, 7]
\`\`\`

AlphaGenome predicts **transcription factor binding** across the whole genome — thousands of different TFs, each recognizing its own sequence motif. It can detect when a single-base mutation creates or destroys a binding site.

### Your Task

Implement \`find_motif(seq, motif)\` that returns all positions where the motif occurs, and \`motif_count(seq, motif)\` that returns the count.`,

	starterCode: `def find_motif(seq, motif):
    # Return list of positions where motif starts in seq
    pass

def motif_count(seq, motif):
    return len(find_motif(seq, motif))

seq = "AATATAATATA"
print(find_motif(seq, "TATA"))
print(motif_count(seq, "TATA"))
print(find_motif("ATCGATCG", "ATC"))
print(motif_count("GCGCGC", "GC"))
`,

	solution: `def find_motif(seq, motif):
    positions = []
    for i in range(len(seq) - len(motif) + 1):
        if seq[i:i+len(motif)] == motif:
            positions.append(i)
    return positions

def motif_count(seq, motif):
    return len(find_motif(seq, motif))

seq = "AATATAATATA"
print(find_motif(seq, "TATA"))
print(motif_count(seq, "TATA"))
print(find_motif("ATCGATCG", "ATC"))
print(motif_count("GCGCGC", "GC"))
`,

	tests: [
		{
			name: "find_motif finds TATA at correct positions",
			code: `{{FUNC}}
print(find_motif("AATATAATATA", "TATA"))`,
			expected: "[2, 7]\n",
		},
		{
			name: "find_motif finds multiple ATC occurrences",
			code: `{{FUNC}}
print(find_motif("ATCGATCG", "ATC"))`,
			expected: "[0, 4]\n",
		},
		{
			name: "motif_count returns correct count",
			code: `{{FUNC}}
print(motif_count("GCGCGC", "GC"))`,
			expected: "3\n",
		},
		{
			name: "find_motif returns empty list when motif absent",
			code: `{{FUNC}}
print(find_motif("AAATTT", "GC"))`,
			expected: "[]\n",
		},
	],
};
