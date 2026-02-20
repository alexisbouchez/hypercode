import type { Lesson } from "../../types";

export const transcription: Lesson = {
	id: "transcription",
	title: "Transcription",
	chapterId: "gene-anatomy",
	content: `## DNA → RNA

Genes are not directly read to make proteins. First, the DNA is copied into **messenger RNA (mRNA)** in a process called **transcription**.

RNA uses the same four-letter alphabet as DNA, with one change: Thymine (T) is replaced by **Uracil (U)**.

| DNA base | RNA base |
|----------|----------|
| A | A |
| T | **U** |
| G | G |
| C | C |

\`\`\`python
def transcribe(dna):
    return dna.replace("T", "U")

print(transcribe("ATGCGATAA"))  # AUGCGAUAA
print(transcribe("ATCG"))       # AUCG
\`\`\`

The resulting mRNA sequence is then exported from the nucleus and used as a template to build a protein. AlphaGenome predicts **RNA production levels** (expression) for thousands of genes across different tissues — liver, brain, heart, and more — from the DNA sequence alone.

### Your Task

Implement \`transcribe(dna)\` that converts a DNA sequence to its RNA equivalent.`,

	starterCode: `def transcribe(dna):
    # Replace all T with U
    pass

print(transcribe("ATGCGATAA"))
print(transcribe("ATCG"))
print(transcribe("TTTAAA"))
print(transcribe("GCGCGC"))
`,

	solution: `def transcribe(dna):
    return dna.replace("T", "U")

print(transcribe("ATGCGATAA"))
print(transcribe("ATCG"))
print(transcribe("TTTAAA"))
print(transcribe("GCGCGC"))
`,

	tests: [
		{
			name: "transcribe replaces T with U",
			code: `{{FUNC}}
print(transcribe("ATGCGATAA"))`,
			expected: "AUGCGAUAA\n",
		},
		{
			name: "transcribe handles all T bases",
			code: `{{FUNC}}
print(transcribe("TTTAAA"))`,
			expected: "UUUAAA\n",
		},
		{
			name: "transcribe leaves A, G, C unchanged",
			code: `{{FUNC}}
print(transcribe("GCGCGC"))`,
			expected: "GCGCGC\n",
		},
		{
			name: "transcribe converts ATCG",
			code: `{{FUNC}}
print(transcribe("ATCG"))`,
			expected: "AUCG\n",
		},
	],
};
