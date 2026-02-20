import type { Lesson } from "../../types";

export const orfs: Lesson = {
	id: "orfs",
	title: "Open Reading Frames",
	chapterId: "gene-anatomy",
	content: `## Finding Genes

An **Open Reading Frame** (ORF) is a stretch of DNA that could encode a protein. It starts at an ATG start codon and ends at the first in-frame stop codon (TAA, TAG, or TGA).

\`\`\`
...AAATGCGATAACC...
      ↑ start    ↑ stop
      ATG→CGA→TAA
\`\`\`

To find an ORF:
1. Scan the sequence for ATG
2. From that ATG, read codons in triplets
3. Return the sequence from ATG up to and including the first stop codon

\`\`\`python
def find_orf(seq):
    for i in range(len(seq) - 2):
        if seq[i:i+3] == "ATG":
            for j in range(i, len(seq) - 2, 3):
                codon = seq[j:j+3]
                if codon in ("TAA", "TAG", "TGA"):
                    return seq[i:j+3]
    return ""

print(find_orf("AAATGCGATAA"))   # ATGCGATAA
print(find_orf("ATGTTTGCCTAG"))  # ATGTTTGCCTAG
print(find_orf("AAACCC"))        # (empty — no ORF)
\`\`\`

AlphaGenome predicts **where genes start and end** across hundreds of different cell types — essentially running this kind of search at genomic scale with learned, cell-type-specific rules.

### Your Task

Implement \`find_orf(seq)\` that returns the first ORF found, or an empty string if none exists.`,

	starterCode: `def find_orf(seq):
    # Scan for ATG, then read triplets until a stop codon
    # Return seq[start:stop_end], or "" if no ORF found
    pass

print(find_orf("AAATGCGATAA"))
print(find_orf("ATGTTTGCCTAG"))
print(find_orf("AAACCC"))
print(find_orf("CCCATGTAA"))
`,

	solution: `def find_orf(seq):
    for i in range(len(seq) - 2):
        if seq[i:i+3] == "ATG":
            for j in range(i, len(seq) - 2, 3):
                codon = seq[j:j+3]
                if codon in ("TAA", "TAG", "TGA"):
                    return seq[i:j+3]
    return ""

print(find_orf("AAATGCGATAA"))
print(find_orf("ATGTTTGCCTAG"))
print(find_orf("AAACCC"))
print(find_orf("CCCATGTAA"))
`,

	tests: [
		{
			name: "find_orf finds ORF starting mid-sequence",
			code: `{{FUNC}}
print(find_orf("AAATGCGATAA"))`,
			expected: "ATGCGATAA\n",
		},
		{
			name: "find_orf finds ORF at start of sequence",
			code: `{{FUNC}}
print(find_orf("ATGTTTGCCTAG"))`,
			expected: "ATGTTTGCCTAG\n",
		},
		{
			name: "find_orf returns empty string when no ORF exists",
			code: `{{FUNC}}
print(find_orf("AAACCC"))`,
			expected: "\n",
		},
		{
			name: "find_orf with ATG followed by TAA",
			code: `{{FUNC}}
print(find_orf("CCCATGTAA"))`,
			expected: "ATGTAA\n",
		},
	],
};
