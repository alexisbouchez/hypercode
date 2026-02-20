import type { Lesson } from "../../types";

export const codons: Lesson = {
	id: "codons",
	title: "Codons",
	chapterId: "dna-basics",
	content: `## The Genetic Code

DNA is read in groups of three bases called **codons**. Each codon specifies one amino acid (or a start/stop signal). This is the **genetic code**.

There are 4³ = 64 possible codons but only 20 amino acids, so the code is redundant — multiple codons can encode the same amino acid.

Two special codons:
- **ATG** — the **start codon**. It marks where a gene begins and codes for Methionine (M).
- **TAA, TAG, TGA** — **stop codons**. They signal the end of the protein.

\`\`\`python
def split_into_codons(seq):
    return [seq[i:i+3] for i in range(0, len(seq) - 2, 3)]

def is_start_codon(codon):
    return codon == "ATG"

def is_stop_codon(codon):
    return codon in ("TAA", "TAG", "TGA")

seq = "ATGCGATAA"
codons = split_into_codons(seq)
print(codons)              # ['ATG', 'CGA', 'TAA']
print(is_start_codon(codons[0]))  # True
print(is_stop_codon(codons[-1]))  # True
\`\`\`

AlphaGenome predicts **transcription start sites** — exactly where in the genome a gene begins to be read. Understanding codons is the first step to understanding those predictions.

### Your Task

Implement \`split_into_codons(seq)\`, \`is_start_codon(codon)\`, and \`is_stop_codon(codon)\`.`,

	starterCode: `def split_into_codons(seq):
    # Return a list of 3-character strings, reading from position 0
    pass

def is_start_codon(codon):
    # Return True if codon is "ATG"
    pass

def is_stop_codon(codon):
    # Return True if codon is TAA, TAG, or TGA
    pass

seq = "ATGCGATAA"
codons = split_into_codons(seq)
print(codons)
print(is_start_codon(codons[0]))
print(is_stop_codon(codons[-1]))
print(is_start_codon("CGA"))
print(is_stop_codon("ATG"))
`,

	solution: `def split_into_codons(seq):
    return [seq[i:i+3] for i in range(0, len(seq) - 2, 3)]

def is_start_codon(codon):
    return codon == "ATG"

def is_stop_codon(codon):
    return codon in ("TAA", "TAG", "TGA")

seq = "ATGCGATAA"
codons = split_into_codons(seq)
print(codons)
print(is_start_codon(codons[0]))
print(is_stop_codon(codons[-1]))
print(is_start_codon("CGA"))
print(is_stop_codon("ATG"))
`,

	tests: [
		{
			name: "split_into_codons splits ATGCGATAA into triplets",
			code: `{{FUNC}}
print(split_into_codons("ATGCGATAA"))`,
			expected: "['ATG', 'CGA', 'TAA']\n",
		},
		{
			name: "split_into_codons handles 6-base sequence",
			code: `{{FUNC}}
print(split_into_codons("ATGTAG"))`,
			expected: "['ATG', 'TAG']\n",
		},
		{
			name: "is_start_codon correctly identifies ATG",
			code: `{{FUNC}}
print(is_start_codon("ATG"))
print(is_start_codon("CGA"))
print(is_start_codon("TAA"))`,
			expected: "True\nFalse\nFalse\n",
		},
		{
			name: "is_stop_codon identifies all three stop codons",
			code: `{{FUNC}}
print(is_stop_codon("TAA"))
print(is_stop_codon("TAG"))
print(is_stop_codon("TGA"))
print(is_stop_codon("ATG"))`,
			expected: "True\nTrue\nTrue\nFalse\n",
		},
	],
};
