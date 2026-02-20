import type { Lesson } from "../../types";

export const spliceSites: Lesson = {
	id: "splice-sites",
	title: "Splice Sites",
	chapterId: "gene-anatomy",
	content: `## Cutting and Pasting RNA

Most genes in complex organisms are **split** — the protein-coding regions (**exons**) are interrupted by non-coding sequences (**introns**). After transcription, introns are removed from the pre-mRNA in a process called **splicing**.

The cell uses short sequence signals to mark intron boundaries:
- **Donor site** (intron start): almost always begins with **GT**
- **Acceptor site** (intron end): almost always ends with **AG**

This is called the **GT–AG rule**.

\`\`\`python
def find_splice_sites(dna):
    donors    = [i for i in range(len(dna)-1) if dna[i:i+2] == "GT"]
    acceptors = [i for i in range(len(dna)-1) if dna[i:i+2] == "AG"]
    return donors, acceptors

def is_canonical_intron(seq):
    return seq[:2] == "GT" and seq[-2:] == "AG"
\`\`\`

Splicing errors are a major cause of genetic disease. AlphaGenome directly predicts **splice site usage** — the probability that each GT or AG in the genome is actually used for splicing — a capability with direct clinical applications.

### Your Task

Implement \`find_splice_sites(dna)\` and \`is_canonical_intron(seq)\`.`,

	starterCode: `def find_splice_sites(dna):
    # donors:    all positions i where dna[i:i+2] == "GT"
    # acceptors: all positions i where dna[i:i+2] == "AG"
    donors = []
    acceptors = []
    pass
    return donors, acceptors

def is_canonical_intron(seq):
    # Return True if seq starts with "GT" and ends with "AG"
    pass

dna = "EXONGTINTRONAGEXON"
donors, acceptors = find_splice_sites(dna)
print(donors)
print(acceptors)
print(is_canonical_intron("GTTTTTAG"))
print(is_canonical_intron("ATTTTTAG"))
`,

	solution: `def find_splice_sites(dna):
    donors    = [i for i in range(len(dna)-1) if dna[i:i+2] == "GT"]
    acceptors = [i for i in range(len(dna)-1) if dna[i:i+2] == "AG"]
    return donors, acceptors

def is_canonical_intron(seq):
    return seq[:2] == "GT" and seq[-2:] == "AG"

dna = "EXONGTINTRONAGEXON"
donors, acceptors = find_splice_sites(dna)
print(donors)
print(acceptors)
print(is_canonical_intron("GTTTTTAG"))
print(is_canonical_intron("ATTTTTAG"))
`,

	tests: [
		{
			name: "find_splice_sites finds GT donor at position 4",
			code: `{{FUNC}}
donors, _ = find_splice_sites("EXONGTINTRONAGEXON")
print(donors)`,
			expected: "[4]\n",
		},
		{
			name: "find_splice_sites finds AG acceptor at position 12",
			code: `{{FUNC}}
_, acceptors = find_splice_sites("EXONGTINTRONAGEXON")
print(acceptors)`,
			expected: "[12]\n",
		},
		{
			name: "canonical intron starts GT and ends AG",
			code: `{{FUNC}}
print(is_canonical_intron("GTTTTTAG"))
print(is_canonical_intron("ATTTTTAG"))
print(is_canonical_intron("GTTTTTAC"))`,
			expected: "True\nFalse\nFalse\n",
		},
		{
			name: "find_splice_sites with multiple sites",
			code: `{{FUNC}}
donors, acceptors = find_splice_sites("GTAAGTAG")
print(donors)
print(acceptors)`,
			expected: "[0, 4]\n[3, 6]\n",
		},
	],
};
