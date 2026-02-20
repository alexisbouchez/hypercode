import type { Lesson } from "../../types";

export const complement: Lesson = {
	id: "complement",
	title: "Complement & Reverse Complement",
	chapterId: "dna-basics",
	content: `## Two Strands, One Molecule

DNA is double-stranded. The two strands are **complementary** — each base pairs with its partner:

- A pairs with T
- T pairs with A
- G pairs with C
- C pairs with G

The complement of \`ATCG\` is \`TAGC\`.

But DNA strands run in opposite directions (antiparallel). To get the sequence of the other strand in the conventional 5'→3' direction, you take the complement and **reverse** it — the **reverse complement**:

\`\`\`python
def complement(seq):
    table = {"A": "T", "T": "A", "G": "C", "C": "G"}
    return "".join(table[b] for b in seq)

def reverse_complement(seq):
    return complement(seq)[::-1]

print(complement("ATCG"))          # TAGC
print(reverse_complement("ATCG"))  # CGAT
\`\`\`

When AlphaGenome receives a DNA input, it analyzes **both strands** — the sequence and its reverse complement. Regulatory elements can sit on either strand.

### Your Task

Implement \`complement(seq)\` and \`reverse_complement(seq)\`.`,

	starterCode: `def complement(seq):
    # Map each base to its complement: A↔T, G↔C
    table = {"A": "T", "T": "A", "G": "C", "C": "G"}
    pass

def reverse_complement(seq):
    # Take complement then reverse it
    pass

print(complement("ATCG"))
print(complement("AAATTTGGGCCC"))
print(reverse_complement("ATCG"))
print(reverse_complement("ATGCATGC"))
`,

	solution: `def complement(seq):
    table = {"A": "T", "T": "A", "G": "C", "C": "G"}
    return "".join(table[b] for b in seq)

def reverse_complement(seq):
    return complement(seq)[::-1]

print(complement("ATCG"))
print(complement("AAATTTGGGCCC"))
print(reverse_complement("ATCG"))
print(reverse_complement("ATGCATGC"))
`,

	tests: [
		{
			name: "complement of ATCG is TAGC",
			code: `{{FUNC}}
print(complement("ATCG"))`,
			expected: "TAGC\n",
		},
		{
			name: "complement swaps all bases correctly",
			code: `{{FUNC}}
print(complement("AAATTTGGGCCC"))`,
			expected: "TTTAAACCCGGG\n",
		},
		{
			name: "reverse_complement of ATCG is CGAT",
			code: `{{FUNC}}
print(reverse_complement("ATCG"))`,
			expected: "CGAT\n",
		},
		{
			name: "reverse_complement is its own inverse",
			code: `{{FUNC}}
seq = "ATGCATGC"
print(reverse_complement(reverse_complement(seq)) == seq)`,
			expected: "True\n",
		},
	],
};
