import type { Lesson } from "../../types";

export const dnaSequences: Lesson = {
	id: "dna-sequences",
	title: "DNA Sequences",
	chapterId: "dna-basics",
	content: `## The Language of Life

DNA is the instruction manual for every living organism. It is written in an alphabet of just **four letters**, called nucleotide bases:

| Base | Letter | Pairs with |
|------|--------|-----------|
| Adenine  | A | T |
| Thymine  | T | A |
| Guanine  | G | C |
| Cytosine | C | G |

A DNA sequence is a string over this four-letter alphabet. The human genome is roughly **3 billion** of these letters.

\`\`\`python
seq = "ATCGATCGATCG"
print(len(seq))          # 12
print(seq.count("A"))    # 3
\`\`\`

Programs like **AlphaGenome** read stretches of up to **one million** base pairs at a time and predict how those letters control which genes turn on in which tissues.

### Your Task

Implement \`dna_length(seq)\` and \`count_base(seq, base)\` to start exploring DNA sequences.`,

	starterCode: `def dna_length(seq):
    # Return the number of bases in the sequence
    pass

def count_base(seq, base):
    # Return how many times base appears in seq
    pass

seq = "ATCGATCGATCG"
print(dna_length(seq))
print(count_base(seq, "A"))
print(count_base(seq, "G"))
print(count_base(seq, "C"))
`,

	solution: `def dna_length(seq):
    return len(seq)

def count_base(seq, base):
    return seq.count(base)

seq = "ATCGATCGATCG"
print(dna_length(seq))
print(count_base(seq, "A"))
print(count_base(seq, "G"))
print(count_base(seq, "C"))
`,

	tests: [
		{
			name: "dna_length returns correct length",
			code: `{{FUNC}}
print(dna_length("ATCGATCGATCG"))
print(dna_length("AAAA"))
print(dna_length(""))`,
			expected: "12\n4\n0\n",
		},
		{
			name: "count_base counts A correctly",
			code: `{{FUNC}}
print(count_base("ATCGATCGATCG", "A"))`,
			expected: "3\n",
		},
		{
			name: "count_base counts G and C",
			code: `{{FUNC}}
print(count_base("ATCGATCGATCG", "G"))
print(count_base("ATCGATCGATCG", "C"))`,
			expected: "3\n3\n",
		},
		{
			name: "count_base on single-base sequences",
			code: `{{FUNC}}
print(count_base("AAAA", "A"))
print(count_base("AAAA", "T"))`,
			expected: "4\n0\n",
		},
	],
};
