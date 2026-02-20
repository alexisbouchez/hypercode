import type { Lesson } from "../../types";

export const oneHot: Lesson = {
	id: "one-hot-encoding",
	title: "One-Hot Encoding",
	chapterId: "ml-genomics",
	content: `## Preparing DNA for Neural Networks

Neural networks work with numbers, not letters. To feed a DNA sequence into a model, we convert each base into a **one-hot vector** — a 4-element list with exactly one 1 and three 0s:

| Base | Encoding  |
|------|-----------|
| A    | [1, 0, 0, 0] |
| T    | [0, 1, 0, 0] |
| G    | [0, 0, 1, 0] |
| C    | [0, 0, 0, 1] |

A sequence of length L becomes a matrix of shape L × 4.

\`\`\`python
ONE_HOT = {
    "A": [1, 0, 0, 0],
    "T": [0, 1, 0, 0],
    "G": [0, 0, 1, 0],
    "C": [0, 0, 0, 1],
}

def one_hot_encode(seq):
    return [ONE_HOT[b] for b in seq]

def decode_one_hot(encoded):
    bases = ["A", "T", "G", "C"]
    return "".join(bases[row.index(1)] for row in encoded)

print(one_hot_encode("ATG"))  # [[1,0,0,0],[0,1,0,0],[0,0,1,0]]
print(decode_one_hot([[1,0,0,0],[0,1,0,0],[0,0,1,0]]))  # ATG
\`\`\`

AlphaGenome takes a **1 million base-pair** DNA sequence as one-hot encoded input. Its convolutional layers scan this matrix looking for short patterns — the learned analogues of the TATA box, CpG island, and splice site detectors you have been writing by hand.

### Your Task

Implement \`one_hot_encode(seq)\` and \`decode_one_hot(encoded)\`.`,

	starterCode: `def one_hot_encode(seq):
    ONE_HOT = {
        "A": [1, 0, 0, 0],
        "T": [0, 1, 0, 0],
        "G": [0, 0, 1, 0],
        "C": [0, 0, 0, 1],
    }
    # Return a list of ONE_HOT vectors, one per base
    pass

def decode_one_hot(encoded):
    # Convert list of one-hot vectors back to a DNA string
    bases = ["A", "T", "G", "C"]
    pass

encoded = one_hot_encode("ATG")
print(encoded)
print(decode_one_hot(encoded))
print(decode_one_hot(one_hot_encode("GCTA")))
`,

	solution: `def one_hot_encode(seq):
    ONE_HOT = {
        "A": [1, 0, 0, 0],
        "T": [0, 1, 0, 0],
        "G": [0, 0, 1, 0],
        "C": [0, 0, 0, 1],
    }
    return [ONE_HOT[b] for b in seq]

def decode_one_hot(encoded):
    bases = ["A", "T", "G", "C"]
    return "".join(bases[row.index(1)] for row in encoded)

encoded = one_hot_encode("ATG")
print(encoded)
print(decode_one_hot(encoded))
print(decode_one_hot(one_hot_encode("GCTA")))
`,

	tests: [
		{
			name: "one_hot_encode produces correct vectors",
			code: `{{FUNC}}
print(one_hot_encode("ATG"))`,
			expected: "[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]\n",
		},
		{
			name: "one_hot_encode for all four bases",
			code: `{{FUNC}}
print(one_hot_encode("ATGC"))`,
			expected: "[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]\n",
		},
		{
			name: "decode_one_hot recovers original sequence",
			code: `{{FUNC}}
print(decode_one_hot([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]))`,
			expected: "ATG\n",
		},
		{
			name: "encode then decode is identity",
			code: `{{FUNC}}
seq = "GCTATCGA"
print(decode_one_hot(one_hot_encode(seq)) == seq)`,
			expected: "True\n",
		},
	],
};
