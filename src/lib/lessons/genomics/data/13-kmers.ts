import type { Lesson } from "../../types";

export const kmers: Lesson = {
	id: "kmers",
	title: "K-mer Features",
	chapterId: "ml-genomics",
	content: `## Sequence Fingerprints

A **k-mer** is a DNA substring of length k. The set of all k-mers in a sequence — and their frequencies — is a powerful feature vector for machine learning.

For k=2, there are 4² = 16 possible dinucleotides. For k=6, there are 4096 possible hexamers. Many transcription factors recognize 6–10 base motifs.

\`\`\`python
def get_kmers(seq, k):
    return [seq[i:i+k] for i in range(len(seq) - k + 1)]

def kmer_frequency(seq, k):
    kmers = get_kmers(seq, k)
    freq = {}
    for kmer in kmers:
        freq[kmer] = freq.get(kmer, 0) + 1
    return freq

seq = "ATCGATCG"
print(get_kmers(seq, 3))
# ['ATC', 'TCG', 'CGA', 'GAT', 'ATC', 'TCG']
\`\`\`

K-mer frequencies were one of the earliest ML features for genomics. AlphaGenome's transformer architecture can be thought of as a vastly more powerful version: it learns to detect not just fixed k-mers but arbitrary long-range combinations of sequence patterns.

### Your Task

Implement \`get_kmers(seq, k)\` and \`kmer_frequency(seq, k)\`.`,

	starterCode: `def get_kmers(seq, k):
    # Return a list of all k-length substrings of seq
    pass

def kmer_frequency(seq, k):
    # Return a dict mapping each kmer to its count
    pass

seq = "ATCGATCG"
print(get_kmers(seq, 3))

freq = kmer_frequency(seq, 2)
print(sorted(freq.items()))
`,

	solution: `def get_kmers(seq, k):
    return [seq[i:i+k] for i in range(len(seq) - k + 1)]

def kmer_frequency(seq, k):
    kmers = get_kmers(seq, k)
    freq = {}
    for kmer in kmers:
        freq[kmer] = freq.get(kmer, 0) + 1
    return freq

seq = "ATCGATCG"
print(get_kmers(seq, 3))

freq = kmer_frequency(seq, 2)
print(sorted(freq.items()))
`,

	tests: [
		{
			name: "get_kmers extracts all 3-mers from ATCGATCG",
			code: `{{FUNC}}
print(get_kmers("ATCGATCG", 3))`,
			expected: "['ATC', 'TCG', 'CGA', 'GAT', 'ATC', 'TCG']\n",
		},
		{
			name: "get_kmers extracts 2-mers",
			code: `{{FUNC}}
print(get_kmers("ATCG", 2))`,
			expected: "['AT', 'TC', 'CG']\n",
		},
		{
			name: "kmer_frequency counts correctly",
			code: `{{FUNC}}
freq = kmer_frequency("ATCGATCG", 2)
print(sorted(freq.items()))`,
			expected: "[('AT', 2), ('CG', 2), ('GA', 1), ('TC', 2)]\n",
		},
		{
			name: "kmer_frequency on a repetitive sequence",
			code: `{{FUNC}}
freq = kmer_frequency("AAAA", 2)
print(sorted(freq.items()))`,
			expected: "[('AA', 3)]\n",
		},
	],
};
