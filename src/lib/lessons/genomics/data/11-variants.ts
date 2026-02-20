import type { Lesson } from "../../types";

export const variants: Lesson = {
	id: "variants",
	title: "Variants",
	chapterId: "regulation",
	content: `## When One Letter Changes Everything

A **single nucleotide variant** (SNV, also called SNP) is a position in the genome where individuals differ by a single DNA letter. The human genome contains roughly **4–5 million** such differences between any two people.

Most variants are neutral — they occur in non-functional regions. But some variants, particularly those in regulatory elements or splice sites, can disrupt gene regulation and cause disease.

\`\`\`python
def apply_variant(seq, position, alt):
    """Replace the base at position with alt."""
    return seq[:position] + alt + seq[position+1:]

def find_differences(ref, alt_seq):
    """Return positions where ref and alt_seq differ."""
    return [i for i in range(min(len(ref), len(alt_seq)))
            if ref[i] != alt_seq[i]]

ref = "ATCGATCG"
mutant = apply_variant(ref, 3, "T")  # G→T at position 3
print(mutant)                          # ATCTATCG
print(find_differences(ref, mutant))   # [3]
\`\`\`

This is precisely what AlphaGenome does at scale: given a reference sequence and a variant (position + alternative base), it runs two predictions — one for the reference, one for the mutant — and reports the **delta**. That delta is the variant's predicted effect on gene regulation.

### Your Task

Implement \`apply_variant(seq, position, alt)\` and \`find_differences(ref, alt_seq)\`.`,

	starterCode: `def apply_variant(seq, position, alt):
    # Replace seq[position] with alt
    pass

def find_differences(ref, alt_seq):
    # Return list of positions where the two sequences differ
    pass

ref = "ATCGATCG"
mutant = apply_variant(ref, 3, "T")
print(mutant)
print(find_differences(ref, mutant))

ref2 = "GCGCGCGC"
mutant2 = apply_variant(ref2, 0, "A")
print(mutant2)
print(find_differences(ref2, mutant2))
`,

	solution: `def apply_variant(seq, position, alt):
    return seq[:position] + alt + seq[position+1:]

def find_differences(ref, alt_seq):
    return [i for i in range(min(len(ref), len(alt_seq)))
            if ref[i] != alt_seq[i]]

ref = "ATCGATCG"
mutant = apply_variant(ref, 3, "T")
print(mutant)
print(find_differences(ref, mutant))

ref2 = "GCGCGCGC"
mutant2 = apply_variant(ref2, 0, "A")
print(mutant2)
print(find_differences(ref2, mutant2))
`,

	tests: [
		{
			name: "apply_variant replaces single base",
			code: `{{FUNC}}
print(apply_variant("ATCGATCG", 3, "T"))`,
			expected: "ATCTATCG\n",
		},
		{
			name: "apply_variant at position 0",
			code: `{{FUNC}}
print(apply_variant("GCGCGCGC", 0, "A"))`,
			expected: "ACGCGCGC\n",
		},
		{
			name: "find_differences finds single mutation",
			code: `{{FUNC}}
print(find_differences("ATCGATCG", "ATCTATCG"))`,
			expected: "[3]\n",
		},
		{
			name: "find_differences returns empty list for identical sequences",
			code: `{{FUNC}}
print(find_differences("ATCG", "ATCG"))`,
			expected: "[]\n",
		},
	],
};
