import type { Lesson } from "../../types";

export const translation: Lesson = {
	id: "translation",
	title: "Translation",
	chapterId: "gene-anatomy",
	content: `## RNA → Protein

The second step after transcription is **translation**: ribosomes read the mRNA codons and assemble a chain of amino acids — a **protein**.

Each 3-letter RNA codon maps to one amino acid (using standard single-letter codes):

\`\`\`python
CODON_TABLE = {
    "AUG": "M",  # Start / Methionine
    "UUU": "F", "UUC": "F",  # Phenylalanine
    "UAA": "*", "UAG": "*", "UGA": "*",  # Stop
    # ... 61 more
}
\`\`\`

Translation starts at AUG and continues until a stop codon (\`*\`) is reached:

\`\`\`python
def translate(rna):
    protein = ""
    for i in range(0, len(rna) - 2, 3):
        aa = CODON_TABLE.get(rna[i:i+3], "?")
        if aa == "*":
            break
        protein += aa
    return protein

print(translate("AUGAAAUAA"))  # MK
\`\`\`

AlphaGenome predicts **splice junction usage** — which RNA pieces get stitched together before translation — because errors in splicing cause many genetic diseases.

### Your Task

Implement \`translate(rna)\` using the provided codon table.`,

	starterCode: `def translate(rna):
    CODON_TABLE = {
        "AUG": "M",
        "UUU": "F", "UUC": "F",
        "UUA": "L", "UUG": "L",
        "UCU": "S", "UCC": "S", "UCA": "S", "UCG": "S",
        "UAU": "Y", "UAC": "Y",
        "UAA": "*", "UAG": "*",
        "UGU": "C", "UGC": "C",
        "UGA": "*",
        "CGU": "R", "CGC": "R", "CGA": "R", "CGG": "R",
        "AUU": "I", "AUC": "I", "AUA": "I",
        "ACU": "T", "ACC": "T", "ACA": "T", "ACG": "T",
        "AAU": "N", "AAC": "N",
        "AAA": "K", "AAG": "K",
        "GUU": "V", "GUC": "V", "GUA": "V", "GUG": "V",
        "GCU": "A", "GCC": "A", "GCA": "A", "GCG": "A",
        "GAU": "D", "GAC": "D",
        "GAA": "E", "GAG": "E",
        "GGU": "G", "GGC": "G", "GGA": "G", "GGG": "G",
        "UGG": "W",
        "CAU": "H", "CAC": "H",
        "CAA": "Q", "CAG": "Q",
        "CCU": "P", "CCC": "P", "CCA": "P", "CCG": "P",
    }
    # Read codons in triplets, stop at "*", return amino acid string
    pass

print(translate("AUGCGAUAA"))
print(translate("AUGUUU"))
print(translate("AUGAAAUAA"))
`,

	solution: `def translate(rna):
    CODON_TABLE = {
        "AUG": "M",
        "UUU": "F", "UUC": "F",
        "UUA": "L", "UUG": "L",
        "UCU": "S", "UCC": "S", "UCA": "S", "UCG": "S",
        "UAU": "Y", "UAC": "Y",
        "UAA": "*", "UAG": "*",
        "UGU": "C", "UGC": "C",
        "UGA": "*",
        "CGU": "R", "CGC": "R", "CGA": "R", "CGG": "R",
        "AUU": "I", "AUC": "I", "AUA": "I",
        "ACU": "T", "ACC": "T", "ACA": "T", "ACG": "T",
        "AAU": "N", "AAC": "N",
        "AAA": "K", "AAG": "K",
        "GUU": "V", "GUC": "V", "GUA": "V", "GUG": "V",
        "GCU": "A", "GCC": "A", "GCA": "A", "GCG": "A",
        "GAU": "D", "GAC": "D",
        "GAA": "E", "GAG": "E",
        "GGU": "G", "GGC": "G", "GGA": "G", "GGG": "G",
        "UGG": "W",
        "CAU": "H", "CAC": "H",
        "CAA": "Q", "CAG": "Q",
        "CCU": "P", "CCC": "P", "CCA": "P", "CCG": "P",
    }
    protein = ""
    for i in range(0, len(rna) - 2, 3):
        aa = CODON_TABLE.get(rna[i:i+3], "?")
        if aa == "*":
            break
        protein += aa
    return protein

print(translate("AUGCGAUAA"))
print(translate("AUGUUU"))
print(translate("AUGAAAUAA"))
`,

	tests: [
		{
			name: "translate AUG-CGA-UAA to MR",
			code: `{{FUNC}}
print(translate("AUGCGAUAA"))`,
			expected: "MR\n",
		},
		{
			name: "translate AUG-UUU to MF",
			code: `{{FUNC}}
print(translate("AUGUUU"))`,
			expected: "MF\n",
		},
		{
			name: "translate AUG-AAA-UAA to MK",
			code: `{{FUNC}}
print(translate("AUGAAAUAA"))`,
			expected: "MK\n",
		},
		{
			name: "translate stops at stop codon",
			code: `{{FUNC}}
print(translate("AUGUAACGA"))`,
			expected: "M\n",
		},
	],
};
