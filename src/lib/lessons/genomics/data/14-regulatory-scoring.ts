import type { Lesson } from "../../types";

export const regulatoryScoring: Lesson = {
	id: "regulatory-scoring",
	title: "Regulatory Scoring",
	chapterId: "ml-genomics",
	content: `## Predicting Regulatory Activity

Regulatory sequences control gene expression. We can build a **scoring function** that estimates a region's regulatory activity based on the sequence features we have learned:

- **TATA box** (motif TATA): core promoter element, recruits transcription
- **GC box** (motif CACCC): Sp1 transcription factor binding site
- **CpG dinucleotides** (CG): marks of active gene promoters

\`\`\`python
def regulatory_score(seq):
    score = 0
    score += seq.count("TATA")  * 10  # TATA box
    score += seq.count("CACCC") * 5   # GC box
    score += seq.count("CG")    * 2   # CpG dinucleotides
    return score
\`\`\`

This is a toy model, but the idea is identical to what AlphaGenome does — except AlphaGenome learns its scoring weights from millions of experimental measurements across hundreds of cell types, capturing patterns far too subtle for humans to write by hand.

### Your Task

Implement \`regulatory_score(seq)\` using the scoring rules above.`,

	starterCode: `def regulatory_score(seq):
    # score += count("TATA")  * 10
    # score += count("CACCC") * 5
    # score += count("CG")    * 2
    pass

print(regulatory_score("TATAAAAATATA"))   # 2 TATA boxes = 20
print(regulatory_score("CACACCCGCG"))     # 1 CACCC + 2 CG = 9
print(regulatory_score("ATGATGATG"))      # no features = 0
print(regulatory_score("TATACGCACCC"))    # 1 TATA + 1 CG + 1 CACCC = 17
`,

	solution: `def regulatory_score(seq):
    score = 0
    score += seq.count("TATA")  * 10
    score += seq.count("CACCC") * 5
    score += seq.count("CG")    * 2
    return score

print(regulatory_score("TATAAAAATATA"))
print(regulatory_score("CACACCCGCG"))
print(regulatory_score("ATGATGATG"))
print(regulatory_score("TATACGCACCC"))
`,

	tests: [
		{
			name: "two TATA boxes score 20",
			code: `{{FUNC}}
print(regulatory_score("TATAAAAATATA"))`,
			expected: "20\n",
		},
		{
			name: "CACCC + two CG = 9",
			code: `{{FUNC}}
print(regulatory_score("CACACCCGCG"))`,
			expected: "9\n",
		},
		{
			name: "no features scores 0",
			code: `{{FUNC}}
print(regulatory_score("ATGATGATG"))`,
			expected: "0\n",
		},
		{
			name: "TATA + CG + CACCC combined score",
			code: `{{FUNC}}
print(regulatory_score("TATACGCACCC"))`,
			expected: "17\n",
		},
	],
};
