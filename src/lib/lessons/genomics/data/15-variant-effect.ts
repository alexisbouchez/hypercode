import type { Lesson } from "../../types";

export const variantEffect: Lesson = {
	id: "variant-effect",
	title: "Variant Effect Prediction",
	chapterId: "ml-genomics",
	content: `## This is What AlphaGenome Does

You have now built all the conceptual pieces of AlphaGenome:

| Concept | AlphaGenome equivalent |
|---------|----------------------|
| DNA sequence | 1 million bp one-hot encoded input |
| GC windows / motif finding | Convolutional layers |
| Long-range context | Transformer layers |
| Regulatory scoring | Multi-task output heads (expression, accessibility, histone marks…) |
| Variant = apply_variant | Compare ref vs alt predictions |

**Variant effect prediction** is the core use-case: given a reference sequence and a variant (position + alternative base), compute a **delta score** — the predicted change in regulatory activity.

\`\`\`python
def regulatory_score(seq):
    score = 0
    score += seq.count("TATA")  * 10
    score += seq.count("CACCC") * 5
    score += seq.count("CG")    * 2
    return score

def variant_effect(ref_seq, pos, alt_base):
    alt_seq = ref_seq[:pos] + alt_base + ref_seq[pos+1:]
    return regulatory_score(alt_seq) - regulatory_score(ref_seq)
\`\`\`

A **positive** delta means the variant increases regulatory activity (**activating**). A **negative** delta means it reduces it (**repressive**). Zero means **neutral**.

\`\`\`python
ref = "AATACGCG"     # score = 4  (2 CpGs)
print(variant_effect(ref, 0, "T"))   # T at pos 0 creates TATA → +10
\`\`\`

AlphaGenome runs this comparison not for one score but for thousands of molecular outputs simultaneously — gene expression in each tissue, chromatin accessibility, histone marks, and more.

### Your Task

Implement \`variant_effect(ref_seq, pos, alt_base)\` that returns the score delta, and \`classify_variant(delta)\` that returns \`"activating"\`, \`"repressive"\`, or \`"neutral"\`.`,

	starterCode: `def regulatory_score(seq):
    score = 0
    score += seq.count("TATA")  * 10
    score += seq.count("CACCC") * 5
    score += seq.count("CG")    * 2
    return score

def variant_effect(ref_seq, pos, alt_base):
    # Compute regulatory_score(alt) - regulatory_score(ref)
    pass

def classify_variant(delta):
    # Return "activating" if delta > 0, "repressive" if < 0, else "neutral"
    pass

ref = "AATACGCG"
print(regulatory_score(ref))

delta = variant_effect(ref, 0, "T")
print(delta)
print(classify_variant(delta))

delta2 = variant_effect(ref, 4, "A")
print(delta2)
print(classify_variant(delta2))
`,

	solution: `def regulatory_score(seq):
    score = 0
    score += seq.count("TATA")  * 10
    score += seq.count("CACCC") * 5
    score += seq.count("CG")    * 2
    return score

def variant_effect(ref_seq, pos, alt_base):
    alt_seq = ref_seq[:pos] + alt_base + ref_seq[pos+1:]
    return regulatory_score(alt_seq) - regulatory_score(ref_seq)

def classify_variant(delta):
    if delta > 0:
        return "activating"
    elif delta < 0:
        return "repressive"
    else:
        return "neutral"

ref = "AATACGCG"
print(regulatory_score(ref))

delta = variant_effect(ref, 0, "T")
print(delta)
print(classify_variant(delta))

delta2 = variant_effect(ref, 4, "A")
print(delta2)
print(classify_variant(delta2))
`,

	tests: [
		{
			name: "regulatory_score of AATACGCG is 4",
			code: `{{FUNC}}
print(regulatory_score("AATACGCG"))`,
			expected: "4\n",
		},
		{
			name: "variant creating TATA box is activating (+10)",
			code: `{{FUNC}}
print(variant_effect("AATACGCG", 0, "T"))`,
			expected: "10\n",
		},
		{
			name: "variant destroying CpG is repressive (-2)",
			code: `{{FUNC}}
print(variant_effect("AATACGCG", 4, "A"))`,
			expected: "-2\n",
		},
		{
			name: "classify_variant labels correctly",
			code: `{{FUNC}}
print(classify_variant(10))
print(classify_variant(-2))
print(classify_variant(0))`,
			expected: "activating\nrepressive\nneutral\n",
		},
	],
};
