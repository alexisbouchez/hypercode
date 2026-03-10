import type { Lesson } from "../../types";

export const positionalEncoding: Lesson = {
	id: "positional-encoding",
	title: "Positional Encoding",
	chapterId: "transformer",
	content: `## Positional Encoding

Transformers process all tokens in parallel — they have no built-in notion of order. Without positional information, "the cat sat on the mat" and "the mat sat on the cat" would produce identical representations.

### Why Position Matters

Self-attention computes a weighted sum over all tokens, treating them as a **set**. The model cannot tell which token came first, second, or last. Positional encodings inject order information into the token embeddings.

### Sinusoidal Positional Encoding

The original Transformer paper ("Attention Is All You Need") uses fixed sinusoidal functions:

\`\`\`
PE(pos, 2i)     = sin(pos / 10000^(2i/d_model))
PE(pos, 2i + 1) = cos(pos / 10000^(2i/d_model))
\`\`\`

Each dimension \`i\` oscillates at a different frequency. Low dimensions change rapidly across positions; high dimensions change slowly. This creates a unique "fingerprint" for every position.

### Key Properties

1. **Deterministic**: No learned parameters — computed once and added to embeddings
2. **Unique per position**: Every position gets a distinct vector
3. **Bounded**: Values stay in \`[-1, 1]\`, so they don't dominate the embeddings
4. **Relative positioning**: The encoding of position \`pos + k\` can be expressed as a linear function of the encoding at \`pos\`

### Implementation

\`\`\`python
import math

def positional_encoding(seq_len, d_model):
    pe = []
    for pos in range(seq_len):
        row = []
        for i in range(d_model):
            angle = pos / (10000 ** (2 * (i // 2) / d_model))
            if i % 2 == 0:
                row.append(math.sin(angle))
            else:
                row.append(math.cos(angle))
        pe.append(row)
    return pe
\`\`\`

Position 0 always produces \`[sin(0), cos(0), ...] = [0, 1, ...]\`.

### Your Task

Implement \`positional_encoding(seq_len, d_model)\` that returns a list of \`seq_len\` vectors, each of length \`d_model\`.`,

	starterCode: `import math

def positional_encoding(seq_len, d_model):
    # TODO:
    # For each position pos in [0, seq_len):
    #   For each dimension i in [0, d_model):
    #     angle = pos / (10000 ** (2 * (i // 2) / d_model))
    #     even i → sin(angle), odd i → cos(angle)
    pass

pe = positional_encoding(4, 6)
print(len(pe))
print(len(pe[0]))
print(round(pe[0][0], 4))
print(round(pe[0][1], 4))

print(round(pe[1][0], 4))
print(round(pe[1][1], 4))
print(round(pe[1][2], 4))
print(round(pe[1][3], 4))

pe2 = positional_encoding(3, 4)
dot01 = sum(pe2[0][j] * pe2[1][j] for j in range(4))
dot02 = sum(pe2[0][j] * pe2[2][j] for j in range(4))
print(round(dot01, 4))
print(round(dot02, 4))
`,

	solution: `import math

def positional_encoding(seq_len, d_model):
    pe = []
    for pos in range(seq_len):
        row = []
        for i in range(d_model):
            angle = pos / (10000 ** (2 * (i // 2) / d_model))
            if i % 2 == 0:
                row.append(math.sin(angle))
            else:
                row.append(math.cos(angle))
        pe.append(row)
    return pe

pe = positional_encoding(4, 6)
print(len(pe))
print(len(pe[0]))
print(round(pe[0][0], 4))
print(round(pe[0][1], 4))

print(round(pe[1][0], 4))
print(round(pe[1][1], 4))
print(round(pe[1][2], 4))
print(round(pe[1][3], 4))

pe2 = positional_encoding(3, 4)
dot01 = sum(pe2[0][j] * pe2[1][j] for j in range(4))
dot02 = sum(pe2[0][j] * pe2[2][j] for j in range(4))
print(round(dot01, 4))
print(round(dot02, 4))
`,

	tests: [
		{
			name: "PE shape and position-0 values",
			expected: "4\n6\n0.0\n1.0\n",
			code: `import math
{{FUNC}}
pe = positional_encoding(4, 6)
print(len(pe))
print(len(pe[0]))
print(round(pe[0][0], 4))
print(round(pe[0][1], 4))
`,
		},
		{
			name: "PE values at position 1",
			expected: "0.8415\n0.5403\n0.0464\n0.9989\n",
			code: `import math
{{FUNC}}
pe = positional_encoding(4, 6)
print(round(pe[1][0], 4))
print(round(pe[1][1], 4))
print(round(pe[1][2], 4))
print(round(pe[1][3], 4))
`,
		},
		{
			name: "Different positions produce different dot products",
			expected: "1.5403\n0.5837\n",
			code: `import math
{{FUNC}}
pe2 = positional_encoding(3, 4)
dot01 = sum(pe2[0][j] * pe2[1][j] for j in range(4))
dot02 = sum(pe2[0][j] * pe2[2][j] for j in range(4))
print(round(dot01, 4))
print(round(dot02, 4))
`,
		},
	],
};
