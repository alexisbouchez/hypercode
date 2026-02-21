import type { Lesson } from "../../types";

export const trainingPairs: Lesson = {
	id: "training-pairs",
	title: "Token Sequences",
	chapterId: "tokens",
	content: `## Training Data: Next-Token Prediction

A language model learns by predicting the next token in a sequence. For each position in a document, the input is the current token and the target is the next token.

### Wrapping with BOS

We wrap each word with BOS on both sides:

\`\`\`
"emma" → [BOS, e, m, m, a, BOS]
\`\`\`

This lets the model learn:
- Given BOS, predict 'e' (the first character)
- Given 'a', predict BOS (the end of word)

### Sliding Window

From tokens \`[t0, t1, t2, ..., tn]\`, we produce pairs:
- \`(t0, t1)\`
- \`(t1, t2)\`
- ...
- \`(tn-1, tn)\`

In Python:
\`\`\`python
def make_pairs(word, encode, BOS):
    tokens = [BOS] + [encode[ch] for ch in word] + [BOS]
    return [(tokens[i], tokens[i+1]) for i in range(len(tokens)-1)]
\`\`\`

### Example

For "emma" with \`encode = {'a':0, 'e':1, 'm':2}\` and \`BOS=3\`:

| Input | Target | Meaning |
|-------|--------|---------|
| 3 | 1 | BOS → e |
| 1 | 2 | e → m |
| 2 | 2 | m → m |
| 2 | 0 | m → a |
| 0 | 3 | a → BOS |

### Your Task

Implement \`make_pairs(word, encode, BOS)\` that returns a list of \`(input, target)\` integer tuples.`,

	starterCode: `def make_pairs(word, encode, BOS):
    # TODO:
    # 1. Build tokens = [BOS] + encoded characters + [BOS]
    # 2. Return list of (tokens[i], tokens[i+1]) for each position
    pass

chars = sorted(set("emma"))   # ['a', 'e', 'm']
BOS = len(chars)               # 3
encode = {ch: i for i, ch in enumerate(chars)}

pairs = make_pairs("emma", encode, BOS)
for inp, tgt in pairs:
    print(inp, tgt)
`,

	solution: `def make_pairs(word, encode, BOS):
    tokens = [BOS] + [encode[ch] for ch in word] + [BOS]
    return [(tokens[i], tokens[i+1]) for i in range(len(tokens)-1)]

chars = sorted(set("emma"))
BOS = len(chars)
encode = {ch: i for i, ch in enumerate(chars)}

pairs = make_pairs("emma", encode, BOS)
for inp, tgt in pairs:
    print(inp, tgt)
`,

	tests: [
		{
			name: "emma produces 5 correct (input, target) pairs",
			expected: "3 1\n1 2\n2 2\n2 0\n0 3\n",
		},
	],
};
