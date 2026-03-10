import type { Lesson } from "../../types";

export const bpeTokenization: Lesson = {
	id: "bpe-tokenization",
	title: "BPE Tokenization",
	chapterId: "tokens",
	content: `## Byte Pair Encoding (BPE)

Character-level tokenization produces long sequences and a tiny vocabulary. Real LLMs use **Byte Pair Encoding** (BPE), which iteratively merges the most frequent pair of tokens into a new token.

### The Algorithm

Starting from raw UTF-8 bytes:

1. Count every adjacent pair of tokens
2. Merge the most frequent pair into a new token ID
3. Repeat for \`num_merges\` steps

\`\`\`python
tokens = list(text.encode("utf-8"))     # start with raw bytes
merges = {}                              # (pair) → new_id
vocab  = {i: bytes([i]) for i in range(256)}
next_id = 256

for _ in range(num_merges):
    pairs = {}
    for i in range(len(tokens) - 1):
        pair = (tokens[i], tokens[i+1])
        pairs[pair] = pairs.get(pair, 0) + 1
    best = max(pairs, key=pairs.get)
    merges[best] = next_id
    vocab[next_id] = vocab[best[0]] + vocab[best[1]]
    # replace all occurrences of best pair
    ...
    next_id += 1
\`\`\`

### Encoding New Text

To encode unseen text, start from raw bytes and repeatedly apply merges **in the order they were learned** (lowest ID first):

\`\`\`python
tokens = list(text.encode("utf-8"))
while True:
    # find the pair with the smallest merge ID
    best = min(applicable_pairs, key=lambda p: merges[p])
    # merge all occurrences
    ...
\`\`\`

### Decoding

Decoding is simple — look up each token ID in the vocab and concatenate:

\`\`\`python
def bpe_decode(tokens, vocab):
    return b"".join(vocab[t] for t in tokens).decode("utf-8")
\`\`\`

### Your Task

Implement \`bpe_train(text, num_merges)\` returning \`(merges, vocab)\`, \`bpe_encode(text, merges)\`, and \`bpe_decode(tokens, vocab)\`.`,

	starterCode: `def bpe_train(text, num_merges):
    tokens = list(text.encode("utf-8"))
    merges = {}
    vocab = {i: bytes([i]) for i in range(256)}
    next_id = 256
    for _ in range(num_merges):
        # TODO:
        # 1. Count adjacent pairs
        # 2. Find the most frequent pair
        # 3. Record merge and update vocab
        # 4. Replace all occurrences of the pair in tokens
        pass
    return merges, vocab

def bpe_encode(text, merges):
    tokens = list(text.encode("utf-8"))
    # TODO: repeatedly apply the earliest merge
    return tokens

def bpe_decode(tokens, vocab):
    # TODO: concatenate vocab entries and decode
    return ""

merges, vocab = bpe_train("aaabdaaabac", 3)
print(len(merges))
print(sorted(merges.values()))

encoded = bpe_encode("aaabdaaabac", merges)
print(len(encoded))
decoded = bpe_decode(encoded, vocab)
print(decoded)

merges2, vocab2 = bpe_train("abababab", 2)
encoded2 = bpe_encode("abab", merges2)
print(len(encoded2))
decoded2 = bpe_decode(encoded2, vocab2)
print(decoded2)
`,

	solution: `def bpe_train(text, num_merges):
    tokens = list(text.encode("utf-8"))
    merges = {}
    vocab = {i: bytes([i]) for i in range(256)}
    next_id = 256
    for _ in range(num_merges):
        pairs = {}
        for i in range(len(tokens) - 1):
            pair = (tokens[i], tokens[i+1])
            pairs[pair] = pairs.get(pair, 0) + 1
        if not pairs:
            break
        best = max(pairs, key=pairs.get)
        merges[best] = next_id
        vocab[next_id] = vocab[best[0]] + vocab[best[1]]
        new_tokens = []
        i = 0
        while i < len(tokens):
            if i < len(tokens) - 1 and (tokens[i], tokens[i+1]) == best:
                new_tokens.append(next_id)
                i += 2
            else:
                new_tokens.append(tokens[i])
                i += 1
        tokens = new_tokens
        next_id += 1
    return merges, vocab

def bpe_encode(text, merges):
    tokens = list(text.encode("utf-8"))
    while True:
        pairs = {}
        for i in range(len(tokens) - 1):
            pair = (tokens[i], tokens[i+1])
            if pair in merges and pair not in pairs:
                pairs[pair] = merges[pair]
        if not pairs:
            break
        best = min(pairs, key=lambda p: merges[p])
        new_tokens = []
        i = 0
        while i < len(tokens):
            if i < len(tokens) - 1 and (tokens[i], tokens[i+1]) == best:
                new_tokens.append(merges[best])
                i += 2
            else:
                new_tokens.append(tokens[i])
                i += 1
        tokens = new_tokens
    return tokens

def bpe_decode(tokens, vocab):
    return b"".join(vocab[t] for t in tokens).decode("utf-8")

merges, vocab = bpe_train("aaabdaaabac", 3)
print(len(merges))
print(sorted(merges.values()))

encoded = bpe_encode("aaabdaaabac", merges)
print(len(encoded))
decoded = bpe_decode(encoded, vocab)
print(decoded)

merges2, vocab2 = bpe_train("abababab", 2)
encoded2 = bpe_encode("abab", merges2)
print(len(encoded2))
decoded2 = bpe_decode(encoded2, vocab2)
print(decoded2)
`,

	tests: [
		{
			name: "BPE training produces 3 merges",
			expected: "3\n[256, 257, 258]\n",
			code: `{{FUNC}}
merges, vocab = bpe_train("aaabdaaabac", 3)
print(len(merges))
print(sorted(merges.values()))
`,
		},
		{
			name: "BPE encode/decode roundtrip",
			expected: "5\naaabdaaabac\n",
			code: `{{FUNC}}
merges, vocab = bpe_train("aaabdaaabac", 3)
encoded = bpe_encode("aaabdaaabac", merges)
print(len(encoded))
decoded = bpe_decode(encoded, vocab)
print(decoded)
`,
		},
		{
			name: "BPE on repeated pattern",
			expected: "1\nabab\n",
			code: `{{FUNC}}
merges2, vocab2 = bpe_train("abababab", 2)
encoded2 = bpe_encode("abab", merges2)
print(len(encoded2))
decoded2 = bpe_decode(encoded2, vocab2)
print(decoded2)
`,
		},
	],
};
