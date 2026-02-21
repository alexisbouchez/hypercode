import type { Lesson } from "../../types";

export const tokenizer: Lesson = {
	id: "tokenizer",
	title: "Character Tokenizer",
	chapterId: "tokens",
	content: `## Tokenization

Neural networks process numbers, not strings. A **tokenizer** maps characters to integer IDs and back.

### Character-Level Tokenizer

The simplest tokenizer assigns an integer to each unique character:

\`\`\`python
text = "hello"
chars = sorted(set(text))   # ['e', 'h', 'l', 'o']
encode = {ch: i for i, ch in enumerate(chars)}
decode = {i: ch for i, ch in enumerate(chars)}
\`\`\`

The sort ensures a consistent, reproducible mapping.

### The BOS Token

We need a special **Beginning-Of-Sequence** token to mark where sequences start and end. We place it at index \`len(chars)\` — just after all the character tokens:

\`\`\`python
BOS = len(chars)        # e.g. 4 for "hello"
vocab_size = len(chars) + 1
\`\`\`

The same BOS token serves as both start and end marker. A training sequence for "emma" would be:

\`\`\`
[BOS, e, m, m, a, BOS]
\`\`\`

The model learns to predict the next token at every position, including predicting BOS after the last character.

### Encoding and Decoding

\`\`\`python
tokens = [encode[ch] for ch in "hello"]  # [1, 0, 2, 2, 3]
decoded = ''.join(decode[t] for t in tokens)  # "hello"
\`\`\`

### Your Task

Implement \`build_tokenizer(text)\` that returns \`(BOS, vocab_size, encode, decode)\`.`,

	starterCode: `def build_tokenizer(text):
    # TODO:
    # 1. Get sorted unique characters from text
    # 2. Create encode dict: char → index
    # 3. Create decode dict: index → char
    # 4. Set BOS = len(chars), vocab_size = len(chars) + 1
    # 5. Return (BOS, vocab_size, encode, decode)
    pass

BOS, vocab_size, encode, decode = build_tokenizer("hello")
print(vocab_size)

tokens = [encode[ch] for ch in "hello"]
print(tokens)

decoded = ''.join(decode[t] for t in tokens)
print(decoded)
`,

	solution: `def build_tokenizer(text):
    chars = sorted(set(text))
    BOS = len(chars)
    vocab_size = len(chars) + 1
    encode = {ch: i for i, ch in enumerate(chars)}
    decode = {i: ch for i, ch in enumerate(chars)}
    return BOS, vocab_size, encode, decode

BOS, vocab_size, encode, decode = build_tokenizer("hello")
print(vocab_size)

tokens = [encode[ch] for ch in "hello"]
print(tokens)

decoded = ''.join(decode[t] for t in tokens)
print(decoded)
`,

	tests: [
		{
			name: "vocab_size=5, hello encodes and decodes correctly",
			expected: "5\n[1, 0, 2, 2, 3]\nhello\n",
		},
	],
};
