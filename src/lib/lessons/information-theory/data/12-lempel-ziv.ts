import type { Lesson } from "../../types";

export const lempelZiv: Lesson = {
	id: "lempel-ziv",
	title: "LZ Complexity and Run-Length Encoding",
	chapterId: "coding",
	content: `## Lempel-Ziv Complexity

**LZ complexity** (LZ78 variant) measures the number of distinct phrases in the LZ78 parsing of a string. It quantifies the **structural complexity** of a sequence.

### LZ78 Parsing Algorithm

Parse the string left to right, building up a phrase one character at a time:
1. Start with an empty phrase and an empty dictionary
2. Add the next character to the current phrase
3. If the phrase is **not** in the dictionary: add it, increment the count, reset phrase to empty
4. If the phrase **is** in the dictionary: keep extending

After the loop, any leftover phrase that wasn't added counts as one more phrase.

\`\`\`python
def lz_complexity(s):
    if not s:
        return 0
    phrases = set()
    phrase = ""
    c = 0
    for ch in s:
        phrase += ch
        if phrase not in phrases:
            phrases.add(phrase)
            c += 1
            phrase = ""
    if phrase:
        c += 1
    return c
\`\`\`

### Trace Example: \`'aababc'\`

| Step | char | phrase | in dict? | action |
|------|------|--------|----------|--------|
| 0 | a | 'a' | no | add 'a', c=1 |
| 1 | a | 'a' | yes | extend |
| 2 | b | 'ab' | no | add 'ab', c=2 |
| 3 | a | 'a' | yes | extend |
| 4 | b | 'ab' | yes | extend |
| 5 | c | 'abc' | no | add 'abc', c=3 |

Result: **3 phrases**

## Run-Length Encoding

**RLE** compresses repeated characters by storing (character, count) pairs:

$$\\texttt{aaabbc} \\to [(\\texttt{a}, 3), (\\texttt{b}, 2), (\\texttt{c}, 1)]$$

### Your Task

Implement:
- \`lz_complexity(s)\` — LZ78 phrase count
- \`run_length_encode(s)\` — list of \`(char, count)\` tuples`,

	starterCode: `def lz_complexity(s):
    # LZ78 parsing: count distinct new phrases
    # Return 0 for empty string
    pass

def run_length_encode(s):
    # Return list of (char, count) tuples
    pass

print(lz_complexity('aababc'))
print(run_length_encode('aaabbc'))
`,

	solution: `def lz_complexity(s):
    if not s:
        return 0
    phrases = set()
    phrase = ""
    c = 0
    for ch in s:
        phrase += ch
        if phrase not in phrases:
            phrases.add(phrase)
            c += 1
            phrase = ""
    if phrase:
        c += 1
    return c

def run_length_encode(s):
    if not s:
        return []
    result = []
    current = s[0]
    count = 1
    for ch in s[1:]:
        if ch == current:
            count += 1
        else:
            result.append((current, count))
            current = ch
            count = 1
    result.append((current, count))
    return result

print(lz_complexity('aababc'))
print(run_length_encode('aaabbc'))
`,

	tests: [
		{
			name: "lz_complexity('aababc')=3, run_length_encode('aaabbc')=[('a',3),('b',2),('c',1)]",
			expected: "3\n[('a', 3), ('b', 2), ('c', 1)]\n",
		},
		{
			name: "lz_complexity('abcd') = 4 (all different, each new phrase)",
			code: `{{FUNC}}
print(lz_complexity('abcd'))`,
			expected: "4\n",
		},
		{
			name: "lz_complexity('aaaa') = 3",
			code: `{{FUNC}}
print(lz_complexity('aaaa'))`,
			expected: "3\n",
		},
		{
			name: "run_length_encode('abcd') = [('a',1),('b',1),('c',1),('d',1)]",
			code: `{{FUNC}}
print(run_length_encode('abcd'))`,
			expected: "[('a', 1), ('b', 1), ('c', 1), ('d', 1)]\n",
		},
		{
			name: "lz_complexity('ab') = 2",
			code: `{{FUNC}}
print(lz_complexity('ab'))`,
			expected: "2\n",
		},
	],
};
