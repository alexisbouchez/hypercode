import type { Lesson } from "../../types";

export const wcW: Lesson = {
	id: "wc-w",
	title: "wc -w",
	chapterId: "counting",
	content: `## wc -w: Count Words

\`wc -w\` counts the number of words. A word is a sequence of non-whitespace characters separated by spaces, tabs, or newlines:

\`\`\`bash
$ echo "hello world foo" | wc -w
3
\`\`\`

### Your Implementation

Write \`int count_words(const char *s)\` that counts the words using a **state machine**: track whether you are currently inside a word or between words.

\`\`\`c
int count_words(const char *s) {
    int n = 0;
    int in_word = 0;

    while (*s) {
        if (*s == ' ' || *s == '\\t' || *s == '\\n') {
            in_word = 0;
        } else if (!in_word) {
            in_word = 1;
            n++;    // first character of a new word
        }
        s++;
    }
    return n;
}
\`\`\`

The key insight: you increment the counter **once per word**, at the moment you transition from "not in word" to "in word". Without the \`in_word\` flag, you would count each character.

### State Machines in C

This two-state model (in\_word / not\_in\_word) is a classic example of a **finite state machine** implemented with a boolean flag. Many text processing problems in C follow this pattern:

- \`wc -w\`: count transitions into "word" state
- Tokenizers: accumulate characters while in "token" state
- Parsers: track whether you are inside a string, comment, etc.

### Your Task

Implement \`count_words\` using the state machine approach.`,

	starterCode: `#include <stdio.h>

int count_words(const char *s) {
\t// Count words using a state machine
\treturn 0;
}

int main() {
\tprintf("%d\\n", count_words("hello world foo\\n"));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int count_words(const char *s) {
\tint n = 0;
\tint in_word = 0;

\twhile (*s) {
\t\tif (*s == ' ' || *s == '\\t' || *s == '\\n') {
\t\t\tin_word = 0;
\t\t} else if (!in_word) {
\t\t\tin_word = 1;
\t\t\tn++;
\t\t}
\t\ts++;
\t}
\treturn n;
}

int main() {
\tprintf("%d\\n", count_words("hello world foo\\n"));
\treturn 0;
}
`,

	tests: [
		{
			name: "count_words with 3 words",
			expected: "3\n",
		},
		{
			name: "count_words(\"\") = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_words(""));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "count_words with 1 word",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_words("linux\\n"));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "count_words with extra spaces",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_words("  one   two  three  \\n"));
\treturn 0;
}`,
			expected: "3\n",
		},
	],
};
