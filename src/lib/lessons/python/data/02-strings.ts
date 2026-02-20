import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "Strings",
	chapterId: "foundations",
	content: `## Strings

Python strings are immutable sequences of Unicode characters. They come with a rich set of built-in methods.

### Common Methods

\`\`\`python
s = "  Hello, World!  "
s.upper()        # "  HELLO, WORLD!  "
s.lower()        # "  hello, world!  "
s.strip()        # "Hello, World!"
s.replace("World", "Python")  # "  Hello, Python!  "
s.split(", ")    # ["  Hello", "World!  "]
", ".join(["a", "b", "c"])    # "a, b, c"
\`\`\`

### Slicing

\`\`\`python
s = "Python"
s[0]     # "P"
s[-1]    # "n"
s[1:4]   # "yth"
s[::-1]  # "nohtyP"  (reversed)
\`\`\`

### Testing Content

\`\`\`python
"hello".startswith("he")  # True
"hello".endswith("lo")    # True
"ell" in "hello"          # True
"hello".isdigit()         # False
"123".isdigit()           # True
\`\`\`

### len() and count()

\`\`\`python
len("hello")          # 5
"hello".count("l")   # 2
\`\`\`

### Your Task

Implement \`is_palindrome(s)\` that:
- Lowercases and strips \`s\`
- Returns \`True\` if the string reads the same forwards and backwards, \`False\` otherwise`,

	starterCode: `def is_palindrome(s):
    # Lowercase, strip, then check if s == reversed s
    pass

print(is_palindrome("racecar"))
print(is_palindrome("hello"))
print(is_palindrome("  Madam  "))
`,

	solution: `def is_palindrome(s):
    cleaned = s.strip().lower()
    return cleaned == cleaned[::-1]

print(is_palindrome("racecar"))
print(is_palindrome("hello"))
print(is_palindrome("  Madam  "))
`,

	tests: [
		{
			name: "racecar is a palindrome",
			code: `{{FUNC}}
print(is_palindrome("racecar"))`,
			expected: "True\n",
		},
		{
			name: "hello is not a palindrome",
			code: `{{FUNC}}
print(is_palindrome("hello"))`,
			expected: "False\n",
		},
		{
			name: "case-insensitive: Madam",
			code: `{{FUNC}}
print(is_palindrome("Madam"))`,
			expected: "True\n",
		},
		{
			name: "strips whitespace: ' level '",
			code: `{{FUNC}}
print(is_palindrome("  level  "))`,
			expected: "True\n",
		},
	],
};
