import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "shell-scripting",
  content: `## Shell Variables

The shell lets you store values in variables. You can then use those values in commands.

### Assigning a Variable

\`\`\`bash
NAME="Linux"
\`\`\`

- No spaces around \`=\`. Spaces would break the assignment.
- No \`$\` when assigning — only when reading.

### Reading a Variable

Prefix the variable name with \`$\` to expand it:

\`\`\`bash
NAME="Linux"
echo "Hello, $NAME!"
\`\`\`

Output:
\`\`\`
Hello, Linux!
\`\`\`

### Quoting Rules

| Quotes | Variable expansion |
|--------|-------------------|
| Double quotes \`"..."\` | \`$VAR\` is expanded inside |
| Single quotes \`'...'\` | No expansion — everything is literal |

\`\`\`bash
NAME="World"
echo "Hello, $NAME"    # Hello, World
echo 'Hello, $NAME'    # Hello, $NAME
\`\`\`

### Braces for Disambiguation

When the variable name could be confused with surrounding text, use \`\${VAR}\`:

\`\`\`bash
VERSION="2"
echo "v\${VERSION}.0"    # v2.0
\`\`\`

Without braces, the shell would try to expand \`$VERSION\` followed by the literal \`.0\` — which happens to work here, but \`\${VERSION}\` makes it explicit.

### Environment Variables

Some variables are set by the shell and available to all programs:
- \`$HOME\` — your home directory (\`/home/user\`)
- \`$USER\` — your username (\`user\`)
- \`$PATH\` — directories searched for executable commands

### Your Task

Assign the value \`Linux\` to a variable called \`NAME\`, then use \`echo\` to print \`Hello, Linux!\`.`,

  starterCode: `# Assign "Linux" to NAME and echo "Hello, $NAME!"
`,

  solution: `NAME="Linux"
echo "Hello, $NAME!"
`,

  tests: [
    {
      name: "uses variable in echo",
      expected: "Hello, Linux!\n",
    },
  ],
};
