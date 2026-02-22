import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "control-flow",
  content: `## if / else in Kotlin

Kotlin uses \`if\`, \`else if\`, and \`else\` for branching:

\`\`\`kotlin
if (score >= 90) {
    println("A")
} else if (score >= 80) {
    println("B")
} else {
    println("F")
}
\`\`\`

Unlike many languages, \`if\` in Kotlin is an **expression** — it returns a value:

\`\`\`kotlin
val grade = if (score >= 90) "A" else "B"
\`\`\`

## Your Turn

Write a function \`sign(n: Int): String\` that returns \`"positive"\` if \`n > 0\`, \`"negative"\` if \`n < 0\`, and \`"zero"\` otherwise.
`,
  starterCode: `fun sign(n: Int): String {
    if (n > 0) {
        return "positive"
    } else if (n < 0) {
        return "negative"
    } else {
        return "zero"
    }
}

fun main() {
    println(sign(5))
    println(sign(-3))
    println(sign(0))
}
`,
  solution: `fun sign(n: Int): String {
    if (n > 0) {
        return "positive"
    } else if (n < 0) {
        return "negative"
    } else {
        return "zero"
    }
}

fun main() {
    println(sign(5))
    println(sign(-3))
    println(sign(0))
}
`,
  tests: [
    {
      name: "sign(5) = positive",
      code: "{{FUNC}}\nfun main() {\n    println(sign(5))\n}",
      expected: "positive\n",
    },
    {
      name: "sign(-3) = negative",
      code: "{{FUNC}}\nfun main() {\n    println(sign(-3))\n}",
      expected: "negative\n",
    },
    {
      name: "sign(0) = zero",
      code: "{{FUNC}}\nfun main() {\n    println(sign(0))\n}",
      expected: "zero\n",
    },
  ],
};
