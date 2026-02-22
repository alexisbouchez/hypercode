import type { Lesson } from "../../types";

export const when_: Lesson = {
  id: "when",
  title: "When Expressions",
  chapterId: "control-flow",
  content: `## when in Kotlin

\`when\` is Kotlin's pattern-matching construct — like a switch statement, but more powerful. It's also an expression that returns a value:

\`\`\`kotlin
val day = when (n) {
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    else -> "Other"
}
\`\`\`

Each branch uses \`->\` to separate the pattern from the result. The \`else\` branch is required when \`when\` is used as an expression.

You can match multiple values in one branch:

\`\`\`kotlin
val type = when (n) {
    1, 2, 3 -> "small"
    else -> "large"
}
\`\`\`

## Your Turn

Write a function \`season(month: Int): String\` that returns the season for a given month number (1–12). Use: Dec/Jan/Feb → \`"Winter"\`, Mar/Apr/May → \`"Spring"\`, Jun/Jul/Aug → \`"Summer"\`, Sep/Oct/Nov → \`"Autumn"\`.
`,
  starterCode: `fun season(month: Int): String {
    val result = when (month) {
        12 -> "Winter"
        1 -> "Winter"
        2 -> "Winter"
        3 -> "Spring"
        4 -> "Spring"
        5 -> "Spring"
        6 -> "Summer"
        7 -> "Summer"
        8 -> "Summer"
        else -> "Autumn"
    }
    return result
}

fun main() {
    println(season(1))
    println(season(6))
    println(season(10))
}
`,
  solution: `fun season(month: Int): String {
    val result = when (month) {
        12 -> "Winter"
        1 -> "Winter"
        2 -> "Winter"
        3 -> "Spring"
        4 -> "Spring"
        5 -> "Spring"
        6 -> "Summer"
        7 -> "Summer"
        8 -> "Summer"
        else -> "Autumn"
    }
    return result
}

fun main() {
    println(season(1))
    println(season(6))
    println(season(10))
}
`,
  tests: [
    {
      name: "season(1) = Winter",
      code: "{{FUNC}}\nfun main() {\n    println(season(1))\n}",
      expected: "Winter\n",
    },
    {
      name: "season(6) = Summer",
      code: "{{FUNC}}\nfun main() {\n    println(season(6))\n}",
      expected: "Summer\n",
    },
    {
      name: "season(10) = Autumn",
      code: "{{FUNC}}\nfun main() {\n    println(season(10))\n}",
      expected: "Autumn\n",
    },
  ],
};
