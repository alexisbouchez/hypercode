import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "basics",
  content: `## Pattern Matching

Scala's \`match\` expression is like a super-powered switch. It returns a value and handles multiple patterns cleanly:

\`\`\`scala
val n = 3
val name = n match {
  case 1 => "one"
  case 2 => "two"
  case 3 => "three"
  case _ => "other"   // wildcard: matches anything
}
println(name)  // three
\`\`\`

### OR Patterns

Use \`|\` to match multiple values in one case:

\`\`\`scala
def isWeekend(day: Int): Boolean = day match {
  case 6 | 7 => true
  case _     => false
}
\`\`\`

### Guards

Add a condition with \`if\`:

\`\`\`scala
def classify(n: Int): String = n match {
  case 0         => "zero"
  case n if n > 0 => "positive"
  case _         => "negative"
}
\`\`\`

### Your Task

Write a function \`season\` that maps a month number (1–12) to its season using pattern matching:
- \`"winter"\` for months 12, 1, 2
- \`"spring"\` for months 3, 4, 5
- \`"summer"\` for months 6, 7, 8
- \`"autumn"\` for months 9, 10, 11`,

  starterCode: `def season(month: Int): String = month match {
  case 12 | 1 | 2 => "winter"
  case 3 | 4 | 5  => "spring"
  case 6 | 7 | 8  => "summer"
  case _           => "autumn"
}

println(season(1))
println(season(4))
println(season(7))
println(season(10))
`,

  solution: `def season(month: Int): String = month match {
  case 12 | 1 | 2 => "winter"
  case 3 | 4 | 5  => "spring"
  case 6 | 7 | 8  => "summer"
  case _           => "autumn"
}

println(season(1))
println(season(4))
println(season(7))
println(season(10))
`,

  tests: [
    {
      name: "January is winter",
      expected: "winter\n",
      code: `{{FUNC}}
println(season(1))
`,
    },
    {
      name: "April is spring",
      expected: "spring\n",
      code: `{{FUNC}}
println(season(4))
`,
    },
    {
      name: "July is summer",
      expected: "summer\n",
      code: `{{FUNC}}
println(season(7))
`,
    },
    {
      name: "October is autumn",
      expected: "autumn\n",
      code: `{{FUNC}}
println(season(10))
`,
    },
  ],
};
