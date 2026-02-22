import type { Lesson } from "../../types";

export const case_: Lesson = {
  id: "case",
  title: "Case Expressions",
  chapterId: "control-flow",
  content: `## case

The \`case\` expression matches a value against a series of patterns:

\`\`\`elixir
n = 2
result = case n do
  1 -> "one"
  2 -> "two"
  _ -> "other"
end
IO.puts(result)   # two
\`\`\`

The \`_\` wildcard matches anything.

## Matching Atoms

\`case\` works great with atom results:

\`\`\`elixir
day = :saturday
result = case day do
  :monday -> "weekday"
  :tuesday -> "weekday"
  :saturday -> "weekend"
  :sunday -> "weekend"
  _ -> "weekday"
end
IO.puts(result)   # weekend
\`\`\`

## Your Turn

1. Match \`n = 2\` in a case, print \`"two"\`
2. Match \`day = :saturday\` in a case, print \`"weekend"\`
`,
  starterCode: `n = 2
result = case n do
  1 -> "one"
  2 -> "two"
  _ -> "other"
end
IO.puts(result)

day = :saturday
label = case day do
  :saturday -> "weekend"
  :sunday -> "weekend"
  _ -> "weekday"
end
IO.puts(label)
`,
  solution: `n = 2
result = case n do
  1 -> "one"
  2 -> "two"
  _ -> "other"
end
IO.puts(result)

day = :saturday
label = case day do
  :saturday -> "weekend"
  :sunday -> "weekend"
  _ -> "weekday"
end
IO.puts(label)
`,
  tests: [
    {
      name: "case matching integers and atoms",
      expected: "two\nweekend\n",
    },
  ],
};
