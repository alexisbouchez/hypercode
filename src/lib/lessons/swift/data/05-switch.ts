import type { Lesson } from "../../types";

export const switchLesson: Lesson = {
  id: "switch",
  title: "Switch",
  chapterId: "basics",
  content: `## Switch Statements

Swift's \`switch\` is more powerful than C's. Cases don't fall through by default, and no explicit \`break\` is needed:

\`\`\`swift
let day = 3

switch day {
case 1:
    print("Monday")
case 2:
    print("Tuesday")
case 3:
    print("Wednesday")
default:
    print("Other")
}
// Wednesday
\`\`\`

### Multiple Values Per Case

\`\`\`swift
switch month {
case 12, 1, 2:
    print("winter")
case 3, 4, 5:
    print("spring")
default:
    print("other")
}
\`\`\`

### Your Task

Write a function \`season\` that takes a month number (1-12) and returns the season:
- \`"winter"\` for months 12, 1, 2
- \`"spring"\` for months 3, 4, 5
- \`"summer"\` for months 6, 7, 8
- \`"autumn"\` for months 9, 10, 11`,

  starterCode: `func season(_ month: Int) -> String {
    switch month {
    case 12, 1, 2:
        return "winter"
    case 3, 4, 5:
        return "spring"
    case 6, 7, 8:
        return "summer"
    default:
        return "autumn"
    }
}

print(season(1))
print(season(4))
print(season(7))
print(season(10))
`,

  solution: `func season(_ month: Int) -> String {
    switch month {
    case 12, 1, 2:
        return "winter"
    case 3, 4, 5:
        return "spring"
    case 6, 7, 8:
        return "summer"
    default:
        return "autumn"
    }
}

print(season(1))
print(season(4))
print(season(7))
print(season(10))
`,

  tests: [
    {
      name: "January is winter",
      expected: "winter\n",
      code: `{{FUNC}}
print(season(1))
`,
    },
    {
      name: "April is spring",
      expected: "spring\n",
      code: `{{FUNC}}
print(season(4))
`,
    },
    {
      name: "July is summer",
      expected: "summer\n",
      code: `{{FUNC}}
print(season(7))
`,
    },
    {
      name: "October is autumn",
      expected: "autumn\n",
      code: `{{FUNC}}
print(season(10))
`,
    },
  ],
};
