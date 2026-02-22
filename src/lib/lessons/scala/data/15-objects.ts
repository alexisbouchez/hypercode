import type { Lesson } from "../../types";

export const objects: Lesson = {
  id: "objects",
  title: "Objects",
  chapterId: "oop",
  content: `## Objects (Singletons)

A Scala \`object\` is a singleton — a class with exactly one instance, created automatically:

\`\`\`scala
object MathUtils {
  def square(x: Int): Int = x * x
  def cube(x: Int): Int = x * x * x
}

println(MathUtils.square(4))  // 16
println(MathUtils.cube(3))    // 27
\`\`\`

Objects are useful for utility methods and constants:

\`\`\`scala
object Constants {
  val Pi = 3.14159
  val E  = 2.71828
}

println(Constants.Pi)
\`\`\`

### Your Task

Define an \`object StringUtils\` with a method \`isPalindrome(s: String): Boolean\` that returns \`true\` if the string equals its reverse.`,

  starterCode: `object StringUtils {
  def isPalindrome(s: String): Boolean = s == s.reverse
}

println(StringUtils.isPalindrome("racecar"))
println(StringUtils.isPalindrome("hello"))
`,

  solution: `object StringUtils {
  def isPalindrome(s: String): Boolean = s == s.reverse
}

println(StringUtils.isPalindrome("racecar"))
println(StringUtils.isPalindrome("hello"))
`,

  tests: [
    {
      name: "racecar is palindrome",
      expected: "true\n",
      code: `{{FUNC}}
println(StringUtils.isPalindrome("racecar"))
`,
    },
    {
      name: "hello is not palindrome",
      expected: "false\n",
      code: `{{FUNC}}
println(StringUtils.isPalindrome("hello"))
`,
    },
    {
      name: "madam is palindrome",
      expected: "true\n",
      code: `{{FUNC}}
println(StringUtils.isPalindrome("madam"))
`,
    },
  ],
};
