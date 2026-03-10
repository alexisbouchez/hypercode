import type { Lesson } from "../../types";

export const anonymous: Lesson = {
  id: "anonymous",
  title: "Anonymous Functions",
  chapterId: "functions",
  content: `## fn...end

Anonymous functions are created with \`fn\` and called with a dot:

\`\`\`elixir
double = fn x -> x * 2 end
IO.puts(double.(5))   # 10
\`\`\`

The dot \`.\` is required when calling anonymous functions in Elixir.

## Capture Syntax &

The \`&\` shorthand lets you write compact anonymous functions:

\`\`\`elixir
add = &(&1 + &2)
IO.puts(add.(3, 4))   # 7
\`\`\`

\`&1\`, \`&2\` refer to the first and second arguments.

## Your Turn

1. Create \`double = fn x -> x * 2 end\` and print \`double.(5)\` → 10
2. Create \`add = &(&1 + &2)\` and print \`add.(3, 4)\` → 7
3. Create \`square = &(&1 * &1)\` and print \`square.(6)\` → 36
`,
  starterCode: `double = fn x -> x * 2 end
IO.puts(double.(5))

add = &(&1 + &2)
IO.puts(add.(3, 4))

square = &(&1 * &1)
IO.puts(square.(6))
`,
  solution: `double = fn x -> x * 2 end
IO.puts(double.(5))

add = &(&1 + &2)
IO.puts(add.(3, 4))

square = &(&1 * &1)
IO.puts(square.(6))
`,
  tests: [
    {
      name: "fn and & capture",
      expected: "10\n7\n36\n",
    },
    {
      name: "anonymous function with zero",
      code: `double = fn x -> x * 2 end
IO.puts(double.(0))
IO.puts(double.(-3))
`,
      expected: "0\n-6\n",
    },
    {
      name: "capture syntax with subtraction",
      code: `subtract = &(&1 - &2)
IO.puts(subtract.(10, 3))
IO.puts(subtract.(5, 5))
`,
      expected: "7\n0\n",
    },
  ],
};
