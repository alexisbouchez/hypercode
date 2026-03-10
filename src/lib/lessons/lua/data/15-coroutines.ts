import type { Lesson } from "../../types";

export const coroutines: Lesson = {
  id: "coroutines",
  title: "Coroutines Overview",
  chapterId: "advanced",
  content: `## Coroutines

Coroutines are one of Lua's most powerful features. They allow cooperative multitasking by letting functions suspend and resume execution.

\`\`\`lua
function producer()
  local i = 0
  while true do
    i = i + 1
    coroutine.yield(i)
  end
end
\`\`\`

A coroutine is created with \`coroutine.create\` and resumed with \`coroutine.resume\`. Each \`yield\` pauses execution and returns a value.

Since coroutines require runtime support beyond our transpiler, in this lesson we will simulate the pattern using closures that mimic coroutine behavior:

\`\`\`lua
function makeRange(start, stop)
  local i = start - 1
  return function()
    i = i + 1
    if i <= stop then
      return i
    end
    return nil
  end
end
\`\`\`

### Iterator Pattern

This closure-based approach is how many Lua iterators work under the hood.

### Your Task

Write a \`fibonacci\` function that returns an iterator. Each call to the iterator returns the next Fibonacci number. The sequence starts: 1, 1, 2, 3, 5, 8, ...`,

  starterCode: `function fibonacci()
  local a = 0
  local b = 1
  return function()
    local next = a + b
    a = b
    b = next
    return a
  end
end

local fib = fibonacci()
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
`,

  solution: `function fibonacci()
  local a = 0
  local b = 1
  return function()
    local next = a + b
    a = b
    b = next
    return a
  end
end

local fib = fibonacci()
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
`,

  tests: [
    {
      name: "first 6 fibonacci numbers",
      expected: "1\n1\n2\n3\n5\n8\n",
      code: `{{FUNC}}
local fib = fibonacci()
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
print(fib())
`,
    },
    {
      name: "independent iterators",
      expected: "1\n1\n1\n",
      code: `{{FUNC}}
local fib1 = fibonacci()
local fib2 = fibonacci()
print(fib1())
print(fib2())
print(fib1())
`,
    },
    {
      name: "first 3 fibonacci",
      expected: "1\n1\n2\n",
      code: `{{FUNC}}
local fib = fibonacci()
print(fib())
print(fib())
print(fib())
`,
    },
  ],
};
