import type { Lesson } from "../../types";

export const metatables: Lesson = {
  id: "metatables",
  title: "Metatables",
  chapterId: "tables",
  content: `## Metatables

Metatables let you customize how tables behave. The most common use is the \`__index\` metamethod, which provides default values or inheritance.

\`\`\`lua
local defaults = {color = "red", size = 10}
local obj = {size = 25}
setmetatable(obj, {__index = defaults})

print(obj.size)   -- 25  (own field)
print(obj.color)  -- red (from defaults)
\`\`\`

### Object-Oriented Pattern

Metatables enable OOP in Lua:

\`\`\`lua
local Animal = {}
Animal.__index = Animal

function Animal.new(name, sound)
  local self = {}
  setmetatable(self, Animal)
  self.name = name
  self.sound = sound
  return self
end

function Animal.speak(self)
  print(self.name .. " says " .. self.sound)
end
\`\`\`

### Your Task

Create a simple "class" using metatables. Create a \`Dog\` table with a \`new\` constructor and a \`bark\` method.`,

  starterCode: `local Dog = {}
Dog.__index = Dog

function Dog.new(name)
  local self = {}
  self.name = name
  return self
end

function Dog.bark(self)
  print(self.name .. " says Woof!")
end

local rex = Dog.new("Rex")
local max = Dog.new("Max")

Dog.bark(rex)
Dog.bark(max)
`,

  solution: `local Dog = {}
Dog.__index = Dog

function Dog.new(name)
  local self = {}
  self.name = name
  return self
end

function Dog.bark(self)
  print(self.name .. " says Woof!")
end

local rex = Dog.new("Rex")
local max = Dog.new("Max")

Dog.bark(rex)
Dog.bark(max)
`,

  tests: [
    {
      name: "Rex barks",
      expected: "Rex says Woof!\n",
      code: `{{FUNC}}
local rex = Dog.new("Rex")
Dog.bark(rex)
`,
    },
    {
      name: "Max barks",
      expected: "Max says Woof!\n",
      code: `{{FUNC}}
local max = Dog.new("Max")
Dog.bark(max)
`,
    },
    {
      name: "Buddy barks",
      expected: "Buddy says Woof!\n",
      code: `{{FUNC}}
local buddy = Dog.new("Buddy")
Dog.bark(buddy)
`,
    },
  ],
};
