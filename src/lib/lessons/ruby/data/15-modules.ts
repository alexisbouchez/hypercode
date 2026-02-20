import type { Lesson } from "../../types";

export const modules: Lesson = {
	id: "modules",
	title: "Modules",
	chapterId: "methods_oop",
	content: `## Modules in Ruby

A **module** is a collection of methods and constants that can be mixed into classes. Unlike classes, modules cannot be instantiated directly.

### Modules as Mixins

Use \`include\` to add a module's methods to a class:

\`\`\`ruby
module Greetable
  def greet
    "Hello, I'm #{name}!"
  end
end

class Person
  include Greetable
  attr_reader :name

  def initialize(name)
    @name = name
  end
end

alice = Person.new("Alice")
puts alice.greet   # Hello, I'm Alice!
\`\`\`

The module's method \`greet\` can call \`name\` because it will be mixed into classes that define it.

### Namespacing

Modules also serve as namespaces to organize related code:

\`\`\`ruby
module MathUtils
  PI = 3.14159

  def self.circle_area(r)
    PI * r ** 2
  end
end

puts MathUtils::PI                # 3.14159
puts MathUtils.circle_area(5)    # 78.53975
\`\`\`

### Multiple Mixins

A class can include multiple modules:

\`\`\`ruby
module Swimmable
  def swim; "swimming!"; end
end

module Flyable
  def fly; "flying!"; end
end

class Duck
  include Swimmable
  include Flyable
end

d = Duck.new
puts d.swim   # swimming!
puts d.fly    # flying!
\`\`\`

### Your Task

Create a module \`Describable\` with a method \`describe\` that returns \`"#{self.class}: #{name}"\`.

Create a class \`Animal\` that includes \`Describable\`, has \`attr_reader :name\`, and an \`initialize(name)\` method.

Create \`Animal.new("Whiskers")\` and print \`cat.describe\`.`,

	starterCode: `module Describable
  def describe
    # Return "ClassName: name"
  end
end

class Animal
  include Describable
  attr_reader :name

  def initialize(name)
    # store name
  end
end

cat = Animal.new("Whiskers")
puts cat.describe
`,

	solution: `module Describable
  def describe
    "#{self.class}: #{name}"
  end
end

class Animal
  include Describable
  attr_reader :name

  def initialize(name)
    @name = name
  end
end

cat = Animal.new("Whiskers")
puts cat.describe
`,

	tests: [
		{
			name: "Animal: Whiskers",
			expected: "Animal: Whiskers\n",
		},
	],
};
