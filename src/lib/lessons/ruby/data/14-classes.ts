import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes",
	chapterId: "methods_oop",
	content: `## Classes in Ruby

A class is a blueprint for objects. Define one with \`class\` and \`end\`:

\`\`\`ruby
class Dog
  def initialize(name)
    @name = name    # instance variable
  end

  def bark
    "#{@name} says: Woof!"
  end
end

dog = Dog.new("Rex")
puts dog.bark    # Rex says: Woof!
\`\`\`

### Instance Variables

Instance variables start with \`@\` and are private by default. Use \`attr_reader\`, \`attr_writer\`, or \`attr_accessor\` to expose them:

\`\`\`ruby
class Person
  attr_reader :name     # creates a getter
  attr_accessor :age    # creates getter and setter

  def initialize(name, age)
    @name = name
    @age = age
  end
end

p = Person.new("Alice", 30)
puts p.name   # Alice
p.age = 31
puts p.age    # 31
\`\`\`

### to_s

Override \`to_s\` to control how an object is printed:

\`\`\`ruby
class Point
  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    "(#{@x}, #{@y})"
  end
end

puts Point.new(3, 4)  # (3, 4)
\`\`\`

### Your Task

Create a \`Circle\` class with:
- \`initialize(radius)\` — stores the radius as \`@radius\`
- \`area\` — returns \`(Math::PI * @radius ** 2).round(2)\`
- \`to_s\` — returns \`"Circle(radius=<radius>)"\`

Create \`Circle.new(5)\`, print its string form and its area.`,

	starterCode: `class Circle
  def initialize(radius)
    # store radius
  end

  def area
    # return Math::PI * @radius ** 2, rounded to 2 decimal places
  end

  def to_s
    # return "Circle(radius=<radius>)"
  end
end

c = Circle.new(5)
puts c.to_s
puts c.area
`,

	solution: `class Circle
  def initialize(radius)
    @radius = radius
  end

  def area
    (Math::PI * @radius ** 2).round(2)
  end

  def to_s
    "Circle(radius=#{@radius})"
  end
end

c = Circle.new(5)
puts c.to_s
puts c.area
`,

	tests: [
		{
			name: "Circle(radius=5), 78.54",
			expected: "Circle(radius=5)\n78.54\n",
		},
	],
};
