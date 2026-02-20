import type { Lesson } from "../../types";

export const inheritance: Lesson = {
	id: "inheritance",
	title: "Inheritance",
	chapterId: "object-oriented",
	content: `## Inheritance

Inheritance lets a class reuse and extend another class's behavior. The child class inherits all of the parent's methods and attributes.

### Basic Inheritance

\`\`\`python
class Animal:
    def __init__(self, name):
        self.name = name

    def speak(self):
        return "..."

class Dog(Animal):          # Dog inherits from Animal
    def speak(self):        # override parent method
        return "Woof!"

class Cat(Animal):
    def speak(self):
        return "Meow!"

dog = Dog("Rex")
dog.speak()   # "Woof!"
dog.name      # "Rex"  (inherited attribute)
\`\`\`

### super()

Call the parent's \`__init__\` to avoid repeating initialization code:

\`\`\`python
class Vehicle:
    def __init__(self, make, speed):
        self.make = make
        self.speed = speed

class Car(Vehicle):
    def __init__(self, make, speed, doors):
        super().__init__(make, speed)  # delegate to parent
        self.doors = doors
\`\`\`

### isinstance() and issubclass()

\`\`\`python
isinstance(dog, Dog)     # True
isinstance(dog, Animal)  # True  (Dog is an Animal)
issubclass(Dog, Animal)  # True
\`\`\`

### Abstract Methods (abc)

\`\`\`python
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self):
        pass
\`\`\`

### Your Task

Implement three classes:
- \`Shape\` with an \`area()\` method that returns \`0\`
- \`Circle(Shape)\` that takes \`radius\` and overrides \`area()\` to return \`π * r²\` (use \`3.14159\`)
- \`Rectangle(Shape)\` that takes \`width\` and \`height\` and overrides \`area()\``,

	starterCode: `import math

class Shape:
    def area(self):
        return 0

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        # Return pi * r^2 using math.pi
        pass

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        pass

print(Shape().area())
print(round(Circle(5).area(), 2))
print(Rectangle(4, 6).area())
`,

	solution: `import math

class Shape:
    def area(self):
        return 0

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

print(Shape().area())
print(round(Circle(5).area(), 2))
print(Rectangle(4, 6).area())
`,

	tests: [
		{
			name: "Shape.area() returns 0",
			code: `{{FUNC}}
print(Shape().area())`,
			expected: "0\n",
		},
		{
			name: "Circle(5).area() ≈ 78.54",
			code: `{{FUNC}}
print(round(Circle(5).area(), 2))`,
			expected: "78.54\n",
		},
		{
			name: "Rectangle(4, 6).area() = 24",
			code: `{{FUNC}}
print(Rectangle(4, 6).area())`,
			expected: "24\n",
		},
		{
			name: "Circle is a Shape",
			code: `{{FUNC}}
print(isinstance(Circle(1), Shape))`,
			expected: "True\n",
		},
	],
};
