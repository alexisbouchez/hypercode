import type { Lesson } from "../../types";

export const classes: Lesson = {
	id: "classes",
	title: "Classes",
	chapterId: "object-oriented",
	content: `## Classes

Python is fully object-oriented. A class defines a blueprint for objects — bundling data (attributes) and behavior (methods).

### Defining a Class

\`\`\`python
class Dog:
    def __init__(self, name, breed):  # constructor
        self.name = name              # instance attribute
        self.breed = breed

    def bark(self):
        return f"{self.name} says: Woof!"

    def __repr__(self):               # string representation
        return f"Dog({self.name!r}, {self.breed!r})"
\`\`\`

### Creating Instances

\`\`\`python
rex = Dog("Rex", "German Shepherd")
rex.bark()   # "Rex says: Woof!"
rex.name     # "Rex"
print(rex)   # Dog('Rex', 'German Shepherd')
\`\`\`

### Special Methods (Dunder Methods)

\`\`\`python
__init__   # constructor
__repr__   # developer-friendly string
__str__    # user-friendly string
__len__    # len() support
__eq__     # == comparison
__lt__     # < comparison
\`\`\`

### Class vs Instance Attributes

\`\`\`python
class Counter:
    total = 0          # class attribute (shared)

    def __init__(self):
        self.count = 0 # instance attribute

    def increment(self):
        self.count += 1
        Counter.total += 1
\`\`\`

### Your Task

Implement a \`Stack\` class with:
- \`push(item)\` — adds item to the top
- \`pop()\` — removes and returns the top item (raise \`IndexError\` if empty)
- \`peek()\` — returns top item without removing (raise \`IndexError\` if empty)
- \`is_empty()\` — returns \`True\` if no items
- \`size()\` — returns number of items`,

	starterCode: `class Stack:
    def __init__(self):
        self._data = []

    def push(self, item):
        pass

    def pop(self):
        pass

    def peek(self):
        pass

    def is_empty(self):
        pass

    def size(self):
        pass

s = Stack()
print(s.is_empty())
s.push(1)
s.push(2)
s.push(3)
print(s.size())
print(s.peek())
print(s.pop())
print(s.size())
`,

	solution: `class Stack:
    def __init__(self):
        self._data = []

    def push(self, item):
        self._data.append(item)

    def pop(self):
        if not self._data:
            raise IndexError("pop from empty stack")
        return self._data.pop()

    def peek(self):
        if not self._data:
            raise IndexError("peek at empty stack")
        return self._data[-1]

    def is_empty(self):
        return len(self._data) == 0

    def size(self):
        return len(self._data)

s = Stack()
print(s.is_empty())
s.push(1)
s.push(2)
s.push(3)
print(s.size())
print(s.peek())
print(s.pop())
print(s.size())
`,

	tests: [
		{
			name: "new stack is empty",
			code: `{{FUNC}}
s = Stack()
print(s.is_empty())`,
			expected: "True\n",
		},
		{
			name: "size after 3 pushes",
			code: `{{FUNC}}
s = Stack()
s.push(1)
s.push(2)
s.push(3)
print(s.size())`,
			expected: "3\n",
		},
		{
			name: "peek returns top without removing",
			code: `{{FUNC}}
s = Stack()
s.push(10)
s.push(20)
print(s.peek())
print(s.size())`,
			expected: "20\n2\n",
		},
		{
			name: "pop returns and removes top",
			code: `{{FUNC}}
s = Stack()
s.push("a")
s.push("b")
print(s.pop())
print(s.pop())
print(s.is_empty())`,
			expected: "b\na\nTrue\n",
		},
	],
};
