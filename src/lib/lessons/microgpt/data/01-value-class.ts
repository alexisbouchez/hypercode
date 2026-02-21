import type { Lesson } from "../../types";

export const valueClass: Lesson = {
	id: "value-class",
	title: "The Value Class",
	chapterId: "autograd",
	content: `## The Autograd Engine

Every neural network needs to compute gradients — the direction to nudge each weight to reduce loss. In frameworks like PyTorch, this happens automatically. In MicroGPT, we build it ourselves.

The foundation is a single class: **Value**. It wraps a scalar number and records two things:

1. Which other Values created it (the computation graph)
2. What the local gradient of each parent is (how this output changes when each parent changes)

\`\`\`python
class Value:
    __slots__ = ('data', 'grad', '_children', '_local_grads')

    def __init__(self, data, children=(), local_grads=()):
        self.data = data        # the scalar value
        self.grad = 0           # accumulated gradient (starts at 0)
        self._children = children       # parent nodes in the graph
        self._local_grads = local_grads # d(output)/d(each parent)
\`\`\`

### Addition

When \`c = a + b\`:
- Output: \`c.data = a.data + b.data\`
- Local gradients: \`dc/da = 1\` and \`dc/db = 1\`

So \`c\` stores \`children=(a, b)\` and \`local_grads=(1, 1)\`.

\`\`\`python
def __add__(self, other):
    other = other if isinstance(other, Value) else Value(other)
    return Value(self.data + other.data, (self, other), (1, 1))
\`\`\`

### Multiplication

When \`c = a * b\`:
- Output: \`c.data = a.data * b.data\`
- Local gradients: \`dc/da = b.data\` and \`dc/db = a.data\`

\`\`\`python
def __mul__(self, other):
    other = other if isinstance(other, Value) else Value(other)
    return Value(self.data * other.data, (self, other), (other.data, self.data))
\`\`\`

### Your Task

Implement the \`Value\` class with \`__init__\`, \`__add__\`, and \`__mul__\`.`,

	starterCode: `class Value:
    __slots__ = ('data', 'grad', '_children', '_local_grads')

    def __init__(self, data, children=(), local_grads=()):
        # TODO: store data, set grad to 0, store children and local_grads
        pass

    def __add__(self, other):
        # Convert plain numbers to Value if needed
        other = other if isinstance(other, Value) else Value(other)
        # TODO: return a new Value with the sum, recording (self, other) as children
        # Local grads for addition: dc/da = 1, dc/db = 1
        pass

    def __mul__(self, other):
        other = other if isinstance(other, Value) else Value(other)
        # TODO: return a new Value with the product, recording (self, other) as children
        # Local grads for multiplication: dc/da = b.data, dc/db = a.data
        pass

a = Value(3.0)
b = Value(4.0)
c = a + b
d = a * b
print(c.data)
print(d.data)
`,

	solution: `class Value:
    __slots__ = ('data', 'grad', '_children', '_local_grads')

    def __init__(self, data, children=(), local_grads=()):
        self.data = data
        self.grad = 0
        self._children = children
        self._local_grads = local_grads

    def __add__(self, other):
        other = other if isinstance(other, Value) else Value(other)
        return Value(self.data + other.data, (self, other), (1, 1))

    def __mul__(self, other):
        other = other if isinstance(other, Value) else Value(other)
        return Value(self.data * other.data, (self, other), (other.data, self.data))

a = Value(3.0)
b = Value(4.0)
c = a + b
d = a * b
print(c.data)
print(d.data)
`,

	tests: [
		{
			name: "addition and multiplication work correctly",
			expected: "7.0\n12.0\n",
		},
	],
};
