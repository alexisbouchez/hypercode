import type { Lesson } from "../../types";

export const shortcuts: Lesson = {
	id: "shortcuts",
	title: "Operator Shortcuts",
	chapterId: "autograd",
	content: `## Operator Shortcuts

Python lets you overload the full set of arithmetic operators. Most can be defined in terms of the operations we already have — no new gradient logic needed.

### Negation

\`-a\` is just \`a * -1\`:

\`\`\`python
def __neg__(self): return self * -1
\`\`\`

### Right-hand Operations

Python calls \`__radd__\` when the left operand doesn't know how to add. \`3 + a\` becomes \`a.__radd__(3)\`:

\`\`\`python
def __radd__(self, other): return self + other
def __rmul__(self, other): return self * other
\`\`\`

### Subtraction and Division

Both reduce to existing operations:

\`\`\`python
def __sub__(self, other):  return self + (-other)
def __rsub__(self, other): return other + (-self)
def __truediv__(self, other):  return self * other**-1
def __rtruediv__(self, other): return other * self**-1
\`\`\`

Note \`other**-1\` — this calls Python's built-in \`**\` on the raw number (not \`Value.__pow__\`), giving the scalar reciprocal. Then \`self * scalar\` uses \`Value.__mul__\` which wraps the scalar automatically.

### Your Task

Add all seven shortcut methods to Value.`,

	starterCode: `import math

class Value:
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

    def __pow__(self, other):
        return Value(self.data**other, (self,), (other * self.data**(other-1),))

    def log(self): return Value(math.log(self.data), (self,), (1/self.data,))
    def exp(self): return Value(math.exp(self.data), (self,), (math.exp(self.data),))
    def relu(self): return Value(max(0, self.data), (self,), (float(self.data > 0),))

    # TODO: implement the seven shortcuts below
    def __neg__(self): pass
    def __radd__(self, other): pass
    def __sub__(self, other): pass
    def __rsub__(self, other): pass
    def __rmul__(self, other): pass
    def __truediv__(self, other): pass
    def __rtruediv__(self, other): pass

a = Value(5.0)
b = Value(2.0)
print((-a).data)
print((a - b).data)
print((a / b).data)
print((3 + a).data)
`,

	solution: `import math

class Value:
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

    def __pow__(self, other):
        return Value(self.data**other, (self,), (other * self.data**(other-1),))

    def log(self): return Value(math.log(self.data), (self,), (1/self.data,))
    def exp(self): return Value(math.exp(self.data), (self,), (math.exp(self.data),))
    def relu(self): return Value(max(0, self.data), (self,), (float(self.data > 0),))

    def __neg__(self): return self * -1
    def __radd__(self, other): return self + other
    def __sub__(self, other): return self + (-other)
    def __rsub__(self, other): return other + (-self)
    def __rmul__(self, other): return self * other
    def __truediv__(self, other): return self * other**-1
    def __rtruediv__(self, other): return other * self**-1

a = Value(5.0)
b = Value(2.0)
print((-a).data)
print((a - b).data)
print((a / b).data)
print((3 + a).data)
`,

	tests: [
		{
			name: "negation, subtraction, division, and radd work",
			expected: "-5.0\n3.0\n2.5\n8.0\n",
		},
	],
};
