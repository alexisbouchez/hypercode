import type { Lesson } from "../../types";

export const operations: Lesson = {
	id: "operations",
	title: "More Operations",
	chapterId: "autograd",
	content: `## More Operations on Value

The neural network uses six scalar operations beyond add and mul: **power**, **log**, **exp**, **relu**, and their negation/division variants.

Each operation records its local gradient — the derivative of the output with respect to the input.

### Power

\`a ** n\` → local gradient is \`n * a^(n-1)\` (power rule):

\`\`\`python
def __pow__(self, other):
    return Value(self.data**other, (self,), (other * self.data**(other-1),))
\`\`\`

### Log and Exp

\`d/da log(a) = 1/a\` and \`d/da exp(a) = exp(a)\`:

\`\`\`python
def log(self):
    return Value(math.log(self.data), (self,), (1/self.data,))

def exp(self):
    return Value(math.exp(self.data), (self,), (math.exp(self.data),))
\`\`\`

### ReLU

ReLU is \`max(0, x)\`. Its derivative is 1 when \`x > 0\`, and 0 otherwise:

\`\`\`python
def relu(self):
    return Value(max(0, self.data), (self,), (float(self.data > 0),))
\`\`\`

ReLU is the nonlinearity in the MLP block. Without nonlinearities, stacking linear layers collapses to a single linear transformation.

### Your Task

Add \`__pow__\`, \`log\`, \`exp\`, and \`relu\` to the Value class.`,

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
        # TODO: local grad is other * self.data**(other-1)
        pass

    def log(self):
        # TODO: local grad is 1/self.data
        pass

    def exp(self):
        # TODO: local grad is math.exp(self.data)
        pass

    def relu(self):
        # TODO: output is max(0, self.data), local grad is float(self.data > 0)
        pass

a = Value(2.0)
print(round((a**3).data, 4))
print(round(Value(math.e).log().data, 4))
print(Value(5.0).relu().data)
print(Value(-3.0).relu().data)
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

    def log(self):
        return Value(math.log(self.data), (self,), (1/self.data,))

    def exp(self):
        return Value(math.exp(self.data), (self,), (math.exp(self.data),))

    def relu(self):
        return Value(max(0, self.data), (self,), (float(self.data > 0),))

a = Value(2.0)
print(round((a**3).data, 4))
print(round(Value(math.e).log().data, 4))
print(Value(5.0).relu().data)
print(Value(-3.0).relu().data)
`,

	tests: [
		{
			name: "pow, log, exp, relu compute correctly",
			expected: "8.0\n1.0\n5.0\n0\n",
		},
	],
};
