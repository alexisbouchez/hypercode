import type { Lesson } from "../../types";

export const backward: Lesson = {
	id: "backward",
	title: "Backpropagation",
	chapterId: "autograd",
	content: `## The Backward Pass

Backpropagation applies the **chain rule** in reverse topological order. For each node, we multiply the gradient flowing in (from the output side) by each local gradient and accumulate it into the child's gradient.

### The Chain Rule

If \`z = f(a, b)\`, and we already know \`dL/dz\` (gradient from the loss to \`z\`), then:

\`\`\`
dL/da = (dz/da) * (dL/dz) = local_grad_a * z.grad
dL/db = (dz/db) * (dL/dz) = local_grad_b * z.grad
\`\`\`

### The backward() Method

1. Build the topological order
2. Set the root gradient to 1 (the loss w.r.t. itself is 1)
3. Walk in reverse order: for each node, propagate its gradient to its children

\`\`\`python
def backward(self):
    topo = []
    visited = set()
    def build_topo(v):
        if v not in visited:
            visited.add(v)
            for child in v._children:
                build_topo(child)
            topo.append(v)
    build_topo(self)
    self.grad = 1
    for v in reversed(topo):
        for child, local_grad in zip(v._children, v._local_grads):
            child.grad += local_grad * v.grad
\`\`\`

Note \`+=\`: gradients **accumulate** because the same Value may appear multiple times in the graph (e.g., \`a * a\`).

### Verifying by Hand

For \`z = a * b\`:
- \`z._local_grads = (b.data, a.data)\`
- After \`z.backward()\`: \`a.grad = b.data * 1\`, \`b.grad = a.data * 1\`

For \`w = a + b\`:
- \`w._local_grads = (1, 1)\`
- After \`w.backward()\`: \`a.grad = 1\`, \`b.grad = 1\`

### Your Task

Add \`backward()\` to the Value class.`,

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
    def __neg__(self): return self * -1
    def __radd__(self, other): return self + other
    def __sub__(self, other): return self + (-other)
    def __rsub__(self, other): return other + (-self)
    def __rmul__(self, other): return self * other
    def __truediv__(self, other): return self * other**-1
    def __rtruediv__(self, other): return other * self**-1

    def backward(self):
        # TODO:
        # 1. Build topological order using DFS
        # 2. Set self.grad = 1 (the loss w.r.t. itself)
        # 3. Walk topo in reverse, propagating gradients:
        #    for each child and its local_grad: child.grad += local_grad * v.grad
        pass

# Test 1: z = a * b → dz/da = b, dz/db = a
a = Value(3.0)
b = Value(4.0)
z = a * b
z.backward()
print(a.grad)
print(b.grad)

# Test 2: w = x + y → dw/dx = 1, dw/dy = 1
x = Value(2.0)
y = Value(5.0)
w = x + y
w.backward()
print(x.grad)
print(y.grad)
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

    def backward(self):
        topo = []
        visited = set()
        def build_topo(v):
            if v not in visited:
                visited.add(v)
                for child in v._children:
                    build_topo(child)
                topo.append(v)
        build_topo(self)
        self.grad = 1.0
        for v in reversed(topo):
            for child, local_grad in zip(v._children, v._local_grads):
                child.grad += local_grad * v.grad

a = Value(3.0)
b = Value(4.0)
z = a * b
z.backward()
print(a.grad)
print(b.grad)

x = Value(2.0)
y = Value(5.0)
w = x + y
w.backward()
print(x.grad)
print(y.grad)
`,

	tests: [
		{
			name: "gradients for multiply and add are correct",
			expected: "4.0\n3.0\n1.0\n1.0\n",
		},
	],
};
