import type { Lesson } from "../../types";

export const trainingLoop: Lesson = {
	id: "training-loop",
	title: "Training Loop",
	chapterId: "training",
	content: `## The Training Loop

Training is a loop of five steps, repeated for each example:

1. **Pick** a training example
2. **Forward pass**: run the model, compute the loss
3. **Backward pass**: \`loss.backward()\` to compute all gradients
4. **Update**: nudge each parameter in the direction that reduces loss
5. **Zero gradients**: reset \`p.grad = 0\` before the next step

### Why Zero Gradients?

Gradients **accumulate** (we use \`+=\`) so they must be explicitly reset after each update. Otherwise, old gradients from previous examples would corrupt the current update.

### Gradient Descent Update

The simplest optimizer just subtracts a fraction of the gradient:

\`\`\`python
def sgd_step(params, lr):
    for p in params:
        p.data -= lr * p.grad
        p.grad = 0
\`\`\`

### Example: Learning a Single Scalar

The clearest possible training loop: find \`w\` such that \`w * 3 = 3\` (i.e., \`w = 1\`).

\`\`\`python
w = Value(0.0)
for step in range(20):
    pred = w * Value(3.0)
    loss = (pred - Value(3.0)) ** 2
    loss.backward()
    sgd_step([w], lr=0.05)
\`\`\`

Each step, the gradient \`d(loss)/dw = 2 * (3w - 3) * 3 = 18(w-1)\` pushes \`w\` toward 1.

### Your Task

Implement \`sgd_step(params, lr)\` and run the training loop.`,

	starterCode: `import math

class Value:
    __slots__ = ('data', 'grad', '_children', '_local_grads')
    def __init__(self, data, children=(), local_grads=()):
        self.data = data; self.grad = 0
        self._children = children; self._local_grads = local_grads
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
        topo = []; visited = set()
        def build(v):
            if v not in visited:
                visited.add(v)
                for c in v._children: build(c)
                topo.append(v)
        build(self); self.grad = 1
        for v in reversed(topo):
            for c, lg in zip(v._children, v._local_grads):
                c.grad += lg * v.grad

def sgd_step(params, lr):
    # TODO: for each param, subtract lr * grad, then reset grad to 0
    pass

w = Value(0.0)
for step in range(20):
    pred = w * Value(3.0)
    loss = (pred - Value(3.0)) ** 2
    loss.backward()
    sgd_step([w], lr=0.05)

print(round(w.data, 4))
`,

	solution: `import math

class Value:
    __slots__ = ('data', 'grad', '_children', '_local_grads')
    def __init__(self, data, children=(), local_grads=()):
        self.data = data; self.grad = 0
        self._children = children; self._local_grads = local_grads
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
        topo = []; visited = set()
        def build(v):
            if v not in visited:
                visited.add(v)
                for c in v._children: build(c)
                topo.append(v)
        build(self); self.grad = 1
        for v in reversed(topo):
            for c, lg in zip(v._children, v._local_grads):
                c.grad += lg * v.grad

def sgd_step(params, lr):
    for p in params:
        p.data -= lr * p.grad
        p.grad = 0

w = Value(0.0)
for step in range(20):
    pred = w * Value(3.0)
    loss = (pred - Value(3.0)) ** 2
    loss.backward()
    sgd_step([w], lr=0.05)

print(round(w.data, 4))
`,

	tests: [
		{
			name: "SGD converges w toward 1.0 after 20 steps",
			expected: "1.0\n",
		},
	],
};
