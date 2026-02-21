import type { Lesson } from "../../types";

export const adam: Lesson = {
	id: "adam",
	title: "Adam Optimizer",
	chapterId: "training",
	content: `## Adam: Adaptive Moment Estimation

Plain gradient descent uses the same learning rate for all parameters at all times. Adam is better:

1. **Momentum** (first moment \`m\`): smooths out noisy gradients by keeping an exponential moving average. Instead of jumping with every new gradient, we drift in a consistent direction.

2. **Adaptive rates** (second moment \`v\`): parameters with consistently large gradients get smaller steps; parameters with small gradients get larger steps.

### The Update Rule

\`\`\`python
m = beta1 * m + (1 - beta1) * grad       # momentum
v = beta2 * v + (1 - beta2) * grad**2    # variance

m_hat = m / (1 - beta1**(step+1))        # bias correction
v_hat = v / (1 - beta2**(step+1))        # bias correction

param -= lr * m_hat / (v_hat**0.5 + eps)
\`\`\`

### Bias Correction

At the start of training, \`m\` and \`v\` are initialized to 0 and are biased toward zero. Dividing by \`(1 - beta^t)\` corrects this. As \`t\` grows, the correction factor approaches 1.

### MicroGPT Hyperparameters

\`\`\`python
lr = 0.01
beta1 = 0.85   # lower than typical 0.9 — less momentum
beta2 = 0.99   # lower than typical 0.999 — faster adaptation
eps = 1e-8
\`\`\`

### Your Task

Implement \`adam_step(p, m, v, step)\` that performs one Adam update and returns the updated \`(m, v)\`.`,

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

def adam_step(p, m, v, step, lr=0.01, beta1=0.85, beta2=0.99, eps=1e-8):
    # TODO:
    # 1. Update first moment:  m = beta1 * m + (1 - beta1) * p.grad
    # 2. Update second moment: v = beta2 * v + (1 - beta2) * p.grad**2
    # 3. Bias-correct:         m_hat = m / (1 - beta1**(step+1))
    #                          v_hat = v / (1 - beta2**(step+1))
    # 4. Update param:         p.data -= lr * m_hat / (v_hat**0.5 + eps)
    # 5. Zero gradient:        p.grad = 0
    # 6. Return (m, v)
    pass

# Test: one Adam step with a known gradient
p = Value(0.0)
p.grad = -2.0   # pretend backward() computed this

m, v = adam_step(p, m=0.0, v=0.0, step=0, lr=0.01, beta1=0.85, beta2=0.99)
print(round(m, 4))
print(round(v, 4))
print(round(p.data, 4))
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

def adam_step(p, m, v, step, lr=0.01, beta1=0.85, beta2=0.99, eps=1e-8):
    m = beta1 * m + (1 - beta1) * p.grad
    v = beta2 * v + (1 - beta2) * p.grad ** 2
    m_hat = m / (1 - beta1 ** (step + 1))
    v_hat = v / (1 - beta2 ** (step + 1))
    p.data -= lr * m_hat / (v_hat ** 0.5 + eps)
    p.grad = 0
    return m, v

p = Value(0.0)
p.grad = -2.0

m, v = adam_step(p, m=0.0, v=0.0, step=0, lr=0.01, beta1=0.85, beta2=0.99)
print(round(m, 4))
print(round(v, 4))
print(round(p.data, 4))
`,

	tests: [
		{
			name: "one Adam step with grad=-2 produces correct m, v, and param update",
			expected: "-0.3\n0.04\n0.01\n",
		},
	],
};
