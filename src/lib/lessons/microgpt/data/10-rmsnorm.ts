import type { Lesson } from "../../types";

export const rmsnorm: Lesson = {
	id: "rmsnorm",
	title: "RMSNorm",
	chapterId: "primitives",
	content: `## Normalization

Deep networks suffer from **exploding or vanishing activations** — values that grow too large or shrink to zero as they pass through many layers. Normalization keeps activations in a stable range.

### RMSNorm

RMSNorm normalizes each vector by its **root mean square**:

\`\`\`
rms(x) = sqrt(mean(x²) + ε)
x_norm = x / rms(x)
\`\`\`

The small \`ε = 1e-5\` prevents division by zero.

\`\`\`python
def rmsnorm(x):
    ms = sum(xi * xi for xi in x) / len(x)   # mean of squares
    scale = (ms + 1e-5) ** -0.5               # 1 / rms
    return [xi * scale for xi in x]
\`\`\`

### Why RMSNorm over LayerNorm?

LayerNorm subtracts the mean and divides by std. RMSNorm only divides by the RMS — simpler and faster. Modern architectures (LLaMA, Mistral, Gemma) use RMSNorm.

In MicroGPT, RMSNorm is applied **before** the attention and MLP blocks (pre-norm), which stabilizes training more reliably than post-norm.

### Working with Value Objects

All operations here are defined on \`Value\`:
- \`xi * xi\` uses \`Value.__mul__\`
- \`sum(...) / len(x)\` uses \`Value.__truediv__\`
- \`(ms + 1e-5) ** -0.5\` uses \`Value.__add__\` and \`Value.__pow__\`

### Your Task

Implement \`rmsnorm(x)\` where \`x\` is a list of \`Value\` objects.`,

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

def rmsnorm(x):
    # TODO:
    # 1. Compute mean of squares: ms = sum(xi * xi for xi in x) / len(x)
    # 2. Compute scale = (ms + 1e-5) ** -0.5
    # 3. Return [xi * scale for xi in x]
    pass

x = [Value(3.0), Value(4.0)]
result = rmsnorm(x)
print(round(result[0].data, 4))
print(round(result[1].data, 4))
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

def rmsnorm(x):
    ms = sum(xi * xi for xi in x) / len(x)
    scale = (ms + 1e-5) ** -0.5
    return [xi * scale for xi in x]

x = [Value(3.0), Value(4.0)]
result = rmsnorm(x)
print(round(result[0].data, 4))
print(round(result[1].data, 4))
`,

	tests: [
		{
			name: "rmsnorm([3, 4]) normalizes correctly",
			expected: "0.8485\n1.1314\n",
		},
	],
};
