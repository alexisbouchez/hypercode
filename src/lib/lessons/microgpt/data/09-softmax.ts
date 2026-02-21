import type { Lesson } from "../../types";

export const softmax: Lesson = {
	id: "softmax",
	title: "Softmax",
	chapterId: "primitives",
	content: `## Softmax: Logits to Probabilities

The model produces raw **logits** — one per vocabulary token. Softmax converts them to probabilities that sum to 1.

### Definition

\`\`\`
softmax(x)[i] = exp(x[i]) / sum(exp(x[j]) for j)
\`\`\`

### Numerical Stability

Raw exponents overflow for large inputs. The standard fix: subtract the maximum value before exponentiating. This doesn't change the result (the constant cancels in the division) but keeps numbers finite:

\`\`\`python
def softmax(logits):
    max_val = max(val.data for val in logits)
    exps = [(val - max_val).exp() for val in logits]
    total = sum(exps)
    return [e / total for e in exps]
\`\`\`

Note: this version works with \`Value\` objects — \`val - max_val\` uses \`Value.__sub__\` and \`.exp()\` uses \`Value.exp()\`. The operations record the computation graph for backpropagation.

### Cross-Entropy Connection

The model outputs logits → softmax → probabilities → \`-log(p_target)\` is the loss.

During inference we apply softmax again with a temperature:

\`\`\`python
probs = softmax([l / temperature for l in logits])
\`\`\`

### Your Task

Implement \`softmax(logits)\` where logits is a list of \`Value\` objects.`,

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

def softmax(logits):
    # TODO:
    # 1. Subtract max (for stability): max_val = max(val.data for val in logits)
    # 2. Exponentiate each shifted logit: (val - max_val).exp()
    # 3. Normalize by sum
    pass

logits = [Value(1.0), Value(2.0), Value(3.0)]
result = softmax(logits)
print(result.index(max(result, key=lambda v: v.data)))
print(round(sum(v.data for v in result), 10))
print(round(result[0].data, 4))
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

def softmax(logits):
    max_val = max(val.data for val in logits)
    exps = [(val - max_val).exp() for val in logits]
    total = sum(exps)
    return [e / total for e in exps]

logits = [Value(1.0), Value(2.0), Value(3.0)]
result = softmax(logits)
print(result.index(max(result, key=lambda v: v.data)))
print(round(sum(v.data for v in result), 10))
print(round(result[0].data, 4))
`,

	tests: [
		{
			name: "largest logit gets largest probability, outputs sum to 1",
			expected: "2\n1.0\n0.09\n",
		},
	],
};
