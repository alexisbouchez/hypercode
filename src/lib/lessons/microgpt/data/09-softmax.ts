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

### Log-Sum-Exp Trick

When computing \`log(softmax(x))\` — which cross-entropy loss requires — the naive approach \`log(exp(x_i - max) / sum(exp(x_j - max)))\` can still lose precision. The **log-sum-exp** trick computes log-probabilities directly:

\`\`\`
log_softmax(x)[i] = (x[i] - max) - log(sum(exp(x[j] - max)))
\`\`\`

The term \`log(sum(exp(x[j] - max)))\` is the "log-sum-exp" and is computed once for all elements. This avoids ever materializing very small probabilities that would underflow to zero before the \`log\`:

\`\`\`python
def log_softmax(logits):
    max_val = max(val.data for val in logits)
    shifted = [val - max_val for val in logits]
    log_sum = sum(s.exp() for s in shifted).log()
    return [s - log_sum for s in shifted]
\`\`\`

In practice, frameworks like PyTorch fuse softmax + log into \`log_softmax\` for exactly this reason.

### Cross-Entropy Connection

The model outputs logits → softmax → probabilities → \`-log(p_target)\` is the loss. Using log-softmax, this becomes simply \`-log_softmax(logits)[target]\`, which is both numerically stable and efficient.

During inference we apply softmax again with a temperature:

\`\`\`python
probs = softmax([l / temperature for l in logits])
\`\`\`

### Your Task

Implement \`softmax(logits)\` and \`log_softmax(logits)\` where logits is a list of \`Value\` objects. Use the max-subtraction trick for stability in both.`,

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

def log_softmax(logits):
    # TODO:
    # 1. Subtract max for stability
    # 2. Compute log(sum(exp(shifted))) once
    # 3. Return shifted - log_sum for each element
    pass

logits = [Value(1.0), Value(2.0), Value(3.0)]
result = softmax(logits)
print(result.index(max(result, key=lambda v: v.data)))
print(round(sum(v.data for v in result), 10))
print(round(result[0].data, 4))
ls = log_softmax(logits)
print(round(ls[2].data, 4))
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

def log_softmax(logits):
    max_val = max(val.data for val in logits)
    shifted = [val - max_val for val in logits]
    log_sum = sum(s.exp() for s in shifted).log()
    return [s - log_sum for s in shifted]

logits = [Value(1.0), Value(2.0), Value(3.0)]
result = softmax(logits)
print(result.index(max(result, key=lambda v: v.data)))
print(round(sum(v.data for v in result), 10))
print(round(result[0].data, 4))
ls = log_softmax(logits)
print(round(ls[2].data, 4))
`,

	tests: [
		{
			name: "largest logit gets largest probability, outputs sum to 1, log_softmax works",
			expected: "2\n1.0\n0.09\n-0.4076\n",
		},
		{
			name: "numerical stability: large logits do not overflow",
			code: `{{FUNC}}
import math
# Large logits that would overflow with naive exp()
big = [Value(1000.0), Value(1001.0), Value(1002.0)]
sm = softmax(big)
# Should still sum to 1 and have correct relative probabilities
print(round(sum(v.data for v in sm), 10))
print(all(math.isfinite(v.data) for v in sm))
# log_softmax should also be finite
ls = log_softmax(big)
print(all(math.isfinite(v.data) for v in ls))
# Probabilities should match small-logit case (shift-invariant)
small = [Value(0.0), Value(1.0), Value(2.0)]
sm2 = softmax(small)
print(round(sm[0].data, 4) == round(sm2[0].data, 4))`,
			expected: "1.0\nTrue\nTrue\nTrue\n",
		},
	],
};
