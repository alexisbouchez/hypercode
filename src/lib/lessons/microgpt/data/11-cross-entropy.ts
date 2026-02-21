import type { Lesson } from "../../types";

export const crossEntropy: Lesson = {
	id: "cross-entropy",
	title: "Cross-Entropy Loss",
	chapterId: "primitives",
	content: `## The Loss Function

The model produces a probability for every token in the vocabulary. Cross-entropy loss measures how surprised the model is by the correct next token.

### Definition

\`\`\`
loss = -log(p_correct)
\`\`\`

If the model assigns probability 1.0 to the correct token: \`loss = -log(1) = 0\`.
If it assigns probability 0.01: \`loss = -log(0.01) ≈ 4.6\`.

The model is penalized more the lower the probability it gave to the right answer.

### Random Baseline

With a vocabulary of 27 (26 letters + BOS), a random model assigns \`1/27\` to each token:

\`\`\`
baseline loss = -log(1/27) ≈ 3.30
\`\`\`

MicroGPT starts around 3.5 (slightly worse than random due to random initialization) and should reach around 2.0 after 1000 training steps.

### Implementation

\`\`\`python
probs = softmax(logits)
loss = -probs[target_id].log()
\`\`\`

The \`Value.log()\` records the gradient: \`d(-log(p)) / dp = -1/p\`.

This is one loss per position. We average over all positions in a document:

\`\`\`python
loss = (1 / n) * sum(losses)
\`\`\`

### Your Task

Given logits as Value objects and a target index, compute the cross-entropy loss.`,

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
    max_val = max(val.data for val in logits)
    exps = [(val - max_val).exp() for val in logits]
    total = sum(exps)
    return [e / total for e in exps]

def cross_entropy(logits, target):
    # TODO:
    # 1. Apply softmax to get probabilities
    # 2. Return -probs[target].log()
    pass

logits = [Value(2.0), Value(4.0), Value(1.0)]
loss = cross_entropy(logits, target=1)
print(round(loss.data, 4))
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

def cross_entropy(logits, target):
    probs = softmax(logits)
    return -probs[target].log()

logits = [Value(2.0), Value(4.0), Value(1.0)]
loss = cross_entropy(logits, target=1)
print(round(loss.data, 4))  # -log(p_target) where target has high prob
`,

	tests: [
		{
			name: "cross-entropy loss for best logit is small",
			expected: "0.1698\n",
		},
	],
};
