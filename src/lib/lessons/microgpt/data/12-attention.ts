import type { Lesson } from "../../types";

export const attention: Lesson = {
	id: "attention",
	title: "Attention",
	chapterId: "transformer",
	content: `## The Attention Mechanism

Attention is the core idea of the Transformer. It lets every token look at all previous tokens and decide which ones are relevant to its current representation.

### Queries, Keys, Values

Each token is projected into three vectors:
- **Query (Q)**: "What am I looking for?"
- **Key (K)**: "What do I contain?"
- **Value (V)**: "What do I send if selected?"

The output is a weighted sum of values, where the weight between Q and each K is determined by their dot product.

### Scaled Dot-Product Attention

\`\`\`
scores[t] = dot(Q, K[t]) / sqrt(head_dim)
weights = softmax(scores)
output[j] = sum(weights[t] * V[t][j] for t)
\`\`\`

We divide by \`sqrt(head_dim)\` to prevent the dot products from growing too large (which would saturate softmax, pushing weights to near-0 or near-1).

### In MicroGPT

At each new token position, Q is the current token's query. K and V accumulate over all previously seen tokens. This **KV-cache** enables efficient autoregressive generation — we never recompute old keys and values.

\`\`\`python
attn_logits = [sum(q_h[j] * k_h[t][j] for j in range(head_dim)) / head_dim**0.5
               for t in range(len(k_h))]
attn_weights = softmax(attn_logits)
head_out = [sum(attn_weights[t] * v_h[t][j] for t in range(len(v_h)))
            for j in range(head_dim)]
\`\`\`

### Your Task

Implement \`single_head_attention(q, keys, values, head_dim)\` that returns the attended output vector.`,

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

def single_head_attention(q, keys, values, head_dim):
    # TODO:
    # 1. Compute scores: dot(q, k) / sqrt(head_dim) for each k in keys
    # 2. Apply softmax to scores to get weights
    # 3. Compute output: weighted sum of values
    #    out[j] = sum(weights[t] * values[t][j] for t in range(len(values)))
    pass

q  = [Value(1.0), Value(0.0)]
k0 = [Value(1.0), Value(0.0)]
k1 = [Value(0.0), Value(1.0)]
v0 = [Value(1.0), Value(0.0)]
v1 = [Value(0.0), Value(1.0)]

out = single_head_attention(q, [k0, k1], [v0, v1], head_dim=2)
print(round(out[0].data, 4))
print(round(out[1].data, 4))
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

def single_head_attention(q, keys, values, head_dim):
    scores = [sum(q[j] * k[j] for j in range(head_dim)) * head_dim**-0.5
              for k in keys]
    weights = softmax(scores)
    out = [sum(weights[t] * values[t][j] for t in range(len(values)))
           for j in range(head_dim)]
    return out

q  = [Value(1.0), Value(0.0)]
k0 = [Value(1.0), Value(0.0)]
k1 = [Value(0.0), Value(1.0)]
v0 = [Value(1.0), Value(0.0)]
v1 = [Value(0.0), Value(1.0)]

out = single_head_attention(q, [k0, k1], [v0, v1], head_dim=2)
print(round(out[0].data, 4))
print(round(out[1].data, 4))
`,

	tests: [
		{
			name: "attention output is a weighted mix of values",
			expected: "0.6698\n0.3302\n",
		},
	],
};
