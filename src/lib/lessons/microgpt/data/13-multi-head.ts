import type { Lesson } from "../../types";

export const multiHead: Lesson = {
	id: "multi-head",
	title: "Multi-Head Attention",
	chapterId: "transformer",
	content: `## Multi-Head Attention

Instead of one attention computation, the Transformer runs **H independent heads** in parallel, each attending to a different subspace of the representation.

### Why Multiple Heads?

Different heads can specialize:
- Head 1 might track syntactic dependencies
- Head 2 might track semantic similarity
- Head 3 might track positional patterns

With \`n_embd=16\` and \`n_head=4\`, each head operates on a \`head_dim = 16/4 = 4\`-dimensional slice.

### The Split

Each head \`h\` operates on a slice of the Q, K, V vectors:

\`\`\`python
head_size = n_embd // n_head
hs = h * head_size
q_h = q[hs : hs + head_size]
k_h = [k[hs : hs + head_size] for k in all_keys]
v_h = [v[hs : hs + head_size] for v in all_values]
\`\`\`

### Concatenate and Project

The outputs of all H heads are concatenated back into a single vector, then projected with \`attn_wo\`:

\`\`\`python
x_attn = []
for h in range(n_head):
    hs = h * head_dim
    # ... compute head_out ...
    x_attn.extend(head_out)   # concatenate

x = linear(x_attn, attn_wo)  # project
\`\`\`

### Your Task

Implement \`multi_head_attention(q, keys, values, n_head, head_dim)\` that runs H heads and concatenates the results.`,

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

def multi_head_attention(q, keys, values, n_head, head_dim):
    # TODO:
    # For each head h in range(n_head):
    #   hs = h * head_dim
    #   q_h = q[hs : hs + head_dim]
    #   k_h = [k[hs : hs + head_dim] for k in keys]
    #   v_h = [v[hs : hs + head_dim] for v in values]
    #   scores = [sum(q_h[j]*k_h[t][j] for j) / head_dim**0.5 for t]
    #   weights = softmax(scores)
    #   head_out = [sum(weights[t]*v_h[t][j] for t) for j in range(head_dim)]
    #   extend output with head_out
    # Return concatenated output
    output = []
    pass

# n_embd=4, n_head=2, head_dim=2: two heads each of size 2
q  = [Value(1.0), Value(0.0), Value(0.0), Value(1.0)]
k0 = [Value(1.0), Value(0.0), Value(0.0), Value(1.0)]
v0 = [Value(2.0), Value(3.0), Value(4.0), Value(5.0)]

out = multi_head_attention(q, [k0], [v0], n_head=2, head_dim=2)
print(len(out))
print(round(out[0].data, 4))
print(round(out[2].data, 4))
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

def multi_head_attention(q, keys, values, n_head, head_dim):
    output = []
    for h in range(n_head):
        hs = h * head_dim
        q_h = q[hs:hs + head_dim]
        k_h = [k[hs:hs + head_dim] for k in keys]
        v_h = [v[hs:hs + head_dim] for v in values]
        scores = [sum(q_h[j] * k_h[t][j] for j in range(head_dim)) * head_dim**-0.5
                  for t in range(len(k_h))]
        weights = softmax(scores)
        head_out = [sum(weights[t] * v_h[t][j] for t in range(len(v_h)))
                    for j in range(head_dim)]
        output.extend(head_out)
    return output

q  = [Value(1.0), Value(0.0), Value(0.0), Value(1.0)]
k0 = [Value(1.0), Value(0.0), Value(0.0), Value(1.0)]
v0 = [Value(2.0), Value(3.0), Value(4.0), Value(5.0)]

out = multi_head_attention(q, [k0], [v0], n_head=2, head_dim=2)
print(len(out))
print(round(out[0].data, 4))
print(round(out[2].data, 4))
`,

	tests: [
		{
			name: "output length = n_embd, values are correctly attended",
			expected: "4\n2.0\n4.0\n",
		},
	],
};
