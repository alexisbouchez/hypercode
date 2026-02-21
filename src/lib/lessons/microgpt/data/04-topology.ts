import type { Lesson } from "../../types";

export const topology: Lesson = {
	id: "topology",
	title: "Topological Sort",
	chapterId: "autograd",
	content: `## Ordering the Computation Graph

To compute gradients correctly, we need to visit nodes in **reverse topological order** — children before parents. That way, when we process a node's gradient, all downstream gradients are already accumulated.

### The Graph

Every Value is a node in a DAG (Directed Acyclic Graph). Edges point from children to parents — the arrows follow the computation forward.

For \`z = (a + b) * c\`:

\`\`\`
a ──┐
    ├── (a+b) ──┐
b ──┘           ├── z
c ──────────────┘
\`\`\`

To run backward from \`z\`, we need to visit \`z\` first (set its gradient to 1), then \`(a+b)\` and \`c\`, then \`a\` and \`b\`.

### DFS Post-Order

A DFS that appends each node **after** recursing its children produces topological order:

\`\`\`python
def build_topo(v):
    topo = []
    visited = set()

    def dfs(node):
        if node not in visited:
            visited.add(node)
            for child in node._children:
                dfs(child)
            topo.append(node)  # append AFTER children

    dfs(v)
    return topo
\`\`\`

The root (starting node) will always be **last** in the list.

### Your Task

Implement \`build_topo(v)\` and return the sorted list.`,

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

def build_topo(v):
    topo = []
    visited = set()
    def dfs(node):
        # TODO: DFS post-order traversal
        # 1. If node already visited, return
        # 2. Mark visited
        # 3. Recurse into each child
        # 4. Append node to topo
        pass
    dfs(v)
    return topo

a = Value(2.0)
b = Value(3.0)
c = Value(4.0)
z = (a + b) * c
topo = build_topo(z)
print(topo[-1].data)
print(len(topo))
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

def build_topo(v):
    topo = []
    visited = set()
    def dfs(node):
        if node not in visited:
            visited.add(node)
            for child in node._children:
                dfs(child)
            topo.append(node)
    dfs(v)
    return topo

a = Value(2.0)
b = Value(3.0)
c = Value(4.0)
z = (a + b) * c
topo = build_topo(z)
print(topo[-1].data)
print(len(topo))
`,

	tests: [
		{
			name: "root is last, all 5 nodes present",
			expected: "20.0\n5\n",
		},
	],
};
