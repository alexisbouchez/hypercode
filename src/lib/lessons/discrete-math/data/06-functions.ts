import type { Lesson } from "../../types";

export const functions: Lesson = {
	id: "functions",
	title: "Functions",
	chapterId: "sets-relations-functions",
	content: `## Functions as Mappings

A **function** $f: A \\to B$ assigns to each element of $A$ (the **domain**) exactly one element of $B$ (the **codomain**). The **range** (or image) is $f(A) = \\{f(a) : a \\in A\\}$.

### Classification

| Name | Definition | Condition |
|------|------------|-----------|
| **Injective** (one-to-one) | $f(a) = f(b) \\Rightarrow a = b$ | distinct inputs → distinct outputs |
| **Surjective** (onto) | $\\forall b \\in B,\\ \\exists a,\\ f(a) = b$ | every output is reached |
| **Bijective** | injective and surjective | perfect pairing |

A bijection from $A$ to $B$ implies $|A| = |B|$.

\`\`\`python
def is_injective(f_dict):
    values = list(f_dict.values())
    return len(values) == len(set(values))

def is_surjective(f_dict, codomain):
    return set(f_dict.values()) == codomain

def is_bijective(f_dict, codomain):
    return is_injective(f_dict) and is_surjective(f_dict, codomain)

f = {1: 'a', 2: 'b', 3: 'c'}
print(is_bijective(f, {'a', 'b', 'c'}))  # True
\`\`\`

### Composition and Inverse

$(g \\circ f)(x) = g(f(x))$. A function $f$ has an inverse $f^{-1}$ if and only if it is bijective.

### Your Task

Implement \`is_injective\`, \`is_surjective\`, and \`is_bijective\` as shown above.`,

	starterCode: `def is_injective(f_dict):
    # Distinct inputs must map to distinct outputs
    values = list(f_dict.values())
    return len(values) == len(set(values))

def is_surjective(f_dict, codomain):
    # The image must equal the entire codomain
    pass

def is_bijective(f_dict, codomain):
    return is_injective(f_dict) and is_surjective(f_dict, codomain)

f = {1: 'a', 2: 'b', 3: 'c'}
print(is_bijective(f, {'a', 'b', 'c'}))  # True
`,

	solution: `def is_injective(f_dict):
    values = list(f_dict.values())
    return len(values) == len(set(values))

def is_surjective(f_dict, codomain):
    return set(f_dict.values()) == codomain

def is_bijective(f_dict, codomain):
    return is_injective(f_dict) and is_surjective(f_dict, codomain)

f = {1: 'a', 2: 'b', 3: 'c'}
print(is_bijective(f, {'a', 'b', 'c'}))
`,

	tests: [
		{
			name: "is_bijective({1:'a',2:'b',3:'c'}, {'a','b','c'}) = True",
			expected: "True\n",
		},
		{
			name: "is_injective({1:'a',2:'a',3:'c'}) = False (collision)",
			code: `{{FUNC}}
print(is_injective({1: 'a', 2: 'a', 3: 'c'}))`,
			expected: "False\n",
		},
		{
			name: "is_surjective({1:'a',2:'b'}, {'a','b'}) = True",
			code: `{{FUNC}}
print(is_surjective({1: 'a', 2: 'b'}, {'a', 'b'}))`,
			expected: "True\n",
		},
		{
			name: "is_injective({1:'a',2:'b',3:'c'}) = True",
			code: `{{FUNC}}
print(is_injective({1: 'a', 2: 'b', 3: 'c'}))`,
			expected: "True\n",
		},
	],
};
