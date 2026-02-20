import type { Lesson } from "../../types";

export const treeNode: Lesson = {
	id: "tree-node",
	title: "The Node",
	chapterId: "binary-trees",
	content: `## Binary Trees

A **binary tree** is a data structure where each node has at most two children — a left child and a right child. Trees model hierarchical data: file systems, HTML documents, organization charts, and more.

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7
\`\`\`

### The Node Struct

Every node holds a value and two child pointers:

\`\`\`c
struct Node {
    int val;
    struct Node *left;
    struct Node *right;
};
\`\`\`

### Creating Nodes

Allocate each node with \`malloc\`. Set \`left\` and \`right\` to \`NULL\` — a \`NULL\` pointer means no child:

\`\`\`c
struct Node *new_node(int val) {
    struct Node *n = (struct Node *)malloc(sizeof(struct Node));
    n->val = val;
    n->left = NULL;
    n->right = NULL;
    return n;
}
\`\`\`

### Building a Tree

Link nodes together by assigning children:

\`\`\`c
struct Node *root = new_node(4);
root->left = new_node(2);
root->right = new_node(6);
root->left->left = new_node(1);
\`\`\`

### Your Task

Implement \`struct Node *new_node(int val)\` that allocates a node with the given value and \`NULL\` children. Create a root node with value \`10\` and print its value.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *left;
\tstruct Node *right;
};

struct Node *new_node(int val) {
\t// Allocate a Node, set val, left = NULL, right = NULL
\treturn NULL;
}

int main() {
\tstruct Node *root = new_node(10);
\tprintf("%d\\n", root->val);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *left;
\tstruct Node *right;
};

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->left = NULL;
\tn->right = NULL;
\treturn n;
}

int main() {
\tstruct Node *root = new_node(10);
\tprintf("%d\\n", root->val);
\treturn 0;
}
`,

	tests: [
		{
			name: "new_node(10) prints 10",
			expected: "10\n",
		},
		{
			name: "new_node(42)",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *n = new_node(42);
\tprintf("%d\\n", n->val);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "children are NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *n = new_node(7);
\tprintf("%d\\n", n->val);
\tprintf("%d\\n", n->left == NULL ? 1 : 0);
\tprintf("%d\\n", n->right == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "7\n1\n1\n",
		},
		{
			name: "multiple nodes",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->left = new_node(2);
\troot->right = new_node(3);
\tprintf("%d\\n", root->val);
\tprintf("%d\\n", root->left->val);
\tprintf("%d\\n", root->right->val);
\treturn 0;
}`,
			expected: "1\n2\n3\n",
		},
	],
};
