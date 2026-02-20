import type { Lesson } from "../../types";

export const height: Lesson = {
	id: "height",
	title: "Tree Height",
	chapterId: "tree-properties",
	content: `## Tree Height

The **height** of a tree is the number of nodes on the longest path from root to a leaf.

\`\`\`
        4         ← height 3
       / \\
      2   6       ← height 2
     / \\ / \\
    1  3 5  7    ← height 1 (leaves)
\`\`\`

Height of an empty tree is 0. Height of a single node is 1.

### Implementation

At each node, compute the height of both subtrees and take the larger one:

\`\`\`c
int height(struct Node *root) {
    if (root == NULL) return 0;
    int lh = height(root->left);
    int rh = height(root->right);
    return 1 + (lh > rh ? lh : rh);
}
\`\`\`

The ternary \`(lh > rh ? lh : rh)\` gives the maximum of the two heights. Adding 1 counts the current node.

### Example

For the 7-node tree: \`height(4)\` = 1 + max(\`height(2)\`, \`height(6)\`).
- \`height(2)\` = 1 + max(\`height(1)\`, \`height(3)\`) = 1 + max(1, 1) = 2
- \`height(6)\` = 1 + max(\`height(5)\`, \`height(7)\`) = 1 + max(1, 1) = 2

So \`height(4)\` = 1 + max(2, 2) = **3**.

### Your Task

Implement \`int height(struct Node *root)\` that returns the height of the tree.`,

	starterCode: `#include <stdio.h>
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

int height(struct Node *root) {
\t// Return height of the tree (0 for NULL, 1 for leaf)
\treturn 0;
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tprintf("%d\\n", height(root));
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

int height(struct Node *root) {
\tif (root == NULL) return 0;
\tint lh = height(root->left);
\tint rh = height(root->right);
\treturn 1 + (lh > rh ? lh : rh);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tprintf("%d\\n", height(root));
\treturn 0;
}
`,

	tests: [
		{
			name: "balanced 7-node tree has height 3",
			expected: "3\n",
		},
		{
			name: "empty tree has height 0",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", height(NULL));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "single node has height 1",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\tprintf("%d\\n", height(root));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "left-skewed chain of 4",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->left = new_node(2);
\troot->left->left = new_node(3);
\troot->left->left->left = new_node(4);
\tprintf("%d\\n", height(root));
\treturn 0;
}`,
			expected: "4\n",
		},
	],
};
