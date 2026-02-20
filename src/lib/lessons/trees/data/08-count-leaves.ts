import type { Lesson } from "../../types";

export const countLeaves: Lesson = {
	id: "count-leaves",
	title: "Count Leaves",
	chapterId: "tree-properties",
	content: `## Counting Leaf Nodes

A **leaf node** is a node with no children — both \`left\` and \`right\` are \`NULL\`.

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7   ← these 4 are leaves
\`\`\`

### Implementation

\`\`\`c
int count_leaves(struct Node *root) {
    if (root == NULL) return 0;
    if (root->left == NULL && root->right == NULL) return 1;
    return count_leaves(root->left) + count_leaves(root->right);
}
\`\`\`

Three cases:
1. **NULL** — empty subtree, contributes 0 leaves.
2. **Leaf** — no children, contributes 1.
3. **Internal node** — has at least one child, leaves are in the subtrees.

### Your Task

Implement \`int count_leaves(struct Node *root)\` that returns the number of leaf nodes in the tree.`,

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

int count_leaves(struct Node *root) {
\t// Return number of nodes with no children
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
\tprintf("%d\\n", count_leaves(root));
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

int count_leaves(struct Node *root) {
\tif (root == NULL) return 0;
\tif (root->left == NULL && root->right == NULL) return 1;
\treturn count_leaves(root->left) + count_leaves(root->right);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tprintf("%d\\n", count_leaves(root));
\treturn 0;
}
`,

	tests: [
		{
			name: "7-node tree has 4 leaves",
			expected: "4\n",
		},
		{
			name: "single node is a leaf",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\tprintf("%d\\n", count_leaves(root));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "empty tree has 0 leaves",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_leaves(NULL));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "root with one child",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->left = new_node(2);
\tprintf("%d\\n", count_leaves(root));
\treturn 0;
}`,
			expected: "1\n",
		},
	],
};
