import type { Lesson } from "../../types";

export const treeSum: Lesson = {
	id: "tree-sum",
	title: "Sum of Values",
	chapterId: "binary-trees",
	content: `## Sum of All Node Values

To compute the sum of all values in a tree, use the same recursive pattern as traversal:

\`\`\`c
int tree_sum(struct Node *root) {
    if (root == NULL) return 0;
    return root->val + tree_sum(root->left) + tree_sum(root->right);
}
\`\`\`

For the tree below, sum = 1 + 2 + 3 + 4 + 5 + 6 + 7 = **28**:

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7
\`\`\`

### How It Works

1. Base case: empty tree (\`NULL\`) contributes 0.
2. Recursive case: this node's value + sum of left subtree + sum of right subtree.

The order of addition does not matter (addition is associative), so any traversal order works.

### Your Task

Implement \`int tree_sum(struct Node *root)\` that returns the sum of all node values.`,

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

int tree_sum(struct Node *root) {
\t// Return sum of all node values
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
\tprintf("%d\\n", tree_sum(root));
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

int tree_sum(struct Node *root) {
\tif (root == NULL) return 0;
\treturn root->val + tree_sum(root->left) + tree_sum(root->right);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tprintf("%d\\n", tree_sum(root));
\treturn 0;
}
`,

	tests: [
		{
			name: "sum of 7-node tree is 28",
			expected: "28\n",
		},
		{
			name: "empty tree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", tree_sum(NULL));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(5);
\tprintf("%d\\n", tree_sum(root));
\treturn 0;
}`,
			expected: "5\n",
		},
		{
			name: "unbalanced tree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(10);
\troot->left = new_node(3);
\troot->right = new_node(8);
\tprintf("%d\\n", tree_sum(root));
\treturn 0;
}`,
			expected: "21\n",
		},
	],
};
