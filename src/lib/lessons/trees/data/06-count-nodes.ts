import type { Lesson } from "../../types";

export const countNodes: Lesson = {
	id: "count-nodes",
	title: "Count Nodes",
	chapterId: "tree-properties",
	content: `## Counting Nodes

To count how many nodes are in a tree, use the same recursive pattern:

\`\`\`c
int count_nodes(struct Node *root) {
    if (root == NULL) return 0;
    return 1 + count_nodes(root->left) + count_nodes(root->right);
}
\`\`\`

For the 7-node tree: 1 + count(left subtree of 3 nodes) + count(right subtree of 3 nodes) = 1 + 3 + 3 = **7**.

### Why This Works

Each recursive call counts a subtree. The base case returns 0 for empty subtrees. Each non-null node contributes exactly 1 to the total.

This is essentially the same structure as \`tree_sum\` — instead of summing values, you sum 1 per node.

### Your Task

Implement \`int count_nodes(struct Node *root)\` that returns the total number of nodes in the tree.`,

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

int count_nodes(struct Node *root) {
\t// Return total number of nodes
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
\tprintf("%d\\n", count_nodes(root));
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

int count_nodes(struct Node *root) {
\tif (root == NULL) return 0;
\treturn 1 + count_nodes(root->left) + count_nodes(root->right);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tprintf("%d\\n", count_nodes(root));
\treturn 0;
}
`,

	tests: [
		{
			name: "7-node tree has 7 nodes",
			expected: "7\n",
		},
		{
			name: "empty tree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_nodes(NULL));
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
\tstruct Node *root = new_node(1);
\tprintf("%d\\n", count_nodes(root));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "three-node tree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->left = new_node(2);
\troot->right = new_node(3);
\tprintf("%d\\n", count_nodes(root));
\treturn 0;
}`,
			expected: "3\n",
		},
	],
};
