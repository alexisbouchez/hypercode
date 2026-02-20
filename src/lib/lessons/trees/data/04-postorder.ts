import type { Lesson } from "../../types";

export const postorder: Lesson = {
	id: "postorder",
	title: "Postorder Traversal",
	chapterId: "binary-trees",
	content: `## Postorder Traversal

**Postorder**: left subtree → right subtree → current node

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7
\`\`\`

Postorder visits: **1 3 2 5 7 6 4**

The root comes **last**. Postorder is used when you need to process children before their parent — for example, freeing a tree (you free children before the parent), evaluating expression trees (compute operands before the operator), or deleting a directory (delete files before the folder).

### Implementation

\`\`\`c
void postorder(struct Node *root) {
    if (root == NULL) return;
    postorder(root->left);
    postorder(root->right);
    printf("%d\\n", root->val);  // visit root last
}
\`\`\`

### Summary of the Three Traversals

| Traversal | Order | Use |
|-----------|-------|-----|
| Inorder | left → root → right | sorted output from BST |
| Preorder | root → left → right | copy a tree |
| Postorder | left → right → root | free/delete a tree |

### Your Task

Implement \`void postorder(struct Node *root)\` that prints each node's value, visiting left then right subtrees before the current node.`,

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

void postorder(struct Node *root) {
\t// Visit left, visit right, then print root->val
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tpostorder(root);
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

void postorder(struct Node *root) {
\tif (root == NULL) return;
\tpostorder(root->left);
\tpostorder(root->right);
\tprintf("%d\\n", root->val);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tpostorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "postorder of 7-node tree",
			expected: "1\n3\n2\n5\n7\n6\n4\n",
		},
		{
			name: "single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(8);
\tpostorder(root);
\treturn 0;
}`,
			expected: "8\n",
		},
		{
			name: "root with two children",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(10);
\troot->left = new_node(5);
\troot->right = new_node(15);
\tpostorder(root);
\treturn 0;
}`,
			expected: "5\n15\n10\n",
		},
		{
			name: "right-only chain",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->right = new_node(2);
\troot->right->right = new_node(3);
\tpostorder(root);
\treturn 0;
}`,
			expected: "3\n2\n1\n",
		},
	],
};
