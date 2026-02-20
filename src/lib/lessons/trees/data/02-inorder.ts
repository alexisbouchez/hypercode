import type { Lesson } from "../../types";

export const inorder: Lesson = {
	id: "inorder",
	title: "Inorder Traversal",
	chapterId: "binary-trees",
	content: `## Inorder Traversal

**Tree traversal** means visiting every node in a defined order. There are three classic traversal orders — inorder, preorder, and postorder — each with different uses.

**Inorder**: left subtree → current node → right subtree

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7
\`\`\`

Inorder visits: **1 2 3 4 5 6 7**

For a Binary Search Tree, inorder traversal always yields values in **sorted ascending order**.

### Implementation

Inorder traversal is naturally recursive:

\`\`\`c
void inorder(struct Node *root) {
    if (root == NULL) return;
    inorder(root->left);
    printf("%d\\n", root->val);
    inorder(root->right);
}
\`\`\`

The base case (\`root == NULL\`) handles empty subtrees — when a node has no left or right child, those recursive calls return immediately.

### Your Task

Implement \`void inorder(struct Node *root)\` that prints each node's value on its own line, visiting left subtree, then root, then right subtree.`,

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

void inorder(struct Node *root) {
\t// Visit left, print root->val, visit right
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tinorder(root);
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

void inorder(struct Node *root) {
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tinorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "inorder of 7-node tree",
			expected: "1\n2\n3\n4\n5\n6\n7\n",
		},
		{
			name: "single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(5);
\tinorder(root);
\treturn 0;
}`,
			expected: "5\n",
		},
		{
			name: "left-only subtree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(2);
\troot->left = new_node(1);
\tinorder(root);
\treturn 0;
}`,
			expected: "1\n2\n",
		},
		{
			name: "right-only subtree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(5);
\troot->right = new_node(7);
\tinorder(root);
\treturn 0;
}`,
			expected: "5\n7\n",
		},
	],
};
