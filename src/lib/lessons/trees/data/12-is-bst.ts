import type { Lesson } from "../../types";

export const isBst: Lesson = {
	id: "is-bst",
	title: "Validate BST",
	chapterId: "bst-operations",
	content: `## Validating a BST

Given an arbitrary binary tree, how do you check if it satisfies the BST property?

A naive approach — checking that each node's left child is smaller and right child is larger — fails for this tree:

\`\`\`
        5
       / \\
      1   7
       \\
        6    ← 6 > 5, but it's in the left subtree!
\`\`\`

Each node must be greater than **everything** in its left subtree and less than **everything** in its right subtree.

### The Min-Max Trick

Pass a valid range \`[min, max]\` down the tree. Each node's value must fall strictly within this range:

\`\`\`c
int is_bst(struct Node *root, int min, int max) {
    if (root == NULL) return 1;
    if (root->val <= min || root->val >= max) return 0;
    return is_bst(root->left, min, root->val)
        && is_bst(root->right, root->val, max);
}
\`\`\`

Call with \`is_bst(root, -1000000, 1000000)\`.

Going left narrows the upper bound to \`root->val\`. Going right narrows the lower bound. This propagates the full BST constraint down every path.

### Your Task

Implement \`int is_bst(struct Node *root, int min, int max)\` that returns 1 if the subtree is a valid BST with all values in the range (min, max) exclusive, 0 otherwise.`,

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

int is_bst(struct Node *root, int min, int max) {
\t// Return 1 if valid BST with values in (min, max), else 0
\treturn 0;
}

int main() {
\t// Valid BST
\tstruct Node *valid = new_node(5);
\tvalid->left = new_node(3);
\tvalid->right = new_node(7);
\tvalid->left->left = new_node(1);
\tvalid->left->right = new_node(4);
\tprintf("%d\\n", is_bst(valid, -1000000, 1000000));

\t// Invalid: 6 is in left subtree of 5
\tstruct Node *invalid = new_node(5);
\tinvalid->left = new_node(1);
\tinvalid->left->right = new_node(6);
\tprintf("%d\\n", is_bst(invalid, -1000000, 1000000));
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

int is_bst(struct Node *root, int min, int max) {
\tif (root == NULL) return 1;
\tif (root->val <= min || root->val >= max) return 0;
\treturn is_bst(root->left, min, root->val)
\t\t&& is_bst(root->right, root->val, max);
}

int main() {
\t// Valid BST
\tstruct Node *valid = new_node(5);
\tvalid->left = new_node(3);
\tvalid->right = new_node(7);
\tvalid->left->left = new_node(1);
\tvalid->left->right = new_node(4);
\tprintf("%d\\n", is_bst(valid, -1000000, 1000000));

\t// Invalid: 6 is in left subtree of 5
\tstruct Node *invalid = new_node(5);
\tinvalid->left = new_node(1);
\tinvalid->left->right = new_node(6);
\tprintf("%d\\n", is_bst(invalid, -1000000, 1000000));
\treturn 0;
}
`,

	tests: [
		{
			name: "valid BST returns 1, invalid returns 0",
			expected: "1\n0\n",
		},
		{
			name: "empty tree is valid BST",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_bst(NULL, -1000000, 1000000));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "single node is valid BST",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(10);
\tprintf("%d\\n", is_bst(root, -1000000, 1000000));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "right child smaller than root is invalid",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(10);
\troot->right = new_node(5);
\tprintf("%d\\n", is_bst(root, -1000000, 1000000));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
