import type { Lesson } from "../../types";

export const bstInsert: Lesson = {
	id: "bst-insert",
	title: "BST Insert",
	chapterId: "bst",
	content: `## Binary Search Tree

A **Binary Search Tree (BST)** is a binary tree with one rule:
- Every node's value is **greater** than all values in its left subtree.
- Every node's value is **less** than all values in its right subtree.

\`\`\`
        5
       / \\
      3   7
     / \\ / \\
    1  4 6  8
\`\`\`

This property makes search, insert, and delete operations efficient — each step cuts the remaining search space in half.

### Inserting a Value

To insert a value, walk down the tree following BST rules until you find an empty spot:

\`\`\`c
struct Node *bst_insert(struct Node *root, int val) {
    if (root == NULL) return new_node(val);
    if (val < root->val)
        root->left = bst_insert(root->left, val);
    else if (val > root->val)
        root->right = bst_insert(root->right, val);
    return root;
}
\`\`\`

Duplicates are ignored (neither left nor right branch taken).

### Verifying with Inorder

After inserting into a BST, an inorder traversal always yields sorted output:

Insert 5, 3, 7, 1, 4 → inorder gives 1, 3, 4, 5, 7.

### Your Task

Implement \`struct Node *bst_insert(struct Node *root, int val)\` that inserts \`val\` into the BST rooted at \`root\` and returns the (possibly new) root.`,

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
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

struct Node *bst_insert(struct Node *root, int val) {
\t// Insert val into the BST, return root
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
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

struct Node *bst_insert(struct Node *root, int val) {
\tif (root == NULL) return new_node(val);
\tif (val < root->val)
\t\troot->left = bst_insert(root->left, val);
\telse if (val > root->val)
\t\troot->right = bst_insert(root->right, val);
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
\tinorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "insert 5,3,7,1,4 → sorted inorder",
			expected: "1\n3\n4\n5\n7\n",
		},
		{
			name: "single insert",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 10);
\tinorder(root);
\treturn 0;
}`,
			expected: "10\n",
		},
		{
			name: "insert in reverse order",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 4);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 2);
\troot = bst_insert(root, 1);
\tinorder(root);
\treturn 0;
}`,
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "duplicates ignored",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 3);
\tinorder(root);
\treturn 0;
}`,
			expected: "3\n",
		},
	],
};
