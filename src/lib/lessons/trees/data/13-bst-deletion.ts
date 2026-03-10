import type { Lesson } from "../../types";

export const bstDeletion: Lesson = {
	id: "bst-deletion",
	title: "BST Deletion",
	chapterId: "bst-operations",
	content: `## Deleting from a BST

Deletion is the trickiest BST operation. There are three cases:

### Case 1: Leaf Node (no children)

Simply remove the node by returning NULL to the parent.

\`\`\`
Delete 4:       5            5
               / \\    →     / \\
              3   7        3   7
             / \\          /
            1   4        1
\`\`\`

### Case 2: One Child

Replace the node with its only child.

\`\`\`
Delete 3:       5            5
               / \\    →     / \\
              3   7        1   7
             /
            1
\`\`\`

### Case 3: Two Children

Find the **in-order successor** (smallest node in the right subtree), copy its value into the node being deleted, then recursively delete the successor.

\`\`\`
Delete 5:       5            6
               / \\    →     / \\
              3   7        3   7
             / \\ /        / \\   \\
            1  4 6       1   4   8
                  \\
                   8
\`\`\`

The in-order successor of 5 is 6 (leftmost node in right subtree).

### Finding the Minimum

A helper to find the minimum value in a subtree (used to locate the in-order successor):

\`\`\`c
int bst_min(struct Node *root) {
    while (root->left != NULL)
        root = root->left;
    return root->val;
}
\`\`\`

### Your Task

Implement \`struct Node *bst_delete(struct Node *root, int val)\` that deletes \`val\` from the BST and returns the new root.`,

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

struct Node *bst_insert(struct Node *root, int val) {
\tif (root == NULL) return new_node(val);
\tif (val < root->val)
\t\troot->left = bst_insert(root->left, val);
\telse if (val > root->val)
\t\troot->right = bst_insert(root->right, val);
\treturn root;
}

void inorder(struct Node *root) {
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

int bst_min(struct Node *root) {
\twhile (root->left != NULL)
\t\troot = root->left;
\treturn root->val;
}

struct Node *bst_delete(struct Node *root, int val) {
\t// Delete val from the BST, return root
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
\troot = bst_delete(root, 3);
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

struct Node *bst_insert(struct Node *root, int val) {
\tif (root == NULL) return new_node(val);
\tif (val < root->val)
\t\troot->left = bst_insert(root->left, val);
\telse if (val > root->val)
\t\troot->right = bst_insert(root->right, val);
\treturn root;
}

void inorder(struct Node *root) {
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

int bst_min(struct Node *root) {
\twhile (root->left != NULL)
\t\troot = root->left;
\treturn root->val;
}

struct Node *bst_delete(struct Node *root, int val) {
\tif (root == NULL) return NULL;
\tif (val < root->val) {
\t\troot->left = bst_delete(root->left, val);
\t} else if (val > root->val) {
\t\troot->right = bst_delete(root->right, val);
\t} else {
\t\tif (root->left == NULL) {
\t\t\tstruct Node *tmp = root->right;
\t\t\treturn tmp;
\t\t} else if (root->right == NULL) {
\t\t\tstruct Node *tmp = root->left;
\t\t\treturn tmp;
\t\t}
\t\tint succ = bst_min(root->right);
\t\troot->val = succ;
\t\troot->right = bst_delete(root->right, succ);
\t}
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
\troot = bst_delete(root, 3);
\tinorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "delete node with two children",
			expected: "1\n4\n5\n7\n",
		},
		{
			name: "delete leaf node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_delete(root, 3);
\tinorder(root);
\treturn 0;
}`,
			expected: "5\n7\n",
		},
		{
			name: "delete node with one child",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_delete(root, 3);
\tinorder(root);
\treturn 0;
}`,
			expected: "1\n5\n7\n",
		},
		{
			name: "delete root node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 6);
\troot = bst_insert(root, 8);
\troot = bst_delete(root, 5);
\tinorder(root);
\treturn 0;
}`,
			expected: "3\n6\n7\n8\n",
		},
	],
};
