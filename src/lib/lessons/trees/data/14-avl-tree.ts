import type { Lesson } from "../../types";

export const avlTree: Lesson = {
	id: "avl-tree",
	title: "AVL Tree",
	chapterId: "bst-operations",
	content: `## AVL Tree

An **AVL tree** is a self-balancing Binary Search Tree. After every insertion or deletion, the tree automatically rebalances itself so that the **balance factor** of every node stays within **-1, 0, or 1**.

### Balance Factor

The balance factor of a node is the height of its left subtree minus the height of its right subtree:

\`\`\`
balance(node) = height(left) - height(right)
\`\`\`

If \`balance > 1\`, the tree is **left-heavy**. If \`balance < -1\`, it is **right-heavy**. Either case requires a rotation.

### Right Rotation (Left-Heavy)

When a node is left-heavy and the left child is left-heavy or balanced, perform a **right rotation**:

\`\`\`
      30               20
     /        →       /  \\
    20               10   30
   /
  10
\`\`\`

\`\`\`c
struct Node *right_rotate(struct Node *y) {
    struct Node *x = y->left;
    struct Node *t = x->right;
    x->right = y;
    y->left = t;
    y->height = 1 + max(get_height(y->left), get_height(y->right));
    x->height = 1 + max(get_height(x->left), get_height(x->right));
    return x;
}
\`\`\`

### Left Rotation (Right-Heavy)

When a node is right-heavy and the right child is right-heavy or balanced, perform a **left rotation**:

\`\`\`
  10                   20
    \\        →        /  \\
     20             10   30
      \\
       30
\`\`\`

### Left-Right and Right-Left Cases

If the imbalance is a zig-zag (e.g., left child is right-heavy), first rotate the child, then rotate the node. These are the **Left-Right** and **Right-Left** double rotation cases.

### Insert with Rebalancing

After a standard BST insert, walk back up and fix any imbalanced node:

1. Update the node's height.
2. Compute the balance factor.
3. Apply the appropriate rotation if \`|balance| > 1\`.

### Your Task

Implement \`struct Node *avl_insert(struct Node *root, int val)\` that inserts \`val\` into an AVL tree and returns the new root, keeping the tree balanced after every insertion. Helper functions \`get_height\`, \`max\`, \`get_balance\`, \`right_rotate\`, and \`left_rotate\` are provided in the starter code.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tint height;
\tstruct Node *left;
\tstruct Node *right;
};

int max(int a, int b) {
\tif (a > b) return a;
\treturn b;
}

int get_height(struct Node *n) {
\tif (n == NULL) return 0;
\treturn n->height;
}

int get_balance(struct Node *n) {
\tif (n == NULL) return 0;
\treturn get_height(n->left) - get_height(n->right);
}

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->height = 1;
\tn->left = NULL;
\tn->right = NULL;
\treturn n;
}

struct Node *right_rotate(struct Node *y) {
\tstruct Node *x = y->left;
\tstruct Node *t = x->right;
\tx->right = y;
\ty->left = t;
\ty->height = 1 + max(get_height(y->left), get_height(y->right));
\tx->height = 1 + max(get_height(x->left), get_height(x->right));
\treturn x;
}

struct Node *left_rotate(struct Node *x) {
\tstruct Node *y = x->right;
\tstruct Node *t = y->left;
\ty->left = x;
\tx->right = t;
\tx->height = 1 + max(get_height(x->left), get_height(x->right));
\ty->height = 1 + max(get_height(y->left), get_height(y->right));
\treturn y;
}

void inorder(struct Node *root) {
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

struct Node *avl_insert(struct Node *root, int val) {
\t// Insert val into the AVL tree, return root
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = avl_insert(root, 30);
\troot = avl_insert(root, 20);
\troot = avl_insert(root, 10);
\tprintf("%d\\n", root->val);
\tinorder(root);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tint height;
\tstruct Node *left;
\tstruct Node *right;
};

int max(int a, int b) {
\tif (a > b) return a;
\treturn b;
}

int get_height(struct Node *n) {
\tif (n == NULL) return 0;
\treturn n->height;
}

int get_balance(struct Node *n) {
\tif (n == NULL) return 0;
\treturn get_height(n->left) - get_height(n->right);
}

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->height = 1;
\tn->left = NULL;
\tn->right = NULL;
\treturn n;
}

struct Node *right_rotate(struct Node *y) {
\tstruct Node *x = y->left;
\tstruct Node *t = x->right;
\tx->right = y;
\ty->left = t;
\ty->height = 1 + max(get_height(y->left), get_height(y->right));
\tx->height = 1 + max(get_height(x->left), get_height(x->right));
\treturn x;
}

struct Node *left_rotate(struct Node *x) {
\tstruct Node *y = x->right;
\tstruct Node *t = y->left;
\ty->left = x;
\tx->right = t;
\tx->height = 1 + max(get_height(x->left), get_height(x->right));
\ty->height = 1 + max(get_height(y->left), get_height(y->right));
\treturn y;
}

void inorder(struct Node *root) {
\tif (root == NULL) return;
\tinorder(root->left);
\tprintf("%d\\n", root->val);
\tinorder(root->right);
}

struct Node *avl_insert(struct Node *root, int val) {
\tif (root == NULL) return new_node(val);
\tif (val < root->val)
\t\troot->left = avl_insert(root->left, val);
\telse if (val > root->val)
\t\troot->right = avl_insert(root->right, val);
\telse
\t\treturn root;
\troot->height = 1 + max(get_height(root->left), get_height(root->right));
\tint balance = get_balance(root);
\tif (balance > 1 && val < root->left->val)
\t\treturn right_rotate(root);
\tif (balance < -1 && val > root->right->val)
\t\treturn left_rotate(root);
\tif (balance > 1 && val > root->left->val) {
\t\troot->left = left_rotate(root->left);
\t\treturn right_rotate(root);
\t}
\tif (balance < -1 && val < root->right->val) {
\t\troot->right = right_rotate(root->right);
\t\treturn left_rotate(root);
\t}
\treturn root;
}

int main() {
\tstruct Node *root = NULL;
\troot = avl_insert(root, 30);
\troot = avl_insert(root, 20);
\troot = avl_insert(root, 10);
\tprintf("%d\\n", root->val);
\tinorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "right rotation on left-left insert",
			expected: "20\n10\n20\n30\n",
		},
		{
			name: "left rotation on right-right insert",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = avl_insert(root, 10);
\troot = avl_insert(root, 20);
\troot = avl_insert(root, 30);
\tprintf("%d\\n", root->val);
\tinorder(root);
\treturn 0;
}`,
			expected: "20\n10\n20\n30\n",
		},
		{
			name: "left-right double rotation",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = avl_insert(root, 30);
\troot = avl_insert(root, 10);
\troot = avl_insert(root, 20);
\tprintf("%d\\n", root->val);
\tinorder(root);
\treturn 0;
}`,
			expected: "20\n10\n20\n30\n",
		},
		{
			name: "balanced after multiple inserts",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = avl_insert(root, 10);
\troot = avl_insert(root, 20);
\troot = avl_insert(root, 30);
\troot = avl_insert(root, 40);
\troot = avl_insert(root, 50);
\tprintf("%d\\n", root->val);
\tprintf("%d\\n", get_height(root));
\tinorder(root);
\treturn 0;
}`,
			expected: "20\n3\n10\n20\n30\n40\n50\n",
		},
	],
};
