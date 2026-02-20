import type { Lesson } from "../../types";

export const bstMinMax: Lesson = {
	id: "bst-min-max",
	title: "Min and Max",
	chapterId: "bst",
	content: `## Finding Min and Max in a BST

The BST property tells you exactly where the minimum and maximum values live:

- **Minimum**: always in the **leftmost** node — keep going left until there's no left child.
- **Maximum**: always in the **rightmost** node — keep going right until there's no right child.

\`\`\`
        5
       / \\
      3   7
     / \\ / \\
    1  4 6  8
\`\`\`

Min = 1 (leftmost), Max = 8 (rightmost).

### Implementation

\`\`\`c
int bst_min(struct Node *root) {
    if (root->left == NULL) return root->val;
    return bst_min(root->left);
}

int bst_max(struct Node *root) {
    if (root->right == NULL) return root->val;
    return bst_max(root->right);
}
\`\`\`

No comparison needed — the BST structure guarantees the position of the extremes.

### Your Task

Implement \`int bst_min(struct Node *root)\` and \`int bst_max(struct Node *root)\`. Assume the tree is non-empty.`,

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

int bst_min(struct Node *root) {
\t// Return minimum value in BST
\treturn 0;
}

int bst_max(struct Node *root) {
\t// Return maximum value in BST
\treturn 0;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 8);
\tprintf("%d\\n", bst_min(root));
\tprintf("%d\\n", bst_max(root));
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

int bst_min(struct Node *root) {
\tif (root->left == NULL) return root->val;
\treturn bst_min(root->left);
}

int bst_max(struct Node *root) {
\tif (root->right == NULL) return root->val;
\treturn bst_max(root->right);
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 8);
\tprintf("%d\\n", bst_min(root));
\tprintf("%d\\n", bst_max(root));
\treturn 0;
}
`,

	tests: [
		{
			name: "min=1 and max=8",
			expected: "1\n8\n",
		},
		{
			name: "single node is both min and max",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 42);
\tprintf("%d\\n", bst_min(root));
\tprintf("%d\\n", bst_max(root));
\treturn 0;
}`,
			expected: "42\n42\n",
		},
		{
			name: "ascending inserts",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 10);
\troot = bst_insert(root, 20);
\troot = bst_insert(root, 30);
\tprintf("%d\\n", bst_min(root));
\tprintf("%d\\n", bst_max(root));
\treturn 0;
}`,
			expected: "10\n30\n",
		},
		{
			name: "min of left-heavy BST",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 50);
\troot = bst_insert(root, 30);
\troot = bst_insert(root, 10);
\troot = bst_insert(root, 60);
\tprintf("%d\\n", bst_min(root));
\treturn 0;
}`,
			expected: "10\n",
		},
	],
};
