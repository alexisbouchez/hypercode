import type { Lesson } from "../../types";

export const bstSearch: Lesson = {
	id: "bst-search",
	title: "BST Search",
	chapterId: "bst",
	content: `## Searching a BST

The BST property makes search efficient. At each node, you only need to check one subtree:

\`\`\`
        5
       / \\
      3   7
     / \\ / \\
    1  4 6  8
\`\`\`

To search for **6**: start at 5. 6 > 5, go right to 7. 6 < 7, go left to 6. Found!

To search for **9**: 9 > 5, right. 9 > 7, right. 9 > 8, right → NULL. Not found.

Each step eliminates half the tree (on average). This is O(log n) for balanced trees.

### Implementation

\`\`\`c
int bst_search(struct Node *root, int val) {
    if (root == NULL) return 0;
    if (val == root->val) return 1;
    if (val < root->val) return bst_search(root->left, val);
    return bst_search(root->right, val);
}
\`\`\`

Returns 1 if found, 0 if not.

### Your Task

Implement \`int bst_search(struct Node *root, int val)\` that returns 1 if \`val\` is in the BST, 0 otherwise.`,

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

int bst_search(struct Node *root, int val) {
\t// Return 1 if val is in the BST, 0 otherwise
\treturn 0;
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
\tprintf("%d\\n", bst_search(root, 3));
\tprintf("%d\\n", bst_search(root, 6));
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

int bst_search(struct Node *root, int val) {
\tif (root == NULL) return 0;
\tif (val == root->val) return 1;
\tif (val < root->val) return bst_search(root->left, val);
\treturn bst_search(root->right, val);
}

int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 3);
\troot = bst_insert(root, 7);
\troot = bst_insert(root, 1);
\troot = bst_insert(root, 4);
\tprintf("%d\\n", bst_search(root, 3));
\tprintf("%d\\n", bst_search(root, 6));
\treturn 0;
}
`,

	tests: [
		{
			name: "search 3 (found) and 6 (not found)",
			expected: "1\n0\n",
		},
		{
			name: "search root value",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 10);
\troot = bst_insert(root, 5);
\troot = bst_insert(root, 15);
\tprintf("%d\\n", bst_search(root, 10));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "search in empty tree",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", bst_search(NULL, 5));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "search all inserted values",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = NULL;
\troot = bst_insert(root, 8);
\troot = bst_insert(root, 4);
\troot = bst_insert(root, 12);
\tprintf("%d\\n", bst_search(root, 4));
\tprintf("%d\\n", bst_search(root, 8));
\tprintf("%d\\n", bst_search(root, 12));
\tprintf("%d\\n", bst_search(root, 7));
\treturn 0;
}`,
			expected: "1\n1\n1\n0\n",
		},
	],
};
