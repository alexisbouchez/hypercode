import type { Lesson } from "../../types";

export const preorder: Lesson = {
	id: "preorder",
	title: "Preorder Traversal",
	chapterId: "binary-trees",
	content: `## Preorder Traversal

**Preorder**: current node → left subtree → right subtree

\`\`\`
        4
       / \\
      2   6
     / \\ / \\
    1  3 5  7
\`\`\`

Preorder visits: **4 2 1 3 6 5 7**

Notice: the root comes **first**. Preorder is useful for copying a tree — if you insert nodes into a new tree in preorder, you get the same structure. It is also used in expression trees where operators precede their operands.

### Implementation

\`\`\`c
void preorder(struct Node *root) {
    if (root == NULL) return;
    printf("%d\\n", root->val);   // visit root first
    preorder(root->left);
    preorder(root->right);
}
\`\`\`

Compare with inorder: the only difference is where the \`printf\` line appears — before or after the recursive calls.

### Your Task

Implement \`void preorder(struct Node *root)\` that prints each node's value, visiting the current node first, then left, then right.`,

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

void preorder(struct Node *root) {
\t// Print root->val, then visit left, then right
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tpreorder(root);
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

void preorder(struct Node *root) {
\tif (root == NULL) return;
\tprintf("%d\\n", root->val);
\tpreorder(root->left);
\tpreorder(root->right);
}

int main() {
\tstruct Node *root = new_node(4);
\troot->left = new_node(2);
\troot->right = new_node(6);
\troot->left->left = new_node(1);
\troot->left->right = new_node(3);
\troot->right->left = new_node(5);
\troot->right->right = new_node(7);
\tpreorder(root);
\treturn 0;
}
`,

	tests: [
		{
			name: "preorder of 7-node tree",
			expected: "4\n2\n1\n3\n6\n5\n7\n",
		},
		{
			name: "single node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(9);
\tpreorder(root);
\treturn 0;
}`,
			expected: "9\n",
		},
		{
			name: "left-only chain",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *root = new_node(1);
\troot->left = new_node(2);
\troot->left->left = new_node(3);
\tpreorder(root);
\treturn 0;
}`,
			expected: "1\n2\n3\n",
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
\tpreorder(root);
\treturn 0;
}`,
			expected: "10\n5\n15\n",
		},
	],
};
