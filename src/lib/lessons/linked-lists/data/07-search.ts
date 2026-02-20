import type { Lesson } from "../../types";

export const search: Lesson = {
	id: "search",
	title: "Search",
	chapterId: "deletions",
	content: `## Searching a Linked List

To find a value, scan the list until you find it or reach the end:

\`\`\`c
int search(struct Node *head, int val) {
    struct Node *cur = head;
    while (cur != NULL) {
        if (cur->val == val) return 1;
        cur = cur->next;
    }
    return 0;
}
\`\`\`

Return \`1\` if found, \`0\` if not.

This is O(n) in the worst case — linked lists have no random access, so there is no shortcut.

### Comparison to Arrays

- **Array**: \`int a[n]\` → binary search O(log n) if sorted
- **Linked list**: must scan linearly O(n), even if sorted

The lack of random access is linked lists' main weakness. Their strength is O(1) insertion/deletion at the front.

### Your Task

Implement \`int search(struct Node *head, int val)\` that returns \`1\` if \`val\` is in the list, \`0\` otherwise.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *next;
};

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->next = NULL;
\treturn n;
}

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\treturn head;
}

int search(struct Node *head, int val) {
\t// Return 1 if val found, 0 otherwise
\treturn 0;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 10);
\thead = push_back(head, 20);
\thead = push_back(head, 30);
\tprintf("%d\\n", search(head, 20));
\tprintf("%d\\n", search(head, 99));
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

struct Node {
\tint val;
\tstruct Node *next;
};

struct Node *new_node(int val) {
\tstruct Node *n = (struct Node *)malloc(sizeof(struct Node));
\tn->val = val;
\tn->next = NULL;
\treturn n;
}

struct Node *push_back(struct Node *head, int val) {
\tstruct Node *n = new_node(val);
\tif (head == NULL) return n;
\tstruct Node *cur = head;
\twhile (cur->next != NULL) cur = cur->next;
\tcur->next = n;
\treturn head;
}

int search(struct Node *head, int val) {
\tstruct Node *cur = head;
\twhile (cur != NULL) {
\t\tif (cur->val == val) return 1;
\t\tcur = cur->next;
\t}
\treturn 0;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 10);
\thead = push_back(head, 20);
\thead = push_back(head, 30);
\tprintf("%d\\n", search(head, 20));
\tprintf("%d\\n", search(head, 99));
\treturn 0;
}
`,

	tests: [
		{
			name: "found=1, not found=0",
			expected: "1\n0\n",
		},
		{
			name: "search empty list = 0",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", search(NULL, 5));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "search first node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\tprintf("%d\\n", search(head, 1));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "search last node",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\tprintf("%d\\n", search(head, 3));
\treturn 0;
}`,
			expected: "1\n",
		},
	],
};
