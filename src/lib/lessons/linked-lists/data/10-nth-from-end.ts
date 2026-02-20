import type { Lesson } from "../../types";

export const nthFromEnd: Lesson = {
	id: "nth-from-end",
	title: "Nth from End",
	chapterId: "classic",
	content: `## Finding the Nth Node from the End

To find the nth node from the end **in one pass**, use two pointers:

1. Advance **fast** by \`n\` steps
2. Move both **fast** and **slow** together until fast reaches NULL
3. **slow** is now at the nth node from the end

\`\`\`c
struct Node *nth_from_end(struct Node *head, int n) {
    struct Node *fast = head;
    struct Node *slow = head;
    while (n > 0 && fast != NULL) {
        fast = fast->next;
        n--;
    }
    if (fast == NULL && n > 0) return NULL; // out of bounds
    while (fast != NULL) {
        fast = fast->next;
        slow = slow->next;
    }
    return slow;
}
\`\`\`

### Why It Works

\`\`\`
List: [1] -> [2] -> [3] -> [4] -> [5] -> NULL    n=2

Step 1 — advance fast by 2:
  fast               → [3]
  slow               → [1]

Step 2 — move both until fast is NULL:
  fast [3]->[4]->[5]->NULL  (2 steps)
  slow [1]->[2]->[3]        (2 steps)

slow points to [3] — the 2nd node from the end ✓
\`\`\`

### Your Task

Implement \`struct Node *nth_from_end(struct Node *head, int n)\` that returns the nth node from the end (1-indexed), or \`NULL\` if out of bounds.`,

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

struct Node *nth_from_end(struct Node *head, int n) {
\t// Return the nth node from the end (1-indexed)
\treturn NULL;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\tprintf("%d\\n", nth_from_end(head, 1)->val);
\tprintf("%d\\n", nth_from_end(head, 3)->val);
\tprintf("%d\\n", nth_from_end(head, 5)->val);
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

struct Node *nth_from_end(struct Node *head, int n) {
\tstruct Node *fast = head;
\tstruct Node *slow = head;
\twhile (n > 0 && fast != NULL) {
\t\tfast = fast->next;
\t\tn--;
\t}
\tif (n > 0) return NULL;
\twhile (fast != NULL) {
\t\tfast = fast->next;
\t\tslow = slow->next;
\t}
\treturn slow;
}

int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 1);
\thead = push_back(head, 2);
\thead = push_back(head, 3);
\thead = push_back(head, 4);
\thead = push_back(head, 5);
\tprintf("%d\\n", nth_from_end(head, 1)->val);
\tprintf("%d\\n", nth_from_end(head, 3)->val);
\tprintf("%d\\n", nth_from_end(head, 5)->val);
\treturn 0;
}
`,

	tests: [
		{
			name: "1st, 3rd, 5th from end of [1,2,3,4,5]",
			expected: "5\n3\n1\n",
		},
		{
			name: "2nd from end",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = NULL;
\thead = push_back(head, 10);
\thead = push_back(head, 20);
\thead = push_back(head, 30);
\tprintf("%d\\n", nth_from_end(head, 2)->val);
\treturn 0;
}`,
			expected: "20\n",
		},
		{
			name: "out of bounds returns NULL",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(1);
\tprintf("%d\\n", nth_from_end(head, 5) == NULL ? 1 : 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "last node from single-element list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *head = new_node(42);
\tprintf("%d\\n", nth_from_end(head, 1)->val);
\treturn 0;
}`,
			expected: "42\n",
		},
	],
};
