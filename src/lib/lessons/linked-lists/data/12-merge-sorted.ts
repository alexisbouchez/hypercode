import type { Lesson } from "../../types";

export const mergeSorted: Lesson = {
	id: "merge-sorted",
	title: "Merge Sorted Lists",
	chapterId: "classic",
	content: `## Merging Two Sorted Linked Lists

Given two sorted lists, produce one merged sorted list by splicing together existing nodes — no new allocation needed.

\`\`\`c
struct Node *merge_sorted(struct Node *a, struct Node *b) {
    struct Node dummy;
    struct Node *tail = &dummy;
    dummy.next = NULL;
    while (a != NULL && b != NULL) {
        if (a->val <= b->val) {
            tail->next = a;
            a = a->next;
        } else {
            tail->next = b;
            b = b->next;
        }
        tail = tail->next;
    }
    tail->next = (a != NULL) ? a : b;
    return dummy.next;
}
\`\`\`

### The Dummy Head Trick

A **dummy (sentinel) node** before the real head lets you avoid special-casing the first node. You always append to \`tail->next\` and advance \`tail\`. At the end, \`dummy.next\` is the real head.

### Draining the Remainder

When one list is exhausted, the other is already sorted — attach it directly with \`tail->next = a != NULL ? a : b\`.

### Example

\`\`\`
a: [1] -> [3] -> [5] -> NULL
b: [2] -> [4] -> [6] -> NULL

Compare 1 vs 2 → take 1   tail: dummy->[1]
Compare 3 vs 2 → take 2   tail: dummy->[1]->[2]
Compare 3 vs 4 → take 3   tail: dummy->[1]->[2]->[3]
Compare 5 vs 4 → take 4   tail: dummy->[1]->[2]->[3]->[4]
Compare 5 vs 6 → take 5   tail: ...->[5]
Compare (a done) vs 6 → attach b
Result: [1]->[2]->[3]->[4]->[5]->[6]->NULL
\`\`\`

### Your Task

Implement \`struct Node *merge_sorted(struct Node *a, struct Node *b)\` that merges two sorted lists and returns the head of the merged list.`,

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

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *merge_sorted(struct Node *a, struct Node *b) {
\t// Merge two sorted lists, return new head
\treturn NULL;
}

int main() {
\tstruct Node *a = NULL;
\ta = push_back(a, 1);
\ta = push_back(a, 3);
\ta = push_back(a, 5);
\tstruct Node *b = NULL;
\tb = push_back(b, 2);
\tb = push_back(b, 4);
\tb = push_back(b, 6);
\tstruct Node *merged = merge_sorted(a, b);
\tprint_list(merged);
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

void print_list(struct Node *head) {
\tstruct Node *cur = head;
\twhile (cur != NULL) { printf("%d\\n", cur->val); cur = cur->next; }
}

struct Node *merge_sorted(struct Node *a, struct Node *b) {
\tstruct Node dummy;
\tstruct Node *tail = &dummy;
\tdummy.next = NULL;
\twhile (a != NULL && b != NULL) {
\t\tif (a->val <= b->val) {
\t\t\ttail->next = a;
\t\t\ta = a->next;
\t\t} else {
\t\t\ttail->next = b;
\t\t\tb = b->next;
\t\t}
\t\ttail = tail->next;
\t}
\ttail->next = (a != NULL) ? a : b;
\treturn dummy.next;
}

int main() {
\tstruct Node *a = NULL;
\ta = push_back(a, 1);
\ta = push_back(a, 3);
\ta = push_back(a, 5);
\tstruct Node *b = NULL;
\tb = push_back(b, 2);
\tb = push_back(b, 4);
\tb = push_back(b, 6);
\tstruct Node *merged = merge_sorted(a, b);
\tprint_list(merged);
\treturn 0;
}
`,

	tests: [
		{
			name: "merge [1,3,5] and [2,4,6]",
			expected: "1\n2\n3\n4\n5\n6\n",
		},
		{
			name: "merge with empty list",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = NULL;
\ta = push_back(a, 1);
\ta = push_back(a, 2);
\ta = push_back(a, 3);
\tstruct Node *merged = merge_sorted(a, NULL);
\tprint_list(merged);
\treturn 0;
}`,
			expected: "1\n2\n3\n",
		},
		{
			name: "merge two single-element lists",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = new_node(5);
\tstruct Node *b = new_node(3);
\tstruct Node *merged = merge_sorted(a, b);
\tprint_list(merged);
\treturn 0;
}`,
			expected: "3\n5\n",
		},
		{
			name: "merge lists with duplicates",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tstruct Node *a = NULL;
\ta = push_back(a, 1);
\ta = push_back(a, 3);
\tstruct Node *b = NULL;
\tb = push_back(b, 1);
\tb = push_back(b, 3);
\tstruct Node *merged = merge_sorted(a, b);
\tprint_list(merged);
\treturn 0;
}`,
			expected: "1\n1\n3\n3\n",
		},
	],
};
