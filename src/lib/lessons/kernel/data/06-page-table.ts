import type { Lesson } from "../../types";

export const pageTable: Lesson = {
	id: "page-table",
	title: "Page Table",
	chapterId: "memory",
	content: `## The Page Table

The **page table** is the kernel's map from virtual page numbers to physical frame numbers. For each VPN that is mapped, the page table entry (PTE) stores the **physical frame number** (PFN). The full physical address is:

\`\`\`
physical = (PFN << page_bits) | offset
\`\`\`

A PTE of 0 means the page is not mapped — accessing it triggers a **page fault**, and the kernel either loads the page from disk or kills the process with SIGSEGV.

### Single-Level Page Table

\`\`\`c
// pt[vpn] = PFN (0 if not mapped)
unsigned long pt[4] = {0, 5, 0, 7};
// VPN 1 → physical frame 5
// VPN 3 → physical frame 7
// VPN 0, 2 → not mapped (page fault)
\`\`\`

### Your Implementation

Write \`void translate(unsigned long pt[], int n, unsigned long vaddr)\` that prints the physical address, or \`"page fault"\` if the page is not mapped.

\`\`\`c
void translate(unsigned long pt[], int n, unsigned long vaddr) {
    int page_bits = 12;
    unsigned long vpn    = vaddr >> page_bits;
    unsigned long offset = vaddr & 0xFFF;

    if (vpn >= n || pt[vpn] == 0) {
        printf("page fault\\n");
        return;
    }
    printf("0x%x\\n", (pt[vpn] << page_bits) | offset);
}
\`\`\`

### Your Task

Implement \`translate\` that performs a single-level page table lookup using 4KB pages.`,

	starterCode: `#include <stdio.h>

void translate(unsigned long pt[], int n, unsigned long vaddr) {
\t// Look up vaddr in pt[]; print physical address or "page fault"
}

int main() {
\tunsigned long pt[4] = {0, 5, 0, 7};
\ttranslate(pt, 4, 0x1234);
\ttranslate(pt, 4, 0x0ABC);
\ttranslate(pt, 4, 0x3001);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void translate(unsigned long pt[], int n, unsigned long vaddr) {
\tint page_bits = 12;
\tunsigned long vpn    = vaddr >> page_bits;
\tunsigned long offset = vaddr & 0xFFF;

\tif (vpn >= n || pt[vpn] == 0) {
\t\tprintf("page fault\\n");
\t\treturn;
\t}
\tprintf("0x%x\\n", (pt[vpn] << page_bits) | offset);
}

int main() {
\tunsigned long pt[4] = {0, 5, 0, 7};
\ttranslate(pt, 4, 0x1234);
\ttranslate(pt, 4, 0x0ABC);
\ttranslate(pt, 4, 0x3001);
\treturn 0;
}
`,

	tests: [
		{
			name: "valid, fault, valid translations",
			expected: "0x5234\npage fault\n0x7001\n",
		},
		{
			name: "out-of-bounds VPN triggers page fault",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tunsigned long pt[2] = {0, 3};
\ttranslate(pt, 2, 0x5000);
\treturn 0;
}`,
			expected: "page fault\n",
		},
		{
			name: "page-aligned virtual address",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tunsigned long pt[3] = {0, 0, 9};
\ttranslate(pt, 3, 0x2000);
\treturn 0;
}`,
			expected: "0x9000\n",
		},
		{
			name: "offset preserved in physical address",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tunsigned long pt[2] = {0, 1};
\ttranslate(pt, 2, 0x1FFF);
\treturn 0;
}`,
			expected: "0x1fff\n",
		},
	],
};
