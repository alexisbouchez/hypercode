import type { Lesson } from "../../types";

export const virtualAddress: Lesson = {
	id: "virtual-address",
	title: "Virtual Address Layout",
	chapterId: "memory",
	content: `## Virtual Addresses

Every process in Linux sees its own **virtual address space** — a flat 64-bit range from 0 to 2^64. The kernel maps these virtual addresses to physical RAM using the MMU (Memory Management Unit).

A virtual address is split into two fields:

\`\`\`
63        12 11       0
┌──────────┬──────────┐
│   VPN    │  Offset  │
│ (52 bits)│ (12 bits)│
└──────────┴──────────┘
\`\`\`

- **Offset** — position within the page (low \`page_bits\` bits)
- **VPN** (Virtual Page Number) — which page (high bits)

With 4KB pages (\`page_bits = 12\`), each page holds 4096 bytes. The offset selects a byte within that page; the VPN selects which page.

### Extracting the Fields

\`\`\`c
unsigned long offset = vaddr & ((1UL << page_bits) - 1);
unsigned long vpn    = vaddr >> page_bits;
\`\`\`

For \`vaddr = 0x5ABC\` with \`page_bits = 12\`:
- mask = \`(1 << 12) - 1\` = \`0xFFF\`
- offset = \`0x5ABC & 0xFFF\` = \`0xABC\`
- vpn = \`0x5ABC >> 12\` = \`0x5\`

### Your Task

Implement \`void parse_vaddr(unsigned long vaddr, int page_bits)\` that prints the VPN and offset in hexadecimal.`,

	starterCode: `#include <stdio.h>

void parse_vaddr(unsigned long vaddr, int page_bits) {
\t// Print VPN and offset in hex
}

int main() {
\tparse_vaddr(0x5ABC, 12);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void parse_vaddr(unsigned long vaddr, int page_bits) {
\tunsigned long offset = vaddr & ((1UL << page_bits) - 1);
\tunsigned long vpn    = vaddr >> page_bits;
\tprintf("VPN:    0x%x\\n", vpn);
\tprintf("Offset: 0x%x\\n", offset);
}

int main() {
\tparse_vaddr(0x5ABC, 12);
\treturn 0;
}
`,

	tests: [
		{
			name: "parses 0x5ABC with 4KB pages",
			expected: "VPN:    0x5\nOffset: 0xabc\n",
		},
		{
			name: "page-aligned address has zero offset",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tparse_vaddr(0x10000, 12);
\treturn 0;
}`,
			expected: "VPN:    0x10\nOffset: 0x0\n",
		},
		{
			name: "offset within first page",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tparse_vaddr(0x1234, 12);
\treturn 0;
}`,
			expected: "VPN:    0x1\nOffset: 0x234\n",
		},
		{
			name: "2KB pages (page_bits=11)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tparse_vaddr(0xABCD, 11);
\treturn 0;
}`,
			expected: "VPN:    0x15\nOffset: 0x3cd\n",
		},
	],
};
