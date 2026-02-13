import type { Lesson } from "../../types";

export const xorCipher: Lesson = {
  id: "xor-cipher",
  title: "XOR Cipher",
  chapterId: "bitwise",
  content: `## XOR Cipher

XOR (\`EOR\` in ARM64) has a special property that makes it perfect for simple encryption: **applying XOR twice with the same key recovers the original value**.

\`\`\`
A ^ K = E     (encrypt)
E ^ K = A     (decrypt)
\`\`\`

This works because XOR is its own inverse: \`(A ^ K) ^ K = A\`.

### How It Works

Given a plaintext message and a key byte, XOR each byte of the message with the key:

\`\`\`asm
LDRB W1, [X0], #1    // Load plaintext byte
EOR W1, W1, W2       // XOR with key
STRB W1, [X3], #1    // Store encrypted byte
\`\`\`

### Example

Encrypting 'H' (0x48) with key 0x2A:
\`\`\`
0x48 = 01001000
0x2A = 00101010
XOR  = 01100010 = 0x62
\`\`\`

Decrypting 0x62 with the same key:
\`\`\`
0x62 = 01100010
0x2A = 00101010
XOR  = 01001000 = 0x48 = 'H'
\`\`\`

### Decrypting a Message

To decrypt, iterate over the encrypted bytes, XOR each one with the key, and store the result:

\`\`\`asm
LDR X0, =encrypted
LDR X3, =output
MOV W2, #0x2A        // key

decrypt_loop:
    LDRB W1, [X0], #1
    CBZ W1, done
    EOR W1, W1, W2
    STRB W1, [X3], #1
    B decrypt_loop
\`\`\`

### Your Task

The \`.data\` section contains an encrypted message (5 bytes + null). Each byte has been XOR'd with the key \`0x2A\` (42). Decrypt it to reveal the hidden message and print it followed by a newline.

Hint: The decrypted message is \`ARM64\`.`,

  starterCode: `.data
encrypted:
\t.byte 0x6B, 0x78, 0x67, 0x1C, 0x1E, 0
output:
\t.skip 6

.text
.global _start
_start:
\t// Load encrypted data and output buffer addresses
\t// XOR each byte with key 0x2A until null terminator
\t// Store decrypted bytes in output buffer
\t// Add newline and print
`,

  solution: `.data
encrypted:
\t.byte 0x6B, 0x78, 0x67, 0x1C, 0x1E, 0
output:
\t.skip 6

.text
.global _start
_start:
\tLDR X0, =encrypted
\tLDR X3, =output
\tMOV W2, #0x2A
\tMOV X4, #0

decrypt_loop:
\tLDRB W1, [X0], #1
\tCBZ W1, decrypt_done
\tEOR W1, W1, W2
\tSTRB W1, [X3, X4]
\tADD X4, X4, #1
\tB decrypt_loop

decrypt_done:
\tMOV W5, #10
\tSTRB W5, [X3, X4]
\tADD X4, X4, #1

\tMOV X0, #1
\tLDR X1, =output
\tMOV X2, X4
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0
`,

  tests: [
    {
      name: "decrypts to ARM64",
      expected: "ARM64\n",
    },
  ],
};
