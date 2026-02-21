import { C_RUNTIME } from '@/lib/c/runtime';
import { assemble } from '@/lib/arm64/assembler';
import { execute } from '@/lib/arm64/interpreter';

// Test 1: does C_RUNTIME assemble without errors?
const prog = assemble(C_RUNTIME + '\n.text\nmain:\n\tMOV X0, #0\n\tRET\n');
console.log('C_RUNTIME assembles OK, instructions:', prog.instructions.length);
const printfPc = prog.labels.get('printf');
const printfFloatPc = prog.labels.get('printf_float');
console.log('printf at PC:', printfPc, '  printf_float at PC:', printfFloatPc);

// Check instructions around printf_float
if (printfFloatPc !== undefined) {
  for (let i = Math.max(0, printfFloatPc - 2); i < Math.min(printfFloatPc + 12, prog.instructions.length); i++) {
    const instr = prog.instructions[i];
    const ops = instr.operands.map(o => {
      if (o.kind === 'imm') return '#' + o.value.toString();
      if (o.kind === 'reg64') return `X${o.reg}${o.sp ? '(sp)' : ''}`;
      if (o.kind === 'reg32') return `W${o.reg}`;
      if (o.kind === 'mem') return `[X${o.base}+${o.offset}]`;
      if (o.kind === 'label') return o.name;
      return JSON.stringify(o);
    }).join(', ');
    console.log(`  [${i}] ${instr.op} ${ops}`);
  }
}

// Test 2: run a simple printf with FP
const testAsm = C_RUNTIME + `
.data
fmt_str:
\t.asciz "%.4f\\n"
const_32:
\t.byte 0,0,0,0,0,0,64,64

.text
main:
\tSTP X29, X30, [SP, #-16]!
\tMOV X29, SP
\t// load 32.0 into D0
\tLDR X0, =const_32
\t.word 0xFD400000
\t// load format string
\tLDR X0, =fmt_str
\tBL printf
\tMOV X0, #0
\tLDP X29, X30, [SP], #16
\tRET
`;

try {
  const prog2 = assemble(testAsm);
  const result = execute(prog2);
  console.log('\nTest with direct FP load:');
  console.log('  stdout:', JSON.stringify(result.stdout));
  console.log('  error:', result.error || '(none)');
} catch(e) {
  console.error('Error:', e instanceof Error ? e.message : e);
}
