import path from 'path';
import { fileURLToPath } from 'url';
import { HEADERS } from '@/lib/c/headers';
import { parseElf } from '@/lib/c/elf-parser';
import { disassemble } from '@/lib/c/arm64-disasm';
import { link, resolveBranchTargets } from '@/lib/c/linker';
import { C_RUNTIME } from '@/lib/c/runtime';
import { assemble } from '@/lib/arm64/assembler';
import { execute } from '@/lib/arm64/interpreter';

const tccPath = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../../public/tcc/tcc.js');
const createTCC = (await import(tccPath)).default;
const tccModule = await createTCC({ print: () => {}, printErr: () => {} });

try { tccModule.FS.mkdir('/usr'); } catch {}
try { tccModule.FS.mkdir('/usr/include'); } catch {}
try { tccModule.FS.mkdir('/tmp'); } catch {}
for (const [name, content] of Object.entries(HEADERS)) {
  tccModule.FS.writeFile(`/usr/include/${name}`, content);
}

const code = `
#include <stdio.h>
double dot(double ax, double ay, double az, double bx, double by, double bz) {
  return ax*bx + ay*by + az*bz;
}
int main() {
  printf("%.4f\\n", dot(1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
  return 0;
}
`;

tccModule.FS.writeFile('/tmp/input.c', code);
const exitCode = tccModule.callMain(['-c', '-nostdlib', '-I/usr/include', '-o', '/tmp/output.o', '/tmp/input.c']);
if (exitCode !== 0) { console.log('COMPILE FAILED'); process.exit(1); }
const elfBytes = tccModule.FS.readFile('/tmp/output.o');
const elf = parseElf(elfBytes);
if (!elf.textSection) { console.log('NO TEXT'); process.exit(1); }
const textIdx = elf.sections.indexOf(elf.textSection);
const disasmLines = disassemble(elf.textSection.data, elf.symbols, textIdx);
const dataLabels = new Map<string, number>();
for (const sym of elf.symbols) {
  if (sym.name) {
    const sec = elf.sections[sym.sectionIndex];
    if (sec && (sec.name === '.data' || sec.name === '.rodata' || sec.name === '.data.ro')) {
      dataLabels.set(sym.name, sym.value);
    }
  }
}
const linked = link(disasmLines, elf, dataLabels);
const asmText = resolveBranchTargets(linked);
console.log('=== Generated Assembly ===\n' + asmText.slice(0, 3000));

// Run it
const buildDataDirectives = (section: typeof elf.dataSection) => {
  if (!section || section.data.length === 0) return '';
  const sectionIdx = elf.sections.indexOf(section);
  const sectionSyms = elf.symbols.filter(s => s.sectionIndex === sectionIdx && s.name).sort((a, b) => a.value - b.value);
  let result = '';
  for (let si = 0; si < sectionSyms.length; si++) {
    const sym = sectionSyms[si];
    const nextOff = si + 1 < sectionSyms.length ? sectionSyms[si+1].value : section.data.length;
    const bytes = section.data.slice(sym.value, nextOff);
    result += `${sym.name.replace(/[^a-zA-Z0-9_]/g, '_')}:\n`;
    if (bytes.length > 0) {
      const byteStrs: string[] = [];
      for (const b of bytes) byteStrs.push(String(b));
      result += `\t.byte ${byteStrs.join(', ')}\n`;
    }
  }
  return result;
};
const dataDirectives = buildDataDirectives(elf.dataSection);
const rodataDirectives = buildDataDirectives(elf.rodataSection);
let dataSection = '';
if (dataDirectives || rodataDirectives) {
  dataSection = '.data\n' + dataDirectives + rodataDirectives;
}
let fullAsm = '';
if (dataSection) fullAsm += dataSection + '\n';
fullAsm += '.text\n' + asmText;
const fullSource = C_RUNTIME + '\n' + fullAsm;
const program = assemble(fullSource);
const result = execute(program);
console.log('\n=== Output ===');
console.log(JSON.stringify(result.stdout));
console.log('Error:', result.error || '(none)');
