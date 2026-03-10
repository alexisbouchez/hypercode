import { STACK_TOP } from "./constants";

export class RegisterFile {
  // 16 general-purpose 64-bit registers: rax(0), rcx(1), rdx(2), rbx(3), rsp(4), rbp(5), rsi(6), rdi(7), r8-r15(8-15)
  private regs = new BigInt64Array(16);
  public rip = 0; // instruction pointer (index into instruction array)

  // RFLAGS bits we care about
  public cf = false; // carry flag
  public zf = false; // zero flag
  public sf = false; // sign flag
  public of = false; // overflow flag

  constructor() {
    this.regs[4] = BigInt(STACK_TOP); // rsp
  }

  reset() {
    this.regs.fill(0n);
    this.regs[4] = BigInt(STACK_TOP); // rsp
    this.rip = 0;
    this.cf = false;
    this.zf = false;
    this.sf = false;
    this.of = false;
  }

  get64(reg: number): bigint {
    return BigInt.asUintN(64, this.regs[reg]);
  }

  set64(reg: number, value: bigint) {
    this.regs[reg] = BigInt.asIntN(64, value);
  }

  get32(reg: number): number {
    return Number(BigInt.asUintN(32, this.regs[reg]));
  }

  // Writing to a 32-bit register zero-extends to 64 bits on x86_64
  set32(reg: number, value: number) {
    this.regs[reg] = BigInt(value >>> 0);
  }

  get16(reg: number): number {
    return Number(BigInt.asUintN(16, this.regs[reg]));
  }

  set16(reg: number, value: number) {
    const current = BigInt.asUintN(64, this.regs[reg]);
    const masked = current & ~0xFFFFn;
    this.regs[reg] = BigInt.asIntN(64, masked | BigInt(value & 0xFFFF));
  }

  get8(reg: number): number {
    if (reg >= 16) {
      // ah=16, ch=17, dh=18, bh=19 -> high byte of regs 0-3
      const actual = reg - 16;
      return Number((BigInt.asUintN(64, this.regs[actual]) >> 8n) & 0xFFn);
    }
    return Number(BigInt.asUintN(8, this.regs[reg]));
  }

  set8(reg: number, value: number) {
    if (reg >= 16) {
      // ah, ch, dh, bh
      const actual = reg - 16;
      const current = BigInt.asUintN(64, this.regs[actual]);
      const masked = current & ~0xFF00n;
      this.regs[actual] = BigInt.asIntN(64, masked | (BigInt(value & 0xFF) << 8n));
      return;
    }
    const current = BigInt.asUintN(64, this.regs[reg]);
    const masked = current & ~0xFFn;
    this.regs[reg] = BigInt.asIntN(64, masked | BigInt(value & 0xFF));
  }

  setFlagsArith64(result: bigint, a: bigint, b: bigint, isSub: boolean) {
    const u64 = BigInt.asUintN(64, result);
    this.sf = (u64 >> 63n) === 1n;
    this.zf = u64 === 0n;

    if (isSub) {
      // Carry for subtraction: borrow occurs when a < b (unsigned)
      this.cf = BigInt.asUintN(64, a) < BigInt.asUintN(64, b);
      const sa = BigInt.asIntN(64, a);
      const sb = BigInt.asIntN(64, b);
      const sr = BigInt.asIntN(64, u64);
      this.of = (sa >= 0n && sb < 0n && sr < 0n) || (sa < 0n && sb >= 0n && sr >= 0n);
    } else {
      // Carry for addition: unsigned overflow
      this.cf = BigInt.asUintN(65, BigInt.asUintN(64, a) + BigInt.asUintN(64, b)) > 0xFFFFFFFFFFFFFFFFn;
      const sa = BigInt.asIntN(64, a);
      const sb = BigInt.asIntN(64, b);
      const sr = BigInt.asIntN(64, u64);
      this.of = (sa >= 0n && sb >= 0n && sr < 0n) || (sa < 0n && sb < 0n && sr >= 0n);
    }
  }

  setFlagsLogic64(result: bigint) {
    const u64 = BigInt.asUintN(64, result);
    this.sf = (u64 >> 63n) === 1n;
    this.zf = u64 === 0n;
    this.cf = false;
    this.of = false;
  }
}
