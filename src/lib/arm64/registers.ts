import { STACK_TOP } from "./constants";

export class RegisterFile {
  // X0-X30 + XZR(31) stored as BigInt64 for 64-bit values
  private regs = new BigInt64Array(32);
  public pc = 0;
  // NZCV flags
  public n = false;
  public z = false;
  public c = false;
  public v = false;
  // SP is separate from XZR
  private _sp = BigInt(STACK_TOP);

  reset() {
    this.regs.fill(0n);
    this.pc = 0;
    this.n = false;
    this.z = false;
    this.c = false;
    this.v = false;
    this._sp = BigInt(STACK_TOP);
  }

  getX(reg: number): bigint {
    if (reg === 31) return 0n; // XZR
    return this.regs[reg];
  }

  setX(reg: number, value: bigint) {
    if (reg === 31) return; // XZR is hardwired to 0
    this.regs[reg] = value;
  }

  // 32-bit access (lower 32 bits, zero-extended on read, zero-extends on write)
  getW(reg: number): number {
    if (reg === 31) return 0; // WZR
    return Number(BigInt.asUintN(32, this.regs[reg]));
  }

  setW(reg: number, value: number) {
    if (reg === 31) return;
    this.regs[reg] = BigInt(value >>> 0);
  }

  getSP(): bigint {
    return this._sp;
  }

  setSP(value: bigint) {
    this._sp = value;
  }

  // Read register, treating reg 31 as SP (for load/store instructions)
  getXOrSP(reg: number): bigint {
    if (reg === 31) return this._sp;
    return this.regs[reg];
  }

  setXOrSP(reg: number, value: bigint) {
    if (reg === 31) {
      this._sp = value;
      return;
    }
    this.regs[reg] = value;
  }

  setFlags64(result: bigint) {
    const u = BigInt.asUintN(64, result);
    this.n = (u >> 63n) === 1n;
    this.z = u === 0n;
  }

  setFlagsAddition(a: bigint, b: bigint) {
    const result = a + b;
    const u64 = BigInt.asUintN(64, result);
    this.n = (u64 >> 63n) === 1n;
    this.z = u64 === 0n;
    // Carry: unsigned overflow
    this.c = BigInt.asUintN(65, BigInt.asUintN(64, a) + BigInt.asUintN(64, b)) > 0xFFFFFFFFFFFFFFFFn;
    // Overflow: signed overflow
    const sa = BigInt.asIntN(64, a);
    const sb = BigInt.asIntN(64, b);
    const sr = BigInt.asIntN(64, u64);
    this.v = (sa >= 0n && sb >= 0n && sr < 0n) || (sa < 0n && sb < 0n && sr >= 0n);
  }

  setFlagsSubtraction(a: bigint, b: bigint) {
    const result = a - b;
    const u64 = BigInt.asUintN(64, result);
    this.n = (u64 >> 63n) === 1n;
    this.z = u64 === 0n;
    // Carry for subtraction: a >= b (unsigned)
    this.c = BigInt.asUintN(64, a) >= BigInt.asUintN(64, b);
    // Overflow
    const sa = BigInt.asIntN(64, a);
    const sb = BigInt.asIntN(64, b);
    const sr = BigInt.asIntN(64, u64);
    this.v = (sa >= 0n && sb < 0n && sr < 0n) || (sa < 0n && sb >= 0n && sr >= 0n);
  }

  checkCondition(cond: number): boolean {
    switch (cond) {
      case 0: return this.z;                     // EQ
      case 1: return !this.z;                    // NE
      case 2: return this.c;                     // CS/HS
      case 3: return !this.c;                    // CC/LO
      case 4: return this.n;                     // MI
      case 5: return !this.n;                    // PL
      case 6: return this.v;                     // VS
      case 7: return !this.v;                    // VC
      case 8: return this.c && !this.z;          // HI
      case 9: return !this.c || this.z;          // LS
      case 10: return this.n === this.v;         // GE
      case 11: return this.n !== this.v;         // LT
      case 12: return !this.z && this.n === this.v; // GT
      case 13: return this.z || this.n !== this.v;  // LE
      case 14: return true;                      // AL
      default: return false;
    }
  }
}
