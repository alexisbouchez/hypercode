import { MEMORY_SIZE, CODE_BASE } from "./constants";

export class Memory {
  private buf: ArrayBuffer;
  private view: DataView;

  constructor() {
    this.buf = new ArrayBuffer(MEMORY_SIZE);
    this.view = new DataView(this.buf);
  }

  reset() {
    new Uint8Array(this.buf).fill(0);
  }

  private offset(addr: number): number {
    const off = addr - CODE_BASE;
    if (off < 0 || off >= MEMORY_SIZE) {
      throw new Error(`Memory access out of bounds: 0x${addr.toString(16)}`);
    }
    return off;
  }

  readByte(addr: number): number {
    return this.view.getUint8(this.offset(addr));
  }

  writeByte(addr: number, value: number) {
    this.view.setUint8(this.offset(addr), value & 0xff);
  }

  read32(addr: number): number {
    return this.view.getInt32(this.offset(addr), true); // little-endian
  }

  write32(addr: number, value: number) {
    this.view.setInt32(this.offset(addr), value, true);
  }

  read64(addr: number): bigint {
    return this.view.getBigInt64(this.offset(addr), true);
  }

  write64(addr: number, value: bigint) {
    this.view.setBigInt64(this.offset(addr), value, true);
  }

  readU16(addr: number): number {
    return this.view.getUint16(this.offset(addr), true);
  }

  writeU16(addr: number, value: number) {
    this.view.setUint16(this.offset(addr), value & 0xffff, true);
  }

  readU32(addr: number): number {
    return this.view.getUint32(this.offset(addr), true);
  }

  writeU32(addr: number, value: number) {
    this.view.setUint32(this.offset(addr), value >>> 0, true);
  }

  // Write a block of bytes into memory starting at addr
  writeBytes(addr: number, data: Uint8Array) {
    const off = this.offset(addr);
    new Uint8Array(this.buf, off, data.length).set(data);
  }

  // Read a block of bytes from memory
  readBytes(addr: number, length: number): Uint8Array {
    const off = this.offset(addr);
    return new Uint8Array(this.buf.slice(off, off + length));
  }
}
