import {
  SYS_WRITE, SYS_EXIT,
  SYS_FORMAT_DOUBLE,
  SYS_SQRT, SYS_SIN, SYS_COS, SYS_FABS, SYS_FLOOR, SYS_CEIL,
  SYS_ATAN2, SYS_POW, SYS_LOG, SYS_EXP, SYS_TAN,
  SYS_ASIN, SYS_ACOS, SYS_ATAN,
  SYS_FMIN, SYS_FMAX, SYS_ROUND, SYS_TRUNC, SYS_LOG2, SYS_LOG10,
} from "./constants";
import type { RegisterFile } from "./registers";
import type { Memory } from "./memory";

export interface SyscallResult {
  exit: boolean;
  exitCode: number;
  stdout: string;
}

const decoder = new TextDecoder();
const encoder = new TextEncoder();

function bitsToDouble(bits: bigint): number {
  const buf = new ArrayBuffer(8);
  new DataView(buf).setBigUint64(0, BigInt.asUintN(64, bits), true);
  return new DataView(buf).getFloat64(0, true);
}

function doubleToBits(val: number): bigint {
  const buf = new ArrayBuffer(8);
  new DataView(buf).setFloat64(0, val, true);
  return new DataView(buf).getBigUint64(0, true);
}

export function handleSyscall(
  regs: RegisterFile,
  mem: Memory,
): SyscallResult {
  const syscallNum = Number(BigInt.asUintN(64, regs.getX(8)));
  const result: SyscallResult = { exit: false, exitCode: 0, stdout: "" };

  switch (syscallNum) {
    case SYS_WRITE: {
      const fd = Number(regs.getX(0));
      const bufAddr = Number(BigInt.asUintN(64, regs.getX(1)));
      const count = Number(BigInt.asUintN(64, regs.getX(2)));

      if (fd === 1 || fd === 2) {
        const bytes = mem.readBytes(bufAddr, count);
        result.stdout = decoder.decode(bytes);
      }

      regs.setX(0, BigInt(count));
      break;
    }

    case SYS_EXIT: {
      result.exit = true;
      result.exitCode = Number(BigInt.asIntN(64, regs.getX(0)));
      break;
    }

    case SYS_FORMAT_DOUBLE: {
      // X0 = bit pattern of double, X1 = precision, X2 = buffer address
      const bits = regs.getX(0);
      const precision = Number(BigInt.asUintN(64, regs.getX(1)));
      const bufAddr = Number(BigInt.asUintN(64, regs.getX(2)));
      const val = bitsToDouble(bits);
      const str = val.toFixed(Math.min(precision, 20));
      const bytes = encoder.encode(str);
      mem.writeBytes(bufAddr, bytes);
      regs.setX(0, BigInt(bytes.length));
      break;
    }

    case SYS_SQRT:
      regs.setX(0, doubleToBits(Math.sqrt(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_SIN:
      regs.setX(0, doubleToBits(Math.sin(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_COS:
      regs.setX(0, doubleToBits(Math.cos(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_FABS:
      regs.setX(0, doubleToBits(Math.abs(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_FLOOR:
      regs.setX(0, doubleToBits(Math.floor(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_CEIL:
      regs.setX(0, doubleToBits(Math.ceil(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_ATAN2: {
      // X0 = y bits, X1 = x bits
      const y = bitsToDouble(regs.getX(0));
      const x = bitsToDouble(regs.getX(1));
      regs.setX(0, doubleToBits(Math.atan2(y, x)));
      break;
    }
    case SYS_POW: {
      const base = bitsToDouble(regs.getX(0));
      const exp = bitsToDouble(regs.getX(1));
      regs.setX(0, doubleToBits(Math.pow(base, exp)));
      break;
    }
    case SYS_LOG:
      regs.setX(0, doubleToBits(Math.log(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_EXP:
      regs.setX(0, doubleToBits(Math.exp(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_TAN:
      regs.setX(0, doubleToBits(Math.tan(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_ASIN:
      regs.setX(0, doubleToBits(Math.asin(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_ACOS:
      regs.setX(0, doubleToBits(Math.acos(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_ATAN:
      regs.setX(0, doubleToBits(Math.atan(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_FMIN: {
      const a = bitsToDouble(regs.getX(0));
      const b = bitsToDouble(regs.getX(1));
      regs.setX(0, doubleToBits(Math.min(a, b)));
      break;
    }
    case SYS_FMAX: {
      const a = bitsToDouble(regs.getX(0));
      const b = bitsToDouble(regs.getX(1));
      regs.setX(0, doubleToBits(Math.max(a, b)));
      break;
    }
    case SYS_ROUND:
      regs.setX(0, doubleToBits(Math.round(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_TRUNC:
      regs.setX(0, doubleToBits(Math.trunc(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_LOG2:
      regs.setX(0, doubleToBits(Math.log2(bitsToDouble(regs.getX(0)))));
      break;
    case SYS_LOG10:
      regs.setX(0, doubleToBits(Math.log10(bitsToDouble(regs.getX(0)))));
      break;

    default:
      throw new Error(`Unsupported syscall: ${syscallNum}`);
  }

  return result;
}
