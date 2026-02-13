import { SYS_WRITE, SYS_EXIT } from "./constants";
import type { RegisterFile } from "./registers";
import type { Memory } from "./memory";

export interface SyscallResult {
  exit: boolean;
  exitCode: number;
  stdout: string;
}

const decoder = new TextDecoder();

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
        // stdout or stderr
        const bytes = mem.readBytes(bufAddr, count);
        result.stdout = decoder.decode(bytes);
      }

      // Return bytes written in X0
      regs.setX(0, BigInt(count));
      break;
    }

    case SYS_EXIT: {
      result.exit = true;
      result.exitCode = Number(BigInt.asIntN(64, regs.getX(0)));
      break;
    }

    default:
      throw new Error(`Unsupported syscall: ${syscallNum}`);
  }

  return result;
}
