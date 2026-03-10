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
  // x86_64 Linux syscall convention:
  // rax = syscall number
  // rdi = arg1, rsi = arg2, rdx = arg3, r10 = arg4, r8 = arg5, r9 = arg6
  const syscallNum = Number(BigInt.asUintN(64, regs.get64(0))); // rax
  const result: SyscallResult = { exit: false, exitCode: 0, stdout: "" };

  switch (syscallNum) {
    case SYS_WRITE: {
      // sys_write(fd, buf, count) -> rdi, rsi, rdx
      const fd = Number(regs.get64(7));   // rdi
      const bufAddr = Number(BigInt.asUintN(64, regs.get64(6)));  // rsi
      const count = Number(BigInt.asUintN(64, regs.get64(2)));    // rdx

      if (fd === 1 || fd === 2) {
        const bytes = mem.readBytes(bufAddr, count);
        result.stdout = decoder.decode(bytes);
      }

      // Return number of bytes written in rax
      regs.set64(0, BigInt(count));
      break;
    }

    case SYS_EXIT: {
      // sys_exit(code) -> rdi
      result.exit = true;
      result.exitCode = Number(BigInt.asIntN(64, regs.get64(7))); // rdi
      break;
    }

    default:
      throw new Error(`Unsupported syscall: ${syscallNum}`);
  }

  return result;
}
