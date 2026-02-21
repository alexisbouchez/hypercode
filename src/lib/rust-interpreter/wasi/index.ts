import WASI, { WASIProcExit } from "./wasi";
export { WASI, WASIProcExit };

export { Fd, Inode } from "./fd";
export {
  File,
  Directory,
  OpenFile,
  OpenDirectory,
  PreopenDirectory,
  ConsoleStdout,
} from "./fs_mem";
export { SyncOPFSFile, OpenSyncOPFSFile } from "./fs_opfs";
export { strace } from "./strace";
export * as wasi from "./wasi_defs";
