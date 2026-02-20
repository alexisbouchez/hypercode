import type { Chapter, Lesson } from "../types";
import { pcb } from "./data/01-pcb";
import { processStates } from "./data/02-process-states";
import { fork } from "./data/03-fork";
import { scheduler } from "./data/04-scheduler";
import { virtualAddress } from "./data/05-virtual-address";
import { pageTable } from "./data/06-page-table";
import { slab } from "./data/07-slab";
import { inode } from "./data/08-inode";
import { fdTable } from "./data/09-fd-table";
import { fat } from "./data/10-fat";
import { ringBuffer } from "./data/11-ring-buffer";
import { semaphore } from "./data/12-semaphore";
import { signalTable } from "./data/13-signal-table";
import { syscallTable } from "./data/14-syscall-table";
import { writeSyscall } from "./data/15-write-syscall";

export const kernelChapters: Chapter[] = [
	{ id: "processes", title: "Processes" },
	{ id: "memory", title: "Memory" },
	{ id: "filesystem", title: "File System" },
	{ id: "ipc", title: "IPC" },
	{ id: "syscalls", title: "System Calls" },
];

export const kernelLessons: Lesson[] = [
	pcb,
	processStates,
	fork,
	scheduler,
	virtualAddress,
	pageTable,
	slab,
	inode,
	fdTable,
	fat,
	ringBuffer,
	semaphore,
	signalTable,
	syscallTable,
	writeSyscall,
];
