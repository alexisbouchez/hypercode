/**
 * In-browser Linux shell simulator.
 * Provides a virtual filesystem and executes common shell commands.
 * Pure TypeScript — no browser/Node APIs. Works in both environments.
 */

export interface ShellResult {
  stdout: string;
  stderr: string;
  error: string;
}

// ── Virtual filesystem ──────────────────────────────────────────────────────

type FSFile = { type: "file"; content: string; permissions: number };
type FSDir = {
  type: "dir";
  children: Record<string, FSNode>;
  permissions: number;
};
type FSNode = FSFile | FSDir;

function createInitialFS(): FSDir {
  return {
    type: "dir",
    permissions: 0o755,
    children: {
      home: {
        type: "dir",
        permissions: 0o755,
        children: {
          user: {
            type: "dir",
            permissions: 0o755,
            children: {
              "hello.txt": {
                type: "file",
                content: "Hello, Linux!\n",
                permissions: 0o644,
              },
              "notes.txt": {
                type: "file",
                content: "Learn Linux\nPractice daily\nHave fun\n",
                permissions: 0o644,
              },
              docs: {
                type: "dir",
                permissions: 0o755,
                children: {
                  "readme.txt": {
                    type: "file",
                    content: "This is a readme file.\n",
                    permissions: 0o644,
                  },
                },
              },
            },
          },
        },
      },
      etc: {
        type: "dir",
        permissions: 0o755,
        children: {
          hosts: {
            type: "file",
            content: "127.0.0.1 localhost\n::1 localhost\n",
            permissions: 0o644,
          },
        },
      },
      tmp: { type: "dir", permissions: 0o1777, children: {} },
      var: {
        type: "dir",
        permissions: 0o755,
        children: {
          log: {
            type: "dir",
            permissions: 0o755,
            children: {
              "app.log": {
                type: "file",
                content:
                  "INFO: Application started\nWARN: Low memory\nERROR: Connection failed\nINFO: Retry successful\n",
                permissions: 0o644,
              },
            },
          },
        },
      },
    },
  };
}

function cloneFS(node: FSNode): FSNode {
  if (node.type === "file") {
    return { type: "file", content: node.content, permissions: node.permissions };
  }
  const children: Record<string, FSNode> = {};
  for (const [k, v] of Object.entries(node.children)) {
    children[k] = cloneFS(v);
  }
  return { type: "dir", children, permissions: node.permissions };
}

// ── Path utilities ──────────────────────────────────────────────────────────

function normalizePath(p: string): string {
  const parts = p.split("/").filter(Boolean);
  const result: string[] = [];
  for (const part of parts) {
    if (part === "..") result.pop();
    else if (part !== ".") result.push(part);
  }
  return "/" + result.join("/");
}

function resolvePath(p: string, cwd: string): string {
  if (p === "~" || p === "") return "/home/user";
  if (p.startsWith("~/")) return normalizePath("/home/user/" + p.slice(2));
  if (p.startsWith("/")) return normalizePath(p);
  return normalizePath(cwd + "/" + p);
}

function getNode(fs: FSDir, path: string): FSNode | null {
  const parts = path.split("/").filter(Boolean);
  let current: FSNode = fs;
  for (const part of parts) {
    if (current.type !== "dir") return null;
    const child: FSNode | undefined = current.children[part];
    if (!child) return null;
    current = child;
  }
  return current;
}

function getParentDir(
  fs: FSDir,
  path: string
): { dir: FSDir; name: string } | null {
  const parts = path.split("/").filter(Boolean);
  if (parts.length === 0) return null;
  const name = parts[parts.length - 1];
  const parentPath = "/" + parts.slice(0, -1).join("/");
  const parentNode = getNode(fs, parentPath || "/");
  if (!parentNode || parentNode.type !== "dir") return null;
  return { dir: parentNode, name };
}

function setNode(fs: FSDir, path: string, node: FSNode): boolean {
  const result = getParentDir(fs, path);
  if (!result) return false;
  result.dir.children[result.name] = node;
  return true;
}

function deleteNode(fs: FSDir, path: string): boolean {
  const result = getParentDir(fs, path);
  if (!result) return false;
  if (!(result.name in result.dir.children)) return false;
  delete result.dir.children[result.name];
  return true;
}

// ── Shell word splitting & variable expansion ────────────────────────────────

function readVar(
  str: string,
  pos: number,
  vars: Record<string, string>
): { value: string; end: number } {
  if (pos >= str.length) return { value: "$", end: pos };
  if (str[pos] === "{") {
    const close = str.indexOf("}", pos + 1);
    if (close === -1) return { value: "${", end: pos + 1 };
    const name = str.slice(pos + 1, close);
    return { value: vars[name] ?? "", end: close + 1 };
  }
  let end = pos;
  while (end < str.length && /\w/.test(str[end])) end++;
  if (end === pos) return { value: "$", end: pos };
  const name = str.slice(pos, end);
  return { value: vars[name] ?? "", end };
}

/** Parse a string into shell words with variable expansion and quote handling. */
function shellSplit(str: string, vars: Record<string, string>): string[] {
  const words: string[] = [];
  let i = 0;
  let current = "";
  let inWord = false;

  while (i < str.length) {
    const ch = str[i];

    if (ch === '"') {
      inWord = true;
      i++;
      while (i < str.length && str[i] !== '"') {
        if (str[i] === "\\") {
          i++;
          if (i < str.length) { current += str[i]; i++; }
        } else if (str[i] === "$") {
          const { value, end } = readVar(str, i + 1, vars);
          current += value;
          i = end;
        } else {
          current += str[i++];
        }
      }
      i++; // closing "
    } else if (ch === "'") {
      inWord = true;
      i++;
      while (i < str.length && str[i] !== "'") current += str[i++];
      i++; // closing '
    } else if (ch === "$") {
      inWord = true;
      const { value, end } = readVar(str, i + 1, vars);
      current += value;
      i = end;
    } else if (ch === " " || ch === "\t") {
      if (inWord) {
        words.push(current);
        current = "";
        inWord = false;
      }
      i++;
    } else {
      inWord = true;
      current += ch;
      i++;
    }
  }

  if (inWord) words.push(current);
  return words;
}

/** Expand $VAR / ${VAR} in a string without splitting. */
function expandVars(str: string, vars: Record<string, string>): string {
  return shellSplit(str, vars).join(" ");
}

// ── Pipeline / redirect splitting ───────────────────────────────────────────

/** Split a command string by `|` respecting quotes. */
function splitByPipe(line: string): string[] {
  const parts: string[] = [];
  let current = "";
  let inSingle = false;
  let inDouble = false;

  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === "'" && !inDouble) { inSingle = !inSingle; current += ch; }
    else if (ch === '"' && !inSingle) { inDouble = !inDouble; current += ch; }
    else if (ch === "|" && !inSingle && !inDouble) {
      parts.push(current.trim());
      current = "";
    } else {
      current += ch;
    }
  }
  if (current.trim()) parts.push(current.trim());
  return parts;
}

/** Extract output redirection from a command line. Returns the clean command and redirect info. */
function extractRedirect(
  line: string
): { cmd: string; file: string | null; append: boolean } {
  // Match >> before >
  const appendMatch = line.match(/^(.*?)\s*>>\s*([^\s>]+)\s*$/);
  if (appendMatch) {
    return { cmd: appendMatch[1].trim(), file: appendMatch[2], append: true };
  }
  const writeMatch = line.match(/^(.*?)\s*>\s*([^\s>]+)\s*$/);
  if (writeMatch) {
    return { cmd: writeMatch[1].trim(), file: writeMatch[2], append: false };
  }
  return { cmd: line, file: null, append: false };
}

// ── Command implementations ─────────────────────────────────────────────────

interface CmdResult {
  stdout: string;
  stderr: string;
}

function cmdEcho(args: string[]): CmdResult {
  let flags = "";
  let i = 0;
  while (i < args.length && args[i].startsWith("-") && /^-[neE]+$/.test(args[i])) {
    flags += args[i].slice(1);
    i++;
  }
  const text = args.slice(i).join(" ");
  const noNewline = flags.includes("n");
  let out = text;
  if (flags.includes("e")) {
    out = out.replace(/\\n/g, "\n").replace(/\\t/g, "\t").replace(/\\\\/g, "\\");
  }
  return { stdout: out + (noNewline ? "" : "\n"), stderr: "" };
}

function cmdLs(args: string[], cwd: string, fs: FSDir): CmdResult {
  let showHidden = false;
  let showLong = false;
  let target = cwd;

  for (const arg of args) {
    if (arg.startsWith("-")) {
      if (arg.includes("a")) showHidden = true;
      if (arg.includes("l")) showLong = true;
    } else {
      target = resolvePath(arg, cwd);
    }
  }

  const node = getNode(fs, target);
  if (!node) {
    return { stdout: "", stderr: `ls: cannot access '${target}': No such file or directory\n` };
  }

  if (node.type === "file") {
    return { stdout: target.split("/").pop() + "\n", stderr: "" };
  }

  let entries = Object.keys(node.children).sort();
  if (!showHidden) entries = entries.filter((e) => !e.startsWith("."));

  if (showLong) {
    const lines = entries.map((e) => {
      const child = node.children[e];
      const isDir = child.type === "dir";
      const p = child.permissions;
      const perms =
        (isDir ? "d" : "-") +
        ((p >> 6) & 4 ? "r" : "-") +
        ((p >> 6) & 2 ? "w" : "-") +
        ((p >> 6) & 1 ? "x" : "-") +
        ((p >> 3) & 4 ? "r" : "-") +
        ((p >> 3) & 2 ? "w" : "-") +
        ((p >> 3) & 1 ? "x" : "-") +
        (p & 4 ? "r" : "-") +
        (p & 2 ? "w" : "-") +
        (p & 1 ? "x" : "-");
      const size = isDir ? 0 : (child as FSFile).content.length;
      return `${perms} 1 user user ${String(size).padStart(6)} Jan  1 00:00 ${e}`;
    });
    return { stdout: lines.join("\n") + (lines.length > 0 ? "\n" : ""), stderr: "" };
  }

  return { stdout: entries.join("\n") + (entries.length > 0 ? "\n" : ""), stderr: "" };
}

function cmdCd(args: string[], state: ShellState): CmdResult {
  const target = args[0] ? resolvePath(args[0], state.cwd) : "/home/user";
  const node = getNode(state.fs, target);
  if (!node) return { stdout: "", stderr: `cd: ${target}: No such file or directory\n` };
  if (node.type !== "dir") return { stdout: "", stderr: `cd: ${target}: Not a directory\n` };
  state.cwd = target;
  return { stdout: "", stderr: "" };
}

function cmdCat(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  if (args.length === 0) return { stdout: stdin, stderr: "" };
  let out = "";
  for (const arg of args) {
    const path = resolvePath(arg, cwd);
    const node = getNode(fs, path);
    if (!node) return { stdout: out, stderr: `cat: ${arg}: No such file or directory\n` };
    if (node.type === "dir") return { stdout: out, stderr: `cat: ${arg}: Is a directory\n` };
    out += node.content;
  }
  return { stdout: out, stderr: "" };
}

function cmdMkdir(args: string[], cwd: string, fs: FSDir): CmdResult {
  let createParents = false;
  const dirs: string[] = [];
  for (const arg of args) {
    if (arg === "-p") createParents = true;
    else dirs.push(arg);
  }
  for (const dir of dirs) {
    const path = resolvePath(dir, cwd);
    if (createParents) {
      const parts = path.split("/").filter(Boolean);
      let cur: FSNode = fs;
      for (const part of parts) {
        if (cur.type !== "dir") return { stdout: "", stderr: `mkdir: cannot create directory '${dir}'\n` };
        if (!cur.children[part]) {
          cur.children[part] = { type: "dir", permissions: 0o755, children: {} };
        }
        cur = cur.children[part];
      }
    } else {
      const result = getParentDir(fs, path);
      if (!result) return { stdout: "", stderr: `mkdir: cannot create directory '${dir}': No such file or directory\n` };
      if (result.dir.children[result.name]) {
        return { stdout: "", stderr: `mkdir: cannot create directory '${dir}': File exists\n` };
      }
      result.dir.children[result.name] = { type: "dir", permissions: 0o755, children: {} };
    }
  }
  return { stdout: "", stderr: "" };
}

function cmdTouch(args: string[], cwd: string, fs: FSDir): CmdResult {
  for (const arg of args) {
    const path = resolvePath(arg, cwd);
    const existing = getNode(fs, path);
    if (existing) continue;
    const result = getParentDir(fs, path);
    if (!result) return { stdout: "", stderr: `touch: cannot touch '${arg}': No such file or directory\n` };
    result.dir.children[result.name] = { type: "file", content: "", permissions: 0o644 };
  }
  return { stdout: "", stderr: "" };
}

function cmdRm(args: string[], cwd: string, fs: FSDir): CmdResult {
  let recursive = false;
  let force = false;
  const files: string[] = [];
  for (const arg of args) {
    if (arg.startsWith("-")) {
      if (arg.includes("r") || arg.includes("R")) recursive = true;
      if (arg.includes("f")) force = true;
    } else {
      files.push(arg);
    }
  }
  for (const file of files) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node) {
      if (force) continue;
      return { stdout: "", stderr: `rm: cannot remove '${file}': No such file or directory\n` };
    }
    if (node.type === "dir" && !recursive) {
      return { stdout: "", stderr: `rm: cannot remove '${file}': Is a directory\n` };
    }
    deleteNode(fs, path);
  }
  return { stdout: "", stderr: "" };
}

function cmdRmdir(args: string[], cwd: string, fs: FSDir): CmdResult {
  for (const arg of args) {
    const path = resolvePath(arg, cwd);
    const node = getNode(fs, path);
    if (!node) return { stdout: "", stderr: `rmdir: failed to remove '${arg}': No such file or directory\n` };
    if (node.type !== "dir") return { stdout: "", stderr: `rmdir: failed to remove '${arg}': Not a directory\n` };
    if (Object.keys(node.children).length > 0) {
      return { stdout: "", stderr: `rmdir: failed to remove '${arg}': Directory not empty\n` };
    }
    deleteNode(fs, path);
  }
  return { stdout: "", stderr: "" };
}

function cmdCp(args: string[], cwd: string, fs: FSDir): CmdResult {
  let recursive = false;
  const operands: string[] = [];
  for (const arg of args) {
    if (arg === "-r" || arg === "-R" || arg === "-a") recursive = true;
    else operands.push(arg);
  }
  if (operands.length < 2) return { stdout: "", stderr: "cp: missing destination\n" };
  const src = resolvePath(operands[0], cwd);
  const dst = resolvePath(operands[1], cwd);
  const srcNode = getNode(fs, src);
  if (!srcNode) return { stdout: "", stderr: `cp: cannot stat '${operands[0]}': No such file or directory\n` };
  if (srcNode.type === "dir" && !recursive) {
    return { stdout: "", stderr: `cp: -r not specified; omitting directory '${operands[0]}'\n` };
  }
  const dstNode = getNode(fs, dst);
  let finalDst = dst;
  if (dstNode && dstNode.type === "dir") {
    finalDst = normalizePath(dst + "/" + src.split("/").pop()!);
  }
  setNode(fs, finalDst, cloneFS(srcNode));
  return { stdout: "", stderr: "" };
}

function cmdMv(args: string[], cwd: string, fs: FSDir): CmdResult {
  if (args.length < 2) return { stdout: "", stderr: "mv: missing destination\n" };
  const src = resolvePath(args[0], cwd);
  const dst = resolvePath(args[1], cwd);
  const srcNode = getNode(fs, src);
  if (!srcNode) return { stdout: "", stderr: `mv: cannot stat '${args[0]}': No such file or directory\n` };
  const dstNode = getNode(fs, dst);
  let finalDst = dst;
  if (dstNode && dstNode.type === "dir") {
    finalDst = normalizePath(dst + "/" + src.split("/").pop()!);
  }
  setNode(fs, finalDst, srcNode);
  deleteNode(fs, src);
  return { stdout: "", stderr: "" };
}

function cmdHead(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let n = 10;
  let file: string | null = null;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "-n" && i + 1 < args.length) { n = parseInt(args[++i]); }
    else if (args[i].startsWith("-n")) { n = parseInt(args[i].slice(2)); }
    else { file = args[i]; }
  }
  let content = stdin;
  if (file) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node) return { stdout: "", stderr: `head: cannot open '${file}': No such file or directory\n` };
    if (node.type !== "file") return { stdout: "", stderr: `head: cannot open '${file}': Is a directory\n` };
    content = node.content;
  }
  const lines = content.split("\n");
  // If content ends with \n, last element is ""
  const meaningful = content.endsWith("\n") ? lines.slice(0, -1) : lines;
  const selected = meaningful.slice(0, n);
  return { stdout: selected.join("\n") + (selected.length > 0 ? "\n" : ""), stderr: "" };
}

function cmdTail(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let n = 10;
  let file: string | null = null;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "-n" && i + 1 < args.length) { n = parseInt(args[++i]); }
    else if (args[i].startsWith("-n")) { n = parseInt(args[i].slice(2)); }
    else { file = args[i]; }
  }
  let content = stdin;
  if (file) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node) return { stdout: "", stderr: `tail: cannot open '${file}': No such file or directory\n` };
    if (node.type !== "file") return { stdout: "", stderr: `tail: cannot open '${file}': Is a directory\n` };
    content = node.content;
  }
  const lines = content.split("\n");
  const meaningful = content.endsWith("\n") ? lines.slice(0, -1) : lines;
  const selected = meaningful.slice(-n);
  return { stdout: selected.join("\n") + (selected.length > 0 ? "\n" : ""), stderr: "" };
}

function cmdGrep(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let caseInsensitive = false;
  let invert = false;
  let pattern = "";
  const files: string[] = [];
  let i = 0;
  while (i < args.length) {
    if (args[i] === "-i") { caseInsensitive = true; i++; }
    else if (args[i] === "-v") { invert = true; i++; }
    else if (args[i] === "-e" && i + 1 < args.length) { pattern = args[++i]; i++; }
    else if (pattern === "") { pattern = args[i++]; }
    else { files.push(args[i++]); }
  }
  if (!pattern) return { stdout: "", stderr: "grep: missing pattern\n" };

  // Escape regex special chars to treat pattern as literal string
  const escaped = pattern.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(escaped, caseInsensitive ? "i" : "");

  let out = "";
  const sources =
    files.length > 0
      ? files.map((f) => {
          const path = resolvePath(f, cwd);
          const node = getNode(fs, path);
          if (!node || node.type !== "file") return null;
          return node.content;
        })
      : [stdin];

  const rawSources = files.length > 0
    ? files.map((f) => {
        const path = resolvePath(f, cwd);
        const node = getNode(fs, path);
        return node && node.type === "file" ? node.content : null;
      })
    : [stdin];

  for (const content of rawSources) {
    if (content === null) continue;
    const lines = content.split("\n");
    const meaningful = content.endsWith("\n") ? lines.slice(0, -1) : lines;
    for (const line of meaningful) {
      const matches = re.test(line);
      if (matches !== invert) out += line + "\n";
    }
  }

  // suppress unused variable warning
  void sources;

  return { stdout: out, stderr: "" };
}

function cmdWc(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let countLines = false;
  let countWords = false;
  let countChars = false;
  const files: string[] = [];
  for (const arg of args) {
    if (arg === "-l") countLines = true;
    else if (arg === "-w") countWords = true;
    else if (arg === "-c") countChars = true;
    else files.push(arg);
  }
  if (!countLines && !countWords && !countChars) {
    countLines = countWords = countChars = true;
  }

  function countContent(content: string): { lines: number; words: number; chars: number } {
    const lines = (content.match(/\n/g) || []).length;
    const words = content.trim() ? content.trim().split(/\s+/).length : 0;
    const chars = content.length;
    return { lines, words, chars };
  }

  function formatCounts(
    counts: { lines: number; words: number; chars: number },
    label?: string
  ): string {
    const parts: string[] = [];
    if (countLines) parts.push(String(counts.lines));
    if (countWords) parts.push(String(counts.words));
    if (countChars) parts.push(String(counts.chars));
    return parts.join(" ") + (label ? " " + label : "") + "\n";
  }

  if (files.length === 0) {
    return { stdout: formatCounts(countContent(stdin)), stderr: "" };
  }

  let out = "";
  for (const file of files) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node || node.type !== "file") {
      return { stdout: out, stderr: `wc: ${file}: No such file or directory\n` };
    }
    out += formatCounts(countContent(node.content), file);
  }
  return { stdout: out, stderr: "" };
}

function cmdSort(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let reverse = false;
  let unique = false;
  let file: string | null = null;
  for (const arg of args) {
    if (arg === "-r") reverse = true;
    else if (arg === "-u") unique = true;
    else file = arg;
  }
  let content = stdin;
  if (file) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node || node.type !== "file") return { stdout: "", stderr: `sort: ${file}: No such file or directory\n` };
    content = node.content;
  }
  const lines = content.endsWith("\n") ? content.slice(0, -1).split("\n") : content.split("\n");
  let sorted = [...lines].sort();
  if (reverse) sorted.reverse();
  if (unique) sorted = sorted.filter((v, i, a) => a.indexOf(v) === i);
  return { stdout: sorted.join("\n") + "\n", stderr: "" };
}

function cmdUniq(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let file: string | null = null;
  for (const arg of args) {
    if (!arg.startsWith("-")) file = arg;
  }
  let content = stdin;
  if (file) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node || node.type !== "file") return { stdout: "", stderr: `uniq: ${file}: No such file or directory\n` };
    content = node.content;
  }
  const lines = content.endsWith("\n") ? content.slice(0, -1).split("\n") : content.split("\n");
  const result: string[] = [];
  for (let i = 0; i < lines.length; i++) {
    if (i === 0 || lines[i] !== lines[i - 1]) result.push(lines[i]);
  }
  return { stdout: result.join("\n") + "\n", stderr: "" };
}

function cmdCut(args: string[], stdin: string, cwd: string, fs: FSDir): CmdResult {
  let delimiter = "\t";
  let fields = "";
  let file: string | null = null;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "-d" && i + 1 < args.length) delimiter = args[++i];
    else if (args[i].startsWith("-d")) delimiter = args[i].slice(2);
    else if (args[i] === "-f" && i + 1 < args.length) fields = args[++i];
    else if (args[i].startsWith("-f")) fields = args[i].slice(2);
    else file = args[i];
  }
  const fieldNums = fields.split(",").map((f) => parseInt(f) - 1);
  let content = stdin;
  if (file) {
    const path = resolvePath(file, cwd);
    const node = getNode(fs, path);
    if (!node || node.type !== "file") return { stdout: "", stderr: `cut: ${file}: No such file or directory\n` };
    content = node.content;
  }
  const lines = content.endsWith("\n") ? content.slice(0, -1).split("\n") : content.split("\n");
  const out = lines.map((l) => {
    const parts = l.split(delimiter);
    return fieldNums.map((n) => parts[n] ?? "").join(delimiter);
  }).join("\n") + "\n";
  return { stdout: out, stderr: "" };
}

function cmdTr(args: string[], stdin: string): CmdResult {
  if (args.length < 2) return { stdout: stdin, stderr: "" };
  const from = args[0];
  const to = args[1];
  let out = stdin;
  for (let i = 0; i < from.length; i++) {
    const target = to[i] ?? to[to.length - 1];
    out = out.split(from[i]).join(target);
  }
  return { stdout: out, stderr: "" };
}

function cmdChmod(args: string[], cwd: string, fs: FSDir): CmdResult {
  if (args.length < 2) return { stdout: "", stderr: "chmod: missing operand\n" };
  const modeStr = args[0];
  const mode = parseInt(modeStr, 8);
  if (isNaN(mode)) return { stdout: "", stderr: `chmod: invalid mode: '${modeStr}'\n` };
  for (let i = 1; i < args.length; i++) {
    const path = resolvePath(args[i], cwd);
    const node = getNode(fs, path);
    if (!node) return { stdout: "", stderr: `chmod: cannot access '${args[i]}': No such file or directory\n` };
    node.permissions = mode;
  }
  return { stdout: "", stderr: "" };
}

function cmdFind(args: string[], cwd: string, fs: FSDir): CmdResult {
  let searchPath = cwd;
  let namePattern: string | null = null;
  let typeFilter: string | null = null;

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "-name" && i + 1 < args.length) { namePattern = args[++i]; }
    else if (args[i] === "-type" && i + 1 < args.length) { typeFilter = args[++i]; }
    else if (!args[i].startsWith("-")) { searchPath = resolvePath(args[i], cwd); }
  }

  const results: string[] = [];
  function walk(node: FSNode, path: string) {
    if (typeFilter && node.type !== (typeFilter === "d" ? "dir" : "file")) {
      if (node.type === "dir") {
        for (const [name, child] of Object.entries(node.children)) {
          walk(child, path + "/" + name);
        }
      }
      return;
    }
    const name = path.split("/").pop() ?? "";
    const matchesName = namePattern
      ? new RegExp("^" + namePattern.replace(/\*/g, ".*").replace(/\?/g, ".") + "$").test(name)
      : true;
    if (matchesName) results.push(path);
    if (node.type === "dir") {
      for (const [n, child] of Object.entries(node.children)) {
        walk(child, path + "/" + n);
      }
    }
  }

  const startNode = getNode(fs, searchPath);
  if (startNode) {
    results.push(searchPath);
    if (startNode.type === "dir") {
      for (const [n, child] of Object.entries(startNode.children)) {
        walk(child, searchPath + "/" + n);
      }
    }
  }

  return { stdout: results.join("\n") + "\n", stderr: "" };
}

// ── Single command executor ─────────────────────────────────────────────────

interface ShellState {
  fs: FSDir;
  cwd: string;
  vars: Record<string, string>;
}

function executeSingleCommand(
  line: string,
  stdin: string,
  state: ShellState
): CmdResult {
  const words = shellSplit(line, state.vars);
  if (words.length === 0) return { stdout: "", stderr: "" };

  const cmd = words[0];
  const args = words.slice(1);

  switch (cmd) {
    case "echo": return cmdEcho(args);
    case "printf": {
      // Basic printf: just join args
      const fmt = args[0] ?? "";
      const processed = fmt
        .replace(/\\n/g, "\n")
        .replace(/\\t/g, "\t")
        .replace(/\\\\/g, "\\");
      return { stdout: processed, stderr: "" };
    }
    case "pwd": return { stdout: state.cwd + "\n", stderr: "" };
    case "ls": return cmdLs(args, state.cwd, state.fs);
    case "cd": return cmdCd(args, state);
    case "cat": return cmdCat(args, stdin, state.cwd, state.fs);
    case "mkdir": return cmdMkdir(args, state.cwd, state.fs);
    case "touch": return cmdTouch(args, state.cwd, state.fs);
    case "rm": return cmdRm(args, state.cwd, state.fs);
    case "rmdir": return cmdRmdir(args, state.cwd, state.fs);
    case "cp": return cmdCp(args, state.cwd, state.fs);
    case "mv": return cmdMv(args, state.cwd, state.fs);
    case "head": return cmdHead(args, stdin, state.cwd, state.fs);
    case "tail": return cmdTail(args, stdin, state.cwd, state.fs);
    case "grep": return cmdGrep(args, stdin, state.cwd, state.fs);
    case "wc": return cmdWc(args, stdin, state.cwd, state.fs);
    case "sort": return cmdSort(args, stdin, state.cwd, state.fs);
    case "uniq": return cmdUniq(args, stdin, state.cwd, state.fs);
    case "cut": return cmdCut(args, stdin, state.cwd, state.fs);
    case "tr": return cmdTr(args, stdin);
    case "chmod": return cmdChmod(args, state.cwd, state.fs);
    case "find": return cmdFind(args, state.cwd, state.fs);
    case "whoami": return { stdout: "user\n", stderr: "" };
    case "hostname": return { stdout: "hypercode\n", stderr: "" };
    case "uname": {
      if (args.includes("-a")) return { stdout: "Linux hypercode 6.1.0 #1 SMP x86_64 GNU/Linux\n", stderr: "" };
      return { stdout: "Linux\n", stderr: "" };
    }
    case "date": return { stdout: "Thu Jan  1 00:00:00 UTC 1970\n", stderr: "" };
    case "true": return { stdout: "", stderr: "" };
    case "false": return { stdout: "", stderr: "false\n" };
    case "sleep": return { stdout: "", stderr: "" };
    case "read": {
      // In non-interactive mode, read gets empty input
      if (args.length > 0) state.vars[args[0]] = "";
      return { stdout: "", stderr: "" };
    }
    default:
      return { stdout: "", stderr: `${cmd}: command not found\n` };
  }
}

// ── Pipeline executor ────────────────────────────────────────────────────────

function executePipeline(line: string, state: ShellState): CmdResult {
  const { cmd, file, append } = extractRedirect(line);
  const segments = splitByPipe(cmd);

  let stdin = "";
  let lastStdout = "";
  let allStderr = "";

  for (const segment of segments) {
    const result = executeSingleCommand(segment, stdin, state);
    stdin = result.stdout;
    lastStdout = result.stdout;
    allStderr += result.stderr;
  }

  if (file) {
    const path = resolvePath(file, state.cwd);
    const existing = getNode(state.fs, path);
    const prev = existing && existing.type === "file" ? existing.content : "";
    const newContent = append ? prev + lastStdout : lastStdout;
    setNode(state.fs, path, { type: "file", content: newContent, permissions: 0o644 });
    return { stdout: "", stderr: allStderr };
  }

  return { stdout: lastStdout, stderr: allStderr };
}

// ── Condition evaluator ──────────────────────────────────────────────────────

function evaluateCondition(expr: string, state: ShellState): boolean {
  const trimmed = expr.trim();

  // [ ... ] or test ...
  const bracketMatch = trimmed.match(/^\[\s*(.*?)\s*\]$/) ?? trimmed.match(/^test\s+(.*)$/);
  if (!bracketMatch) return false;

  const inner = expandVars(bracketMatch[1], state.vars);

  // File tests
  if (/^-f\s/.test(inner)) {
    const f = inner.slice(2).trim().replace(/^["']|["']$/g, "");
    const node = getNode(state.fs, resolvePath(f, state.cwd));
    return node !== null && node.type === "file";
  }
  if (/^-d\s/.test(inner)) {
    const f = inner.slice(2).trim().replace(/^["']|["']$/g, "");
    const node = getNode(state.fs, resolvePath(f, state.cwd));
    return node !== null && node.type === "dir";
  }
  if (/^-e\s/.test(inner)) {
    const f = inner.slice(2).trim().replace(/^["']|["']$/g, "");
    return getNode(state.fs, resolvePath(f, state.cwd)) !== null;
  }
  if (/^-z\s/.test(inner)) {
    const val = inner.slice(2).trim().replace(/^["']|["']$/g, "");
    return val === "";
  }
  if (/^-n\s/.test(inner)) {
    const val = inner.slice(2).trim().replace(/^["']|["']$/g, "");
    return val !== "";
  }

  // String equality
  const eqMatch = inner.match(/^(.*?)\s*=\s*(.*?)$/);
  if (eqMatch) {
    return eqMatch[1].replace(/^["']|["']$/g, "") === eqMatch[2].replace(/^["']|["']$/g, "");
  }
  const neqMatch = inner.match(/^(.*?)\s*!=\s*(.*?)$/);
  if (neqMatch) {
    return neqMatch[1].replace(/^["']|["']$/g, "") !== neqMatch[2].replace(/^["']|["']$/g, "");
  }

  // Numeric comparisons
  const numMatch = inner.match(/^(\S+)\s+(-eq|-ne|-lt|-le|-gt|-ge)\s+(\S+)$/);
  if (numMatch) {
    const a = parseInt(numMatch[1]);
    const b = parseInt(numMatch[3]);
    switch (numMatch[2]) {
      case "-eq": return a === b;
      case "-ne": return a !== b;
      case "-lt": return a < b;
      case "-le": return a <= b;
      case "-gt": return a > b;
      case "-ge": return a >= b;
    }
  }

  return false;
}

// ── Script block parsers ─────────────────────────────────────────────────────

interface IfBlock {
  condition: string;
  thenLines: string[];
  elseLines: string[];
  endIdx: number;
}

function parseIfBlock(lines: string[], startIdx: number): IfBlock {
  const firstLine = lines[startIdx].trim();
  // "if COND; then" or "if COND then"
  const condMatch =
    firstLine.match(/^if\s+(.*?)\s*;\s*then\s*$/) ??
    firstLine.match(/^if\s+(.*?)\s+then\s*$/);
  const condition = condMatch ? condMatch[1].trim() : firstLine.slice(3).trim();

  let i = startIdx + 1;
  const thenLines: string[] = [];
  const elseLines: string[] = [];
  let inElse = false;
  let depth = 1;

  while (i < lines.length) {
    const t = lines[i].trim();
    if (t.startsWith("if ") && (t.endsWith("; then") || t.endsWith(" then"))) depth++;
    if (t === "fi") {
      depth--;
      if (depth === 0) return { condition, thenLines, elseLines, endIdx: i + 1 };
    }
    if (depth === 1 && (t === "else" || t.startsWith("elif "))) {
      inElse = true;
    } else if (inElse) {
      elseLines.push(lines[i]);
    } else {
      thenLines.push(lines[i]);
    }
    i++;
  }
  return { condition, thenLines, elseLines, endIdx: i };
}

interface ForBlock {
  varName: string;
  list: string[];
  bodyLines: string[];
  endIdx: number;
}

function parseForBlock(
  lines: string[],
  startIdx: number,
  vars: Record<string, string>
): ForBlock {
  const firstLine = lines[startIdx].trim();
  const forMatch =
    firstLine.match(/^for\s+(\w+)\s+in\s+(.*?)\s*;\s*do\s*$/) ??
    firstLine.match(/^for\s+(\w+)\s+in\s+(.*?)\s+do\s*$/);

  const varName = forMatch ? forMatch[1] : "i";
  const listStr = forMatch ? forMatch[2] : "";
  const list = shellSplit(listStr, vars);

  let i = startIdx + 1;
  const bodyLines: string[] = [];
  let depth = 1;

  while (i < lines.length) {
    const t = lines[i].trim();
    if (t.startsWith("for ")) depth++;
    if (t === "done") {
      depth--;
      if (depth === 0) return { varName, list, bodyLines, endIdx: i + 1 };
    }
    bodyLines.push(lines[i]);
    i++;
  }
  return { varName, list, bodyLines, endIdx: i };
}

// ── Main script executor ─────────────────────────────────────────────────────

function executeScriptLines(lines: string[], state: ShellState): string {
  let output = "";
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trim();

    // Skip blanks and comments
    if (trimmed === "" || trimmed.startsWith("#")) { i++; continue; }

    // For loop
    if (
      trimmed.startsWith("for ") &&
      (trimmed.includes("; do") || / do$/.test(trimmed))
    ) {
      const block = parseForBlock(lines, i, state.vars);
      for (const item of block.list) {
        state.vars[block.varName] = item;
        output += executeScriptLines(block.bodyLines, state);
      }
      i = block.endIdx;
      continue;
    }

    // If statement
    if (
      trimmed.startsWith("if ") &&
      (trimmed.includes("; then") || / then$/.test(trimmed))
    ) {
      const block = parseIfBlock(lines, i);
      const condExpanded = expandVars(block.condition, state.vars);
      if (evaluateCondition(condExpanded, state)) {
        output += executeScriptLines(block.thenLines, state);
      } else if (block.elseLines.length > 0) {
        output += executeScriptLines(block.elseLines, state);
      }
      i = block.endIdx;
      continue;
    }

    // Variable assignment: VAR=value (no spaces around =, no command before it)
    const assignMatch = trimmed.match(/^(\w+)=(["'].*["']|[^\s|><]*)$/);
    if (assignMatch) {
      let value = assignMatch[2];
      if (
        (value.startsWith('"') && value.endsWith('"')) ||
        (value.startsWith("'") && value.endsWith("'"))
      ) {
        const inner = value.slice(1, -1);
        value = value.startsWith('"') ? expandVars(inner, state.vars) : inner;
      } else {
        value = expandVars(value, state.vars);
      }
      state.vars[assignMatch[1]] = value;
      i++;
      continue;
    }

    // Regular command line (may include pipes and redirects)
    const result = executePipeline(trimmed, state);
    output += result.stdout;
    // Errors are intentionally dropped from output (stderr is separate)
    i++;
  }

  return output;
}

// ── Public API ───────────────────────────────────────────────────────────────

/** Run a shell script against a fresh virtual filesystem. Returns combined stdout. */
export function runShell(script: string): ShellResult {
  const state: ShellState = {
    fs: createInitialFS(),
    cwd: "/home/user",
    vars: {
      HOME: "/home/user",
      USER: "user",
      SHELL: "/bin/bash",
      PATH: "/usr/local/bin:/usr/bin:/bin",
      PWD: "/home/user",
    },
  };

  try {
    const lines = script.split("\n");
    const stdout = executeScriptLines(lines, state);
    return { stdout, stderr: "", error: "" };
  } catch (err) {
    return {
      stdout: "",
      stderr: "",
      error: err instanceof Error ? err.message : String(err),
    };
  }
}
