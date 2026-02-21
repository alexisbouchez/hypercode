/**
 * In-memory Redis emulator that supports core Redis commands.
 * Produces redis-cli compatible output formatting.
 */

type RedisType = "string" | "list" | "set" | "hash" | "zset" | "hll";

interface StoreEntry {
  type: RedisType;
  value: unknown;
  expireAt?: number;
}

function fmtBulk(s: string | null): string {
  if (s === null) return "(nil)";
  return `"${s}"`;
}

function fmtInt(n: number): string {
  return `(integer) ${n}`;
}

function fmtArr(items: (string | null)[]): string {
  if (items.length === 0) return "(empty array)";
  return items
    .map((item, i) => `${i + 1}) ${item === null ? "(nil)" : `"${item}"`}`)
    .join("\n");
}

function fmtErr(msg: string): string {
  return `(error) ${msg}`;
}

function parseArgs(line: string): string[] {
  const args: string[] = [];
  let i = 0;
  while (i < line.length) {
    while (i < line.length && /\s/.test(line[i])) i++;
    if (i >= line.length) break;
    if (line[i] === '"') {
      i++;
      let s = "";
      while (i < line.length && line[i] !== '"') {
        if (line[i] === "\\" && i + 1 < line.length) {
          i++;
          const c = line[i];
          if (c === "n") s += "\n";
          else if (c === "t") s += "\t";
          else s += c;
        } else {
          s += line[i];
        }
        i++;
      }
      if (i < line.length) i++;
      args.push(s);
    } else if (line[i] === "'") {
      i++;
      let s = "";
      while (i < line.length && line[i] !== "'") s += line[i++];
      if (i < line.length) i++;
      args.push(s);
    } else {
      let s = "";
      while (i < line.length && !/\s/.test(line[i])) s += line[i++];
      args.push(s);
    }
  }
  return args;
}

export class RedisEmulator {
  private store = new Map<string, StoreEntry>();
  private inMulti = false;
  private txQueue: string[][] = [];

  reset() {
    this.store.clear();
    this.inMulti = false;
    this.txQueue = [];
  }

  private now() {
    return Date.now();
  }

  private isExpired(key: string): boolean {
    const e = this.store.get(key);
    return !!e && e.expireAt !== undefined && this.now() > e.expireAt;
  }

  private getEntry(key: string): StoreEntry | undefined {
    const e = this.store.get(key);
    if (!e) return undefined;
    if (e.expireAt !== undefined && this.now() > e.expireAt) {
      this.store.delete(key);
      return undefined;
    }
    return e;
  }

  private getString(key: string): string | null {
    const e = this.getEntry(key);
    if (!e) return null;
    if (e.type !== "string") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as string;
  }

  private getList(key: string): string[] {
    const e = this.getEntry(key);
    if (!e) return [];
    if (e.type !== "list") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as string[];
  }

  private getOrCreateList(key: string): string[] {
    const e = this.getEntry(key);
    if (!e) {
      const list: string[] = [];
      this.store.set(key, { type: "list", value: list });
      return list;
    }
    if (e.type !== "list") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as string[];
  }

  private getSet(key: string): Set<string> {
    const e = this.getEntry(key);
    if (!e) return new Set();
    if (e.type !== "set") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Set<string>;
  }

  private getOrCreateSet(key: string): Set<string> {
    const e = this.getEntry(key);
    if (!e) {
      const s = new Set<string>();
      this.store.set(key, { type: "set", value: s });
      return s;
    }
    if (e.type !== "set") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Set<string>;
  }

  private getHash(key: string): Map<string, string> {
    const e = this.getEntry(key);
    if (!e) return new Map();
    if (e.type !== "hash") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Map<string, string>;
  }

  private getOrCreateHash(key: string): Map<string, string> {
    const e = this.getEntry(key);
    if (!e) {
      const h = new Map<string, string>();
      this.store.set(key, { type: "hash", value: h });
      return h;
    }
    if (e.type !== "hash") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Map<string, string>;
  }

  private getZSet(key: string): Map<string, number> {
    const e = this.getEntry(key);
    if (!e) return new Map();
    if (e.type !== "zset") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Map<string, number>;
  }

  private getOrCreateZSet(key: string): Map<string, number> {
    const e = this.getEntry(key);
    if (!e) {
      const z = new Map<string, number>();
      this.store.set(key, { type: "zset", value: z });
      return z;
    }
    if (e.type !== "zset") throw new Error("WRONGTYPE Operation against a key holding the wrong kind of value");
    return e.value as Map<string, number>;
  }

  private zsetSorted(key: string): { member: string; score: number }[] {
    return Array.from(this.getZSet(key).entries())
      .map(([member, score]) => ({ member, score }))
      .sort((a, b) => a.score - b.score || a.member.localeCompare(b.member));
  }

  executeCommand(args: string[]): string {
    if (args.length === 0) return "";
    const cmd = args[0].toUpperCase();
    const rest = args.slice(1);

    if (cmd === "MULTI") {
      if (this.inMulti) return fmtErr("ERR MULTI calls can not be nested");
      this.inMulti = true;
      this.txQueue = [];
      return "OK";
    }
    if (cmd === "DISCARD") {
      if (!this.inMulti) return fmtErr("ERR DISCARD without MULTI");
      this.inMulti = false;
      this.txQueue = [];
      return "OK";
    }
    if (cmd === "EXEC") {
      if (!this.inMulti) return fmtErr("ERR EXEC without MULTI");
      this.inMulti = false;
      const results = this.txQueue.map((q) => this.executeCommand(q));
      this.txQueue = [];
      if (results.length === 0) return "(empty array)";
      return results.map((r, i) => `${i + 1}) ${r}`).join("\n");
    }
    if (this.inMulti) {
      this.txQueue.push(args);
      return "QUEUED";
    }

    try {
      return this.dispatch(cmd, rest);
    } catch (e) {
      return fmtErr(e instanceof Error ? e.message : String(e));
    }
  }

  private dispatch(cmd: string, args: string[]): string {
    switch (cmd) {
      case "SET": return this.SET(args);
      case "GET": return this.GET(args);
      case "DEL": return this.DEL(args);
      case "EXISTS": return this.EXISTS(args);
      case "MSET": return this.MSET(args);
      case "MGET": return this.MGET(args);
      case "APPEND": return this.APPEND(args);
      case "STRLEN": return this.STRLEN(args);
      case "GETSET": return this.GETSET(args);
      case "SETNX": return this.SETNX(args);
      case "SETEX": return this.SETEX(args);
      case "GETDEL": return this.GETDEL(args);
      case "EXPIRE": return this.EXPIRE(args);
      case "EXPIREAT": return this.EXPIREAT(args);
      case "TTL": return this.TTL(args);
      case "PTTL": return this.PTTL(args);
      case "PERSIST": return this.PERSIST(args);
      case "PEXPIRE": return this.PEXPIRE(args);
      case "INCR": return this.INCR(args);
      case "INCRBY": return this.INCRBY(args);
      case "DECR": return this.DECR(args);
      case "DECRBY": return this.DECRBY(args);
      case "INCRBYFLOAT": return this.INCRBYFLOAT(args);
      case "LPUSH": return this.LPUSH(args);
      case "RPUSH": return this.RPUSH(args);
      case "LRANGE": return this.LRANGE(args);
      case "LPOP": return this.LPOP(args);
      case "RPOP": return this.RPOP(args);
      case "LLEN": return this.LLEN(args);
      case "LINDEX": return this.LINDEX(args);
      case "LSET": return this.LSET(args);
      case "SADD": return this.SADD(args);
      case "SMEMBERS": return this.SMEMBERS(args);
      case "SISMEMBER": return this.SISMEMBER(args);
      case "SCARD": return this.SCARD(args);
      case "SUNION": return this.SUNION(args);
      case "SINTER": return this.SINTER(args);
      case "SDIFF": return this.SDIFF(args);
      case "SREM": return this.SREM(args);
      case "HSET": return this.HSET(args);
      case "HGET": return this.HGET(args);
      case "HGETALL": return this.HGETALL(args);
      case "HDEL": return this.HDEL(args);
      case "HKEYS": return this.HKEYS(args);
      case "HVALS": return this.HVALS(args);
      case "HEXISTS": return this.HEXISTS(args);
      case "HLEN": return this.HLEN(args);
      case "HMGET": return this.HMGET(args);
      case "HINCRBY": return this.HINCRBY(args);
      case "ZADD": return this.ZADD(args);
      case "ZRANGE": return this.ZRANGE(args);
      case "ZRANK": return this.ZRANK(args);
      case "ZSCORE": return this.ZSCORE(args);
      case "ZCARD": return this.ZCARD(args);
      case "ZINCRBY": return this.ZINCRBY(args);
      case "ZREVRANK": return this.ZREVRANK(args);
      case "ZREVRANGE": return this.ZREVRANGE(args);
      case "ZRANGEBYSCORE": return this.ZRANGEBYSCORE(args);
      case "ZREM": return this.ZREM(args);
      case "KEYS": return this.KEYS(args);
      case "TYPE": return this.TYPE(args);
      case "RENAME": return this.RENAME(args);
      case "COPY": return this.COPY(args);
      case "SCAN": return this.SCAN(args);
      case "PFADD": return this.PFADD(args);
      case "PFCOUNT": return this.PFCOUNT(args);
      case "PFMERGE": return this.PFMERGE(args);
      case "DBSIZE": return fmtInt(this.store.size);
      case "FLUSHDB": { this.store.clear(); return "OK"; }
      case "PING": return args.length > 0 ? fmtBulk(args[0]) : "PONG";
      case "SELECT": return "OK";
      case "OBJECT": return this.OBJECT(args);
      default:
        return fmtErr(`ERR unknown command \`${cmd}\``);
    }
  }

  private SET(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'set' command");
    const [key, value, ...opts] = args;
    let ex: number | undefined;
    let px: number | undefined;
    let nx = false;
    let xx = false;
    for (let i = 0; i < opts.length; i++) {
      const o = opts[i].toUpperCase();
      if (o === "EX" && opts[i + 1]) ex = parseInt(opts[++i]);
      else if (o === "PX" && opts[i + 1]) px = parseInt(opts[++i]);
      else if (o === "NX") nx = true;
      else if (o === "XX") xx = true;
    }
    const existing = this.getEntry(key);
    if (nx && existing) return "(nil)";
    if (xx && !existing) return "(nil)";
    let expireAt: number | undefined;
    if (ex !== undefined) expireAt = this.now() + ex * 1000;
    else if (px !== undefined) expireAt = this.now() + px;
    this.store.set(key, { type: "string", value, expireAt });
    return "OK";
  }

  private GET(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'get' command");
    return fmtBulk(this.getString(args[0]));
  }

  private DEL(args: string[]): string {
    let n = 0;
    for (const k of args) { if (this.getEntry(k)) { this.store.delete(k); n++; } }
    return fmtInt(n);
  }

  private EXISTS(args: string[]): string {
    let n = 0;
    for (const k of args) { if (this.getEntry(k)) n++; }
    return fmtInt(n);
  }

  private MSET(args: string[]): string {
    if (args.length < 2 || args.length % 2 !== 0) return fmtErr("ERR syntax error");
    for (let i = 0; i < args.length; i += 2) {
      this.store.set(args[i], { type: "string", value: args[i + 1] });
    }
    return "OK";
  }

  private MGET(args: string[]): string {
    return fmtArr(args.map((k) => this.getString(k)));
  }

  private APPEND(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'append' command");
    const [key, suffix] = args;
    const cur = this.getString(key) ?? "";
    const next = cur + suffix;
    const e = this.store.get(key);
    this.store.set(key, { type: "string", value: next, expireAt: e?.expireAt });
    return fmtInt(next.length);
  }

  private STRLEN(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'strlen' command");
    return fmtInt((this.getString(args[0]) ?? "").length);
  }

  private GETSET(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'getset' command");
    const [key, value] = args;
    const old = this.getString(key);
    this.store.set(key, { type: "string", value });
    return fmtBulk(old);
  }

  private SETNX(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'setnx' command");
    const [key, value] = args;
    if (this.getEntry(key)) return fmtInt(0);
    this.store.set(key, { type: "string", value });
    return fmtInt(1);
  }

  private SETEX(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'setex' command");
    const [key, secs, value] = args;
    const s = parseInt(secs);
    if (isNaN(s) || s <= 0) return fmtErr("ERR invalid expire time in 'setex' command");
    this.store.set(key, { type: "string", value, expireAt: this.now() + s * 1000 });
    return "OK";
  }

  private GETDEL(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'getdel' command");
    const v = this.getString(args[0]);
    if (v !== null) this.store.delete(args[0]);
    return fmtBulk(v);
  }

  private EXPIRE(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'expire' command");
    const e = this.getEntry(args[0]);
    if (!e) return fmtInt(0);
    e.expireAt = this.now() + parseInt(args[1]) * 1000;
    return fmtInt(1);
  }

  private EXPIREAT(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'expireat' command");
    const e = this.getEntry(args[0]);
    if (!e) return fmtInt(0);
    e.expireAt = parseInt(args[1]) * 1000;
    return fmtInt(1);
  }

  private TTL(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'ttl' command");
    const e = this.store.get(args[0]);
    if (!e || this.isExpired(args[0])) { this.store.delete(args[0]); return fmtInt(-2); }
    if (e.expireAt === undefined) return fmtInt(-1);
    return fmtInt(Math.ceil((e.expireAt - this.now()) / 1000));
  }

  private PTTL(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'pttl' command");
    const e = this.store.get(args[0]);
    if (!e || this.isExpired(args[0])) { this.store.delete(args[0]); return fmtInt(-2); }
    if (e.expireAt === undefined) return fmtInt(-1);
    return fmtInt(e.expireAt - this.now());
  }

  private PERSIST(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'persist' command");
    const e = this.getEntry(args[0]);
    if (!e || e.expireAt === undefined) return fmtInt(0);
    delete e.expireAt;
    return fmtInt(1);
  }

  private PEXPIRE(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'pexpire' command");
    const e = this.getEntry(args[0]);
    if (!e) return fmtInt(0);
    e.expireAt = this.now() + parseInt(args[1]);
    return fmtInt(1);
  }

  private getInt(key: string): number {
    const v = this.getString(key);
    if (v === null) return 0;
    const n = parseInt(v);
    if (isNaN(n)) throw new Error("ERR value is not an integer or out of range");
    return n;
  }

  private setInt(key: string, n: number): void {
    const e = this.store.get(key);
    this.store.set(key, { type: "string", value: String(n), expireAt: e?.expireAt });
  }

  private INCR(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'incr' command");
    const n = this.getInt(args[0]) + 1;
    this.setInt(args[0], n);
    return fmtInt(n);
  }

  private INCRBY(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'incrby' command");
    const n = this.getInt(args[0]) + parseInt(args[1]);
    this.setInt(args[0], n);
    return fmtInt(n);
  }

  private DECR(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'decr' command");
    const n = this.getInt(args[0]) - 1;
    this.setInt(args[0], n);
    return fmtInt(n);
  }

  private DECRBY(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'decrby' command");
    const n = this.getInt(args[0]) - parseInt(args[1]);
    this.setInt(args[0], n);
    return fmtInt(n);
  }

  private INCRBYFLOAT(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'incrbyfloat' command");
    const cur = parseFloat(this.getString(args[0]) ?? "0");
    if (isNaN(cur)) return fmtErr("ERR value is not a valid float");
    const n = cur + parseFloat(args[1]);
    const result = String(n);
    const e = this.store.get(args[0]);
    this.store.set(args[0], { type: "string", value: result, expireAt: e?.expireAt });
    return fmtBulk(result);
  }

  private LPUSH(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'lpush' command");
    const [key, ...els] = args;
    const list = this.getOrCreateList(key);
    for (const el of [...els].reverse()) list.unshift(el);
    return fmtInt(list.length);
  }

  private RPUSH(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'rpush' command");
    const [key, ...els] = args;
    const list = this.getOrCreateList(key);
    list.push(...els);
    return fmtInt(list.length);
  }

  private LRANGE(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'lrange' command");
    const list = this.getList(args[0]);
    const len = list.length;
    let s = parseInt(args[1]), e = parseInt(args[2]);
    if (s < 0) s = Math.max(0, len + s);
    if (e < 0) e = len + e;
    e = Math.min(e, len - 1);
    if (s > e) return "(empty array)";
    return fmtArr(list.slice(s, e + 1));
  }

  private LPOP(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'lpop' command");
    const list = this.getList(args[0]);
    if (list.length === 0) return "(nil)";
    if (args[1] !== undefined) {
      const n = parseInt(args[1]);
      const removed = list.splice(0, n);
      if (list.length === 0) this.store.delete(args[0]);
      return fmtArr(removed);
    }
    const v = list.shift()!;
    if (list.length === 0) this.store.delete(args[0]);
    return fmtBulk(v);
  }

  private RPOP(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'rpop' command");
    const list = this.getList(args[0]);
    if (list.length === 0) return "(nil)";
    if (args[1] !== undefined) {
      const n = parseInt(args[1]);
      const removed = list.splice(-n).reverse();
      if (list.length === 0) this.store.delete(args[0]);
      return fmtArr(removed);
    }
    const v = list.pop()!;
    if (list.length === 0) this.store.delete(args[0]);
    return fmtBulk(v);
  }

  private LLEN(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'llen' command");
    return fmtInt(this.getList(args[0]).length);
  }

  private LINDEX(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'lindex' command");
    const list = this.getList(args[0]);
    let idx = parseInt(args[1]);
    if (idx < 0) idx = list.length + idx;
    return fmtBulk(idx >= 0 && idx < list.length ? list[idx] : null);
  }

  private LSET(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'lset' command");
    const list = this.getList(args[0]);
    if (list.length === 0) return fmtErr("ERR no such key");
    let idx = parseInt(args[1]);
    if (idx < 0) idx = list.length + idx;
    if (idx < 0 || idx >= list.length) return fmtErr("ERR index out of range");
    list[idx] = args[2];
    return "OK";
  }

  private SADD(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'sadd' command");
    const [key, ...members] = args;
    const s = this.getOrCreateSet(key);
    let n = 0;
    for (const m of members) { if (!s.has(m)) { s.add(m); n++; } }
    return fmtInt(n);
  }

  private SMEMBERS(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'smembers' command");
    return fmtArr(Array.from(this.getSet(args[0])).sort());
  }

  private SISMEMBER(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'sismember' command");
    return fmtInt(this.getSet(args[0]).has(args[1]) ? 1 : 0);
  }

  private SCARD(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'scard' command");
    return fmtInt(this.getSet(args[0]).size);
  }

  private SUNION(args: string[]): string {
    const r = new Set<string>();
    for (const k of args) for (const m of this.getSet(k)) r.add(m);
    return fmtArr(Array.from(r).sort());
  }

  private SINTER(args: string[]): string {
    if (args.length === 0) return "(empty array)";
    const sets = args.map((k) => this.getSet(k));
    const r = Array.from(sets[0]).filter((m) => sets.slice(1).every((s) => s.has(m))).sort();
    return fmtArr(r);
  }

  private SDIFF(args: string[]): string {
    if (args.length === 0) return "(empty array)";
    const first = new Set(this.getSet(args[0]));
    for (const k of args.slice(1)) for (const m of this.getSet(k)) first.delete(m);
    return fmtArr(Array.from(first).sort());
  }

  private SREM(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'srem' command");
    const [key, ...members] = args;
    const s = this.getSet(key);
    let n = 0;
    for (const m of members) { if (s.delete(m)) n++; }
    if (s.size === 0) this.store.delete(key);
    return fmtInt(n);
  }

  private HSET(args: string[]): string {
    if (args.length < 3 || (args.length - 1) % 2 !== 0) return fmtErr("ERR wrong number of arguments for 'hset' command");
    const [key, ...pairs] = args;
    const h = this.getOrCreateHash(key);
    let n = 0;
    for (let i = 0; i < pairs.length; i += 2) {
      if (!h.has(pairs[i])) n++;
      h.set(pairs[i], pairs[i + 1]);
    }
    return fmtInt(n);
  }

  private HGET(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'hget' command");
    return fmtBulk(this.getHash(args[0]).get(args[1]) ?? null);
  }

  private HGETALL(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'hgetall' command");
    const h = this.getHash(args[0]);
    if (h.size === 0) return "(empty array)";
    const items: string[] = [];
    let i = 1;
    for (const [f, v] of h) { items.push(`${i++}) "${f}"`); items.push(`${i++}) "${v}"`); }
    return items.join("\n");
  }

  private HDEL(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'hdel' command");
    const [key, ...fields] = args;
    const h = this.getHash(key);
    let n = 0;
    for (const f of fields) { if (h.delete(f)) n++; }
    return fmtInt(n);
  }

  private HKEYS(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'hkeys' command");
    return fmtArr(Array.from(this.getHash(args[0]).keys()));
  }

  private HVALS(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'hvals' command");
    return fmtArr(Array.from(this.getHash(args[0]).values()));
  }

  private HEXISTS(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'hexists' command");
    return fmtInt(this.getHash(args[0]).has(args[1]) ? 1 : 0);
  }

  private HLEN(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'hlen' command");
    return fmtInt(this.getHash(args[0]).size);
  }

  private HMGET(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'hmget' command");
    const [key, ...fields] = args;
    const h = this.getHash(key);
    return fmtArr(fields.map((f) => h.get(f) ?? null));
  }

  private HINCRBY(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'hincrby' command");
    const [key, field, by] = args;
    const h = this.getOrCreateHash(key);
    const cur = parseInt(h.get(field) ?? "0");
    if (isNaN(cur)) return fmtErr("ERR hash value is not an integer");
    const n = cur + parseInt(by);
    h.set(field, String(n));
    return fmtInt(n);
  }

  private ZADD(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'zadd' command");
    const [key, ...rest] = args;
    let items = rest;
    let nx = false, xx = false, ch = false;
    while (items.length > 0 && ["NX", "XX", "GT", "LT", "CH"].includes(items[0].toUpperCase())) {
      const f = items.shift()!.toUpperCase();
      if (f === "NX") nx = true;
      if (f === "XX") xx = true;
      if (f === "CH") ch = true;
    }
    if (items.length < 2 || items.length % 2 !== 0) return fmtErr("ERR syntax error");
    const z = this.getOrCreateZSet(key);
    let added = 0, changed = 0;
    for (let i = 0; i < items.length; i += 2) {
      const score = parseFloat(items[i]);
      const member = items[i + 1];
      if (isNaN(score)) return fmtErr("ERR value is not a valid float");
      const exists = z.has(member);
      if (nx && exists) continue;
      if (xx && !exists) continue;
      if (!exists) added++;
      else if (z.get(member) !== score) changed++;
      z.set(member, score);
    }
    return fmtInt(ch ? added + changed : added);
  }

  private ZRANGE(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'zrange' command");
    const [key, sStr, eStr, ...opts] = args;
    const ws = opts.some((o) => o.toUpperCase() === "WITHSCORES");
    const sorted = this.zsetSorted(key);
    const len = sorted.length;
    let s = parseInt(sStr), e = parseInt(eStr);
    if (s < 0) s = Math.max(0, len + s);
    if (e < 0) e = len + e;
    e = Math.min(e, len - 1);
    if (s > e || len === 0) return "(empty array)";
    const slice = sorted.slice(s, e + 1);
    if (ws) {
      const items: string[] = [];
      let i = 1;
      for (const { member, score } of slice) { items.push(`${i++}) "${member}"`); items.push(`${i++}) "${score}"`); }
      return items.join("\n");
    }
    return fmtArr(slice.map((x) => x.member));
  }

  private ZRANK(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'zrank' command");
    const sorted = this.zsetSorted(args[0]);
    const idx = sorted.findIndex((x) => x.member === args[1]);
    return idx === -1 ? "(nil)" : fmtInt(idx);
  }

  private ZSCORE(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'zscore' command");
    const score = this.getZSet(args[0]).get(args[1]);
    return score === undefined ? "(nil)" : fmtBulk(String(score));
  }

  private ZCARD(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'zcard' command");
    return fmtInt(this.getZSet(args[0]).size);
  }

  private ZINCRBY(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'zincrby' command");
    const [key, by, member] = args;
    const z = this.getOrCreateZSet(key);
    const n = (z.get(member) ?? 0) + parseFloat(by);
    z.set(member, n);
    return fmtBulk(String(n));
  }

  private ZREVRANK(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'zrevrank' command");
    const sorted = this.zsetSorted(args[0]);
    const idx = sorted.findIndex((x) => x.member === args[1]);
    return idx === -1 ? "(nil)" : fmtInt(sorted.length - 1 - idx);
  }

  private ZREVRANGE(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'zrevrange' command");
    const [key, sStr, eStr, ...opts] = args;
    const ws = opts.some((o) => o.toUpperCase() === "WITHSCORES");
    const sorted = [...this.zsetSorted(key)].reverse();
    const len = sorted.length;
    let s = parseInt(sStr), e = parseInt(eStr);
    if (s < 0) s = Math.max(0, len + s);
    if (e < 0) e = len + e;
    e = Math.min(e, len - 1);
    if (s > e || len === 0) return "(empty array)";
    const slice = sorted.slice(s, e + 1);
    if (ws) {
      const items: string[] = [];
      let i = 1;
      for (const { member, score } of slice) { items.push(`${i++}) "${member}"`); items.push(`${i++}) "${score}"`); }
      return items.join("\n");
    }
    return fmtArr(slice.map((x) => x.member));
  }

  private ZRANGEBYSCORE(args: string[]): string {
    if (args.length < 3) return fmtErr("ERR wrong number of arguments for 'zrangebyscore' command");
    const [key, minStr, maxStr, ...opts] = args;
    const ws = opts.some((o) => o.toUpperCase() === "WITHSCORES");
    const exMin = minStr.startsWith("(");
    const exMax = maxStr.startsWith("(");
    const min = minStr === "-inf" ? -Infinity : parseFloat(minStr.replace("(", ""));
    const max = maxStr === "+inf" ? Infinity : parseFloat(maxStr.replace("(", ""));
    const sorted = this.zsetSorted(key).filter(({ score }) =>
      (exMin ? score > min : score >= min) && (exMax ? score < max : score <= max)
    );
    if (sorted.length === 0) return "(empty array)";
    if (ws) {
      const items: string[] = [];
      let i = 1;
      for (const { member, score } of sorted) { items.push(`${i++}) "${member}"`); items.push(`${i++}) "${score}"`); }
      return items.join("\n");
    }
    return fmtArr(sorted.map((x) => x.member));
  }

  private ZREM(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'zrem' command");
    const [key, ...members] = args;
    const z = this.getZSet(key);
    let n = 0;
    for (const m of members) { if (z.delete(m)) n++; }
    return fmtInt(n);
  }

  private matchGlob(pattern: string, key: string): boolean {
    let r = "^";
    for (let i = 0; i < pattern.length; i++) {
      const c = pattern[i];
      if (c === "*") r += ".*";
      else if (c === "?") r += ".";
      else r += c.replace(/[.+^${}()|[\]\\]/g, "\\$&");
    }
    return new RegExp(r + "$").test(key);
  }

  private KEYS(args: string[]): string {
    const pattern = args[0] ?? "*";
    const keys: string[] = [];
    for (const [k] of this.store) {
      if (!this.isExpired(k) && this.matchGlob(pattern, k)) keys.push(k);
    }
    return fmtArr(keys.sort());
  }

  private TYPE(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'type' command");
    const e = this.getEntry(args[0]);
    if (!e) return "none";
    return e.type === "hll" ? "string" : e.type;
  }

  private RENAME(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'rename' command");
    const e = this.getEntry(args[0]);
    if (!e) return fmtErr("ERR no such key");
    this.store.set(args[1], e);
    this.store.delete(args[0]);
    return "OK";
  }

  private COPY(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'copy' command");
    const e = this.getEntry(args[0]);
    if (!e) return fmtInt(0);
    if (this.getEntry(args[1])) return fmtInt(0);
    this.store.set(args[1], { ...e });
    return fmtInt(1);
  }

  private SCAN(args: string[]): string {
    let pattern = "*";
    for (let i = 1; i < args.length; i++) {
      if (args[i].toUpperCase() === "MATCH") pattern = args[++i];
      else if (args[i].toUpperCase() === "COUNT") i++;
    }
    const keys: string[] = [];
    for (const [k] of this.store) {
      if (!this.isExpired(k) && this.matchGlob(pattern, k)) keys.push(k);
    }
    return `1) "0"\n2) ${fmtArr(keys.sort())}`;
  }

  private OBJECT(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'object' command");
    if (args[0].toUpperCase() === "ENCODING") {
      const e = this.getEntry(args[1]);
      if (!e) return fmtErr("ERR no such key");
      if (e.type === "string") {
        const v = e.value as string;
        return !isNaN(parseInt(v)) ? fmtBulk("int") : v.length <= 44 ? fmtBulk("embstr") : fmtBulk("raw");
      }
      return fmtBulk("listpack");
    }
    return fmtErr(`ERR unknown subcommand '${args[0]}'`);
  }

  private PFADD(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'pfadd' command");
    const [key, ...elements] = args;
    const e = this.store.get(key);
    let hll: Set<string>;
    if (e && e.type === "hll") {
      hll = e.value as Set<string>;
    } else {
      hll = new Set();
      this.store.set(key, { type: "hll", value: hll });
    }
    const old = hll.size;
    for (const el of elements) hll.add(el);
    return fmtInt(hll.size !== old ? 1 : 0);
  }

  private PFCOUNT(args: string[]): string {
    if (args.length < 1) return fmtErr("ERR wrong number of arguments for 'pfcount' command");
    const union = new Set<string>();
    for (const k of args) {
      const e = this.store.get(k);
      if (e && e.type === "hll") for (const el of e.value as Set<string>) union.add(el);
    }
    return fmtInt(union.size);
  }

  private PFMERGE(args: string[]): string {
    if (args.length < 2) return fmtErr("ERR wrong number of arguments for 'pfmerge' command");
    const [dest, ...sources] = args;
    const merged = new Set<string>();
    for (const k of sources) {
      const e = this.store.get(k);
      if (e && e.type === "hll") for (const el of e.value as Set<string>) merged.add(el);
    }
    this.store.set(dest, { type: "hll", value: merged });
    return "OK";
  }

  run(code: string): string {
    const lines = code.split("\n").map((l) => l.trim()).filter((l) => l && !l.startsWith("#"));
    const outputs: string[] = [];
    for (const line of lines) {
      const args = parseArgs(line);
      if (args.length === 0) continue;
      outputs.push(this.executeCommand(args));
    }
    return outputs.join("\n");
  }
}
