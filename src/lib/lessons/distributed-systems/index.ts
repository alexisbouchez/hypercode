import type { Chapter, Lesson } from "../types";
import { lamportClocks } from "./data/01-lamport-clocks";
import { vectorClocks } from "./data/02-vector-clocks";
import { consistentHashing } from "./data/03-consistent-hashing";
import { leaderElection } from "./data/04-leader-election";
import { quorum } from "./data/05-quorum";
import { crdt } from "./data/06-crdt";
import { bloomFilter } from "./data/07-bloom-filter";
import { rateLimiter } from "./data/08-rate-limiter";
import { circuitBreaker } from "./data/09-circuit-breaker";
import { lruCache } from "./data/10-lru-cache";
import { gossipProtocol } from "./data/11-gossip-protocol";
import { twoPhaseCommit } from "./data/12-two-phase-commit";

export const distributedSystemsChapters: Chapter[] = [
	{ id: "clocks", title: "Clocks & Ordering" },
	{ id: "data-distribution", title: "Data Distribution" },
	{ id: "consensus", title: "Consensus" },
	{ id: "replication", title: "Replication" },
	{ id: "probabilistic", title: "Probabilistic Structures" },
	{ id: "fault-tolerance", title: "Fault Tolerance" },
];

export const distributedSystemsLessons: Lesson[] = [
	lamportClocks,
	vectorClocks,
	consistentHashing,
	leaderElection,
	twoPhaseCommit,
	quorum,
	crdt,
	gossipProtocol,
	bloomFilter,
	rateLimiter,
	circuitBreaker,
	lruCache,
];
