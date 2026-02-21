import type { Chapter, Lesson } from "../types";
import { gcdLcm } from "./data/01-gcd-lcm";
import { primeFactorization } from "./data/02-prime-factorization";
import { divisors } from "./data/03-divisors";
import { perfectNumbers } from "./data/04-perfect-numbers";
import { gcdExtended } from "./data/05-gcd-extended";
import { primality } from "./data/06-primality";
import { sieveOfEratosthenes } from "./data/07-sieve";
import { primeGaps } from "./data/08-prime-gaps";
import { goldbach } from "./data/09-goldbach";
import { primeCounting } from "./data/10-prime-counting";
import { modularArithmetic } from "./data/11-modular-arithmetic";
import { eulerTotient } from "./data/12-euler-totient";
import { chineseRemainder } from "./data/13-chinese-remainder";
import { fermatsLittle } from "./data/14-fermats-little";
import { rsaBasics } from "./data/15-rsa-basics";

export const numberTheoryChapters: Chapter[] = [
	{ id: "divisibility", title: "Divisibility" },
	{ id: "primes", title: "Prime Numbers" },
	{ id: "modular", title: "Modular Arithmetic" },
	{ id: "applications", title: "Applications" },
];

export const numberTheoryLessons: Lesson[] = [
	gcdLcm,
	primeFactorization,
	divisors,
	perfectNumbers,
	gcdExtended,
	primality,
	sieveOfEratosthenes,
	primeGaps,
	goldbach,
	primeCounting,
	modularArithmetic,
	eulerTotient,
	chineseRemainder,
	fermatsLittle,
	rsaBasics,
];
