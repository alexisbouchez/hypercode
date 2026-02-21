import type { Chapter, Lesson } from "../types";
import { caesarCipher } from "./data/01-caesar-cipher";
import { vigenereCipher } from "./data/02-vigenere";
import { xorCipher } from "./data/03-xor-cipher";
import { affineCipher } from "./data/04-affine-cipher";
import { blockCipherModes } from "./data/05-block-cipher-modes";
import { feistelCipher } from "./data/06-feistel";
import { diffieHellman } from "./data/07-diffie-hellman";
import { rsaSystem } from "./data/08-rsa-system";
import { elgamal } from "./data/09-elgamal";
import { simpleHash } from "./data/10-simple-hash";
import { hmacSimple } from "./data/11-hmac-simple";
import { birthdayParadox } from "./data/12-birthday-paradox";
import { commitmentScheme } from "./data/13-commitment-scheme";
import { secretSharing } from "./data/14-secret-sharing";
import { zeroKnowledge } from "./data/15-zero-knowledge";

export const cryptographyChapters: Chapter[] = [
	{ id: "classical", title: "Classical Ciphers" },
	{ id: "symmetric", title: "Symmetric Cryptography" },
	{ id: "asymmetric", title: "Asymmetric Cryptography" },
	{ id: "hash", title: "Hash Functions & MAC" },
	{ id: "protocols", title: "Cryptographic Protocols" },
];

export const cryptographyLessons: Lesson[] = [
	caesarCipher,
	vigenereCipher,
	xorCipher,
	affineCipher,
	blockCipherModes,
	feistelCipher,
	diffieHellman,
	rsaSystem,
	elgamal,
	simpleHash,
	hmacSimple,
	birthdayParadox,
	commitmentScheme,
	secretSharing,
	zeroKnowledge,
];
