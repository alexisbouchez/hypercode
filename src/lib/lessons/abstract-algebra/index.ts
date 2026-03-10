import type { Chapter, Lesson } from "../types";
import { setsAndOperations } from "./data/01-sets-and-operations";
import { groups } from "./data/02-groups";
import { subgroups } from "./data/03-subgroups";
import { cyclicGroups } from "./data/04-cyclic-groups";
import { permutations } from "./data/05-permutations";
import { groupHomomorphisms } from "./data/06-group-homomorphisms";
import { cosetsLagrange } from "./data/07-cosets-lagrange";
import { quotientGroups } from "./data/08-quotient-groups";
import { rings } from "./data/09-rings";
import { ideals } from "./data/10-ideals";
import { polynomialRings } from "./data/11-polynomial-rings";
import { ringHomomorphisms } from "./data/12-ring-homomorphisms";
import { fields } from "./data/13-fields";
import { finiteFields } from "./data/14-finite-fields";
import { cryptoApplications } from "./data/15-crypto-applications";

export const abstractAlgebraChapters: Chapter[] = [
	{ id: "groups", title: "Group Theory" },
	{ id: "homomorphisms", title: "Homomorphisms & Quotients" },
	{ id: "rings", title: "Ring Theory" },
	{ id: "fields", title: "Fields & Applications" },
];

export const abstractAlgebraLessons: Lesson[] = [
	setsAndOperations,
	groups,
	subgroups,
	cyclicGroups,
	permutations,
	groupHomomorphisms,
	cosetsLagrange,
	quotientGroups,
	rings,
	ideals,
	polynomialRings,
	ringHomomorphisms,
	fields,
	finiteFields,
	cryptoApplications,
];
