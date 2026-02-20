import type { Chapter, Lesson } from "../types";
import { typeAnnotations } from "./data/01-type-annotations";
import { arrays } from "./data/02-arrays";
import { functionTypes } from "./data/03-function-types";
import { interfaces } from "./data/04-interfaces";
import { typeAliases } from "./data/05-type-aliases";
import { optionalProperties } from "./data/06-optional-properties";
import { unionTypes } from "./data/07-union-types";
import { typeNarrowing } from "./data/08-type-narrowing";
import { literalTypes } from "./data/09-literal-types";
import { generics } from "./data/10-generics";
import { genericConstraints } from "./data/11-generic-constraints";
import { classes } from "./data/12-classes";
import { enums } from "./data/13-enums";
import { readonly } from "./data/14-readonly";
import { typeGuards } from "./data/15-type-guards";

export const tsChapters: Chapter[] = [
	{ id: "basics", title: "Basics" },
	{ id: "interfaces", title: "Interfaces" },
	{ id: "type-system", title: "Type System" },
	{ id: "generics", title: "Generics" },
	{ id: "classes", title: "Classes" },
	{ id: "advanced", title: "Advanced" },
];

export const tsLessons: Lesson[] = [
	typeAnnotations,
	arrays,
	functionTypes,
	interfaces,
	typeAliases,
	optionalProperties,
	unionTypes,
	typeNarrowing,
	literalTypes,
	generics,
	genericConstraints,
	classes,
	enums,
	readonly,
	typeGuards,
];
