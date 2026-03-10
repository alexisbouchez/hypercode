import type { Chapter, Lesson } from "../types";
import { tokenizingNumbersAndOperators } from "./data/01-tokenizing-numbers-and-operators";
import { tokenizingFullArithmetic } from "./data/02-tokenizing-full-arithmetic";
import { stringLiteralTokens } from "./data/03-string-literal-tokens";
import { completeLexerClass } from "./data/04-complete-lexer-class";
import { astNodeTypes } from "./data/05-ast-node-types";
import { parsingAdditionSubtraction } from "./data/06-parsing-addition-subtraction";
import { operatorPrecedence } from "./data/07-operator-precedence";
import { parenthesizedExpressions } from "./data/08-parenthesized-expressions";
import { evaluatingAST } from "./data/09-evaluating-ast";
import { variablesAndAssignment } from "./data/10-variables-and-assignment";
import { builtinFunctions } from "./data/11-builtin-functions";
import { ifElseExpressions } from "./data/12-if-else-expressions";
import { closuresAndScoping } from "./data/13-closures-and-scoping";
import { errorHandling } from "./data/14-error-handling";
import { completeMiniLanguage } from "./data/15-complete-mini-language";

export const compilerChapters: Chapter[] = [
	{ id: "lexing", title: "Lexing" },
	{ id: "parsing", title: "Parsing" },
	{ id: "evaluation", title: "Evaluation" },
	{ id: "advanced", title: "Advanced" },
];

export const compilerLessons: Lesson[] = [
	tokenizingNumbersAndOperators,
	tokenizingFullArithmetic,
	stringLiteralTokens,
	completeLexerClass,
	astNodeTypes,
	parsingAdditionSubtraction,
	operatorPrecedence,
	parenthesizedExpressions,
	evaluatingAST,
	variablesAndAssignment,
	builtinFunctions,
	ifElseExpressions,
	closuresAndScoping,
	errorHandling,
	completeMiniLanguage,
];
