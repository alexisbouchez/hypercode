export interface RedisValidationSpec {
  type: "contains" | "exact" | "lastLine" | "lineCount";
  value?: string | number;
}

export function parseRedisValidation(expected: string): RedisValidationSpec {
  try {
    return JSON.parse(expected) as RedisValidationSpec;
  } catch {
    return { type: "exact", value: expected };
  }
}

export function evaluateRedisValidation(
  spec: RedisValidationSpec,
  output: string,
): { passed: boolean; actual: string; expected: string } {
  const lines = output.split("\n").filter((l) => l.trim() !== "");

  switch (spec.type) {
    case "contains": {
      const val = String(spec.value ?? "");
      const found = output.includes(val);
      return {
        passed: found,
        actual: found ? `contains "${val}"` : `"${val}" not found in output`,
        expected: `output contains "${val}"`,
      };
    }
    case "exact": {
      const val = String(spec.value ?? "");
      return {
        passed: output.trim() === val.trim(),
        actual: output.trim(),
        expected: val.trim(),
      };
    }
    case "lastLine": {
      const val = String(spec.value ?? "");
      const last = lines[lines.length - 1] ?? "";
      return {
        passed: last === val,
        actual: last,
        expected: val,
      };
    }
    case "lineCount": {
      const expected = Number(spec.value);
      return {
        passed: lines.length === expected,
        actual: `${lines.length} lines`,
        expected: `${expected} lines`,
      };
    }
    default:
      return { passed: true, actual: output, expected: "" };
  }
}
