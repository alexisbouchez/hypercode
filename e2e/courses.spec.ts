import { test, expect } from "@playwright/test";

const courses = [
  {
    courseId: "go",
    firstLessonId: "hello-world",
    solution: `package main

import "fmt"

func main() {
\tfmt.Println("Hello, World!")
}`,
  },
  {
    courseId: "zig",
    firstLessonId: "hello-world",
    solution: `const std = @import("std");

pub fn main() void {
\tstd.debug.print("Hello, World!\\n", .{});
}`,
  },
  {
    courseId: "postgresql",
    firstLessonId: "intro-to-databases",
    solution: "SELECT * FROM products;",
  },
  {
    courseId: "arm64",
    firstLessonId: "hello-arm64",
    solution: `.data
msg:
\t.ascii "Hello, ARM64!\\n"

.text
.global _start
_start:
\tMOV X0, #1
\tLDR X1, =msg
\tMOV X2, #14
\tMOV X8, #64
\tSVC #0

\tMOV X0, #0
\tMOV X8, #93
\tSVC #0`,
  },
  {
    courseId: "c",
    firstLessonId: "hello-c",
    solution: `#include <stdio.h>

int main() {
\tprintf("Hello, C!\\n");
\treturn 0;
}`,
  },
  {
    courseId: "gleam",
    firstLessonId: "hello-world",
    solution: `import gleam/io

pub fn main() {
\tio.println("Hello, World!")
}`,
  },
  {
    courseId: "r",
    firstLessonId: "hello-world",
    solution: `cat("Hello, World!\\n")`,
  },
];

for (const { courseId, firstLessonId, solution } of courses) {
  test(`${courseId} — first lesson runs and passes`, async ({ page }) => {
    await page.goto(`/${courseId}/lessons/${firstLessonId}`);

    // Wait for the runtime to be ready
    await expect(
      page.locator("[data-testid='runtime-status']"),
    ).toContainText("ready", { timeout: 120_000 });

    // Set solution code via exposed test hook (React state setter)
    await page.waitForFunction(
      () => typeof (window as any).__e2eSetCode === "function",
      { timeout: 30_000 },
    );
    await page.evaluate((code) => {
      (window as any).__e2eSetCode(code);
    }, solution);

    // Wait for React to re-render with the new code
    await page.waitForTimeout(200);

    // Click Run
    await page.locator("[data-testid='run-button']").click();

    // Wait for "All tests passed!"
    await expect(
      page.locator("[data-testid='all-passed']"),
    ).toBeVisible({ timeout: 30_000 });
  });
}
