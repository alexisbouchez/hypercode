# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
npm run dev          # Start Next.js dev server
npm run build        # Production build
npm run lint         # Run ESLint
npm run test         # Run all lesson tests (bun src/scripts/test-lessons.ts)
npm run test:e2e     # Playwright end-to-end tests
```

To run a single course's tests, invoke the test runner directly:
```bash
bun src/scripts/test-runners/go-test-runner.ts
bun src/scripts/test-runners/c-test-runner.ts
# etc.
```

## Architecture

Hypercode is a Next.js in-browser interactive coding platform. All code execution happens client-side via WASM runtimes — there is no server-side code compilation.

### Course & Lesson Structure

Each course lives under `src/lib/lessons/<lang>/`:
- `index.ts` — exports `chapters: Chapter[]` and `lessons: Lesson[]`
- `data/01-topic.ts`, `data/02-topic.ts`, … — individual lesson files

A `Lesson` has: `id`, `title`, `chapterId`, `content` (Markdown), `starterCode`, `solution`, and `tests: { name, expected, code? }[]`.

Course registrations (metadata, descriptions, routing slugs) live in `src/lib/courses/index.ts`.

### Runner Pattern

Every language has a runner at `src/lib/<lang>-runner.ts` implementing:
```typescript
init<Lang>Runner(): Promise<void>   // Load WASM / runtime
is<Lang>Ready(): boolean            // Check initialization
run<Lang>(code: string): Promise<RunResult>
runTests(code: string, tests: Test[]): Promise<TestResult[]>
```

The central dispatcher is `src/components/lesson-shell-wrapper.tsx`, which maps `courseId` → runner functions and passes them to `<LessonShell>`.

### Runtimes by Language

| Course | Runtime |
|--------|---------|
| Go | Yaegi WASM (`window._yaegiRun`) |
| Zig | Zig compiler WASM |
| PostgreSQL | PGlite (in-browser Postgres) |
| ARM64 | Custom assembler + interpreter (`src/lib/arm64/`) |
| C | TCC WASM → ELF → ARM64 disassembler → interpreter |
| Gleam | Official Gleam compiler WASM → JavaScript |
| R | webR WASM |
| HolyC | Aiwnios HolyC compiler |
| JavaScript | `new Function(code)()` with console capture |
| TypeScript | Transpile via `@typescript/compiler` → execute |
| Ruby | `@ruby/wasm-wasi` RubyVM |
| Linux/Bash | Shell emulator |
| Three.js | Three.js scene runner |
| Algorithms, Distributed Systems, Trees, Coreutils, Kernel | JavaScript/C via existing runners |

### UI Routing

The single dynamic route `src/app/[courseId]/lessons/[lessonId]/page.tsx` handles all courses. The layout at `src/app/[courseId]/layout.tsx` wraps with course-specific theming.

Key components:
- `lesson-shell-wrapper.tsx` — runner dispatch hub
- `lesson-shell.tsx` — split-panel editor + output UI (Monaco + run/test buttons)
- `code-editor.tsx` — Monaco editor wrapper
- `sidebar.tsx` — course/chapter navigation

### Test Infrastructure

`src/scripts/test-lessons.ts` orchestrates all test runners in sequence. Each runner at `src/scripts/test-runners/<lang>-test-runner.ts` uses the Node/Bun environment (not browser WASM) to execute lesson solutions and verify expected output. Tests run in CI via `.github/workflows/test-lessons.yml`.

### Adding a New Course

1. Create `src/lib/lessons/<lang>/index.ts` exporting `chapters` and `lessons`
2. Add lesson data files under `src/lib/lessons/<lang>/data/`
3. Register the course in `src/lib/courses/index.ts`
4. Create (or reuse) a runner at `src/lib/<lang>-runner.ts`
5. Wire the runner into `src/components/lesson-shell-wrapper.tsx`
6. Create `src/scripts/test-runners/<lang>-test-runner.ts` and add it to `src/scripts/test-lessons.ts`

### Next.js Config Notes

`next.config.ts` sets `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp` headers globally — required for `SharedArrayBuffer` used by WASM runtimes.
