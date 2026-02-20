"use client";

import type { RunResult, Test, TestResult } from "@/lib/lessons/types";

let musicRunnerReady = false;

export function initMusicRunner(): Promise<void> {
  musicRunnerReady = true;
  return Promise.resolve();
}

export function isMusicReady(): boolean {
  return musicRunnerReady;
}

// ---------------------------------------------------------------------------
// Silent AudioContext mock — used when running code in the main thread
// (for console.log capture only). Prevents audio from playing twice.
// ---------------------------------------------------------------------------

class AudioParamMock {
  value: number;
  constructor(val = 0) { this.value = val; }
  setValueAtTime() { return this; }
  linearRampToValueAtTime() { return this; }
  exponentialRampToValueAtTime() { return this; }
  setTargetAtTime() { return this; }
  cancelScheduledValues() { return this; }
}

class AudioNodeMock {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  connect(): any { return this; }
  disconnect() {}
}

class OscillatorNodeMock extends AudioNodeMock {
  type = "sine";
  frequency = new AudioParamMock(440);
  detune = new AudioParamMock(0);
  onended = null;
  start() {}
  stop() {}
  addEventListener() {}
}

class GainNodeMock extends AudioNodeMock {
  gain = new AudioParamMock(1);
}

class BiquadFilterNodeMock extends AudioNodeMock {
  type = "lowpass";
  frequency = new AudioParamMock(350);
  gain = new AudioParamMock(0);
  Q = new AudioParamMock(1);
  detune = new AudioParamMock(0);
}

class DelayNodeMock extends AudioNodeMock {
  delayTime = new AudioParamMock(0);
}

class DynamicsCompressorNodeMock extends AudioNodeMock {
  threshold = new AudioParamMock(-24);
  knee = new AudioParamMock(30);
  ratio = new AudioParamMock(12);
  attack = new AudioParamMock(0.003);
  release = new AudioParamMock(0.25);
  reduction = 0;
}

class BufferSourceNodeMock extends AudioNodeMock {
  buffer = null;
  loop = false;
  start() {}
  stop() {}
}

class AudioContextMock {
  destination = new AudioNodeMock();
  currentTime = 0;
  sampleRate = 44100;
  state = "running";
  createOscillator() { return new OscillatorNodeMock(); }
  createGain() { return new GainNodeMock(); }
  createBiquadFilter() { return new BiquadFilterNodeMock(); }
  createDelay() { return new DelayNodeMock(); }
  createDynamicsCompressor() { return new DynamicsCompressorNodeMock(); }
  createBufferSource() { return new BufferSourceNodeMock(); }
  createBuffer() { return {}; }
  createStereoPanner() { return new AudioNodeMock(); }
  createAnalyser() { return new AudioNodeMock(); }
  createChannelSplitter() { return new AudioNodeMock(); }
  createChannelMerger() { return new AudioNodeMock(); }
  resume() { return Promise.resolve(); }
  suspend() { return Promise.resolve(); }
  close() { return Promise.resolve(); }
}

// ---------------------------------------------------------------------------
// Preview HTML — runs in iframe with REAL AudioContext → actual audio output
// ---------------------------------------------------------------------------

function buildPreviewHtml(code: string): string {
  const escaped = code.replace(/\\/g, "\\\\").replace(/`/g, "\\`").replace(/\$/g, "\\$");
  return `<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    background: #0a0a0f;
    color: #e2e8f0;
    font-family: monospace;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100vh;
    gap: 20px;
  }
  .title { color: #a78bfa; font-size: 13px; letter-spacing: 0.1em; text-transform: uppercase; }
  .bars {
    display: flex;
    gap: 5px;
    align-items: flex-end;
    height: 60px;
  }
  .bar {
    width: 6px;
    background: linear-gradient(to top, #7c3aed, #a78bfa);
    border-radius: 3px 3px 0 0;
    animation: wave 0.8s ease-in-out infinite;
  }
  .bar:nth-child(1)  { animation-delay: 0.0s; }
  .bar:nth-child(2)  { animation-delay: 0.1s; }
  .bar:nth-child(3)  { animation-delay: 0.2s; }
  .bar:nth-child(4)  { animation-delay: 0.3s; }
  .bar:nth-child(5)  { animation-delay: 0.4s; }
  .bar:nth-child(6)  { animation-delay: 0.3s; }
  .bar:nth-child(7)  { animation-delay: 0.2s; }
  .bar:nth-child(8)  { animation-delay: 0.1s; }
  .bar:nth-child(9)  { animation-delay: 0.0s; }
  @keyframes wave {
    0%, 100% { height: 12px; opacity: 0.5; }
    50%       { height: 52px; opacity: 1.0; }
  }
  #log {
    color: #64748b;
    font-size: 11px;
    max-width: 340px;
    text-align: center;
    line-height: 1.6;
    white-space: pre-wrap;
  }
  #error { color: #f87171; font-size: 12px; max-width: 340px; text-align: left; white-space: pre-wrap; }
</style>
</head>
<body>
<div class="title">♪ Web Audio</div>
<div class="bars">
  <div class="bar"></div><div class="bar"></div><div class="bar"></div>
  <div class="bar"></div><div class="bar"></div><div class="bar"></div>
  <div class="bar"></div><div class="bar"></div><div class="bar"></div>
</div>
<div id="log"></div>
<div id="error"></div>
<script>
  const _log = console.log.bind(console);
  const _logEl = document.getElementById('log');
  const _lines = [];
  console.log = (...args) => {
    _lines.push(args.map(String).join(' '));
    _logEl.textContent = _lines.join('\\n');
    _log(...args);
  };
  try {
    eval(\`${escaped}\`);
  } catch(e) {
    document.getElementById('error').textContent = e.message;
  }
<\/script>
</body>
</html>`;
}

// ---------------------------------------------------------------------------
// Extract top-level function declarations (same logic as threejs-runner)
// ---------------------------------------------------------------------------

function extractFunctions(code: string): string {
  const lines = code.split("\n");
  const blocks: string[] = [];
  let depth = 0;
  let inBlock = false;
  let current: string[] = [];

  for (const line of lines) {
    const trimmed = line.trim();
    const isFuncDecl = /^function\s+\w+/.test(trimmed);
    const isArrowOrExpr = /^(const|let|var)\s+\w+\s*=\s*(async\s+)?(function|\()/.test(trimmed);

    if (!inBlock && (isFuncDecl || isArrowOrExpr)) {
      inBlock = true;
      current = [line];
      depth = 0;
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth === 0 && !trimmed.endsWith("{")) {
        blocks.push(line);
        current = [];
        inBlock = false;
      }
    } else if (inBlock) {
      current.push(line);
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth <= 0) {
        blocks.push(current.join("\n"));
        current = [];
        inBlock = false;
        depth = 0;
      }
    }
  }

  return blocks.join("\n\n");
}

// ---------------------------------------------------------------------------
// Run code in main thread with silent AudioContext mock → capture console.log
// ---------------------------------------------------------------------------

function runWithMock(code: string): { stdout: string; errorMsg: string } {
  let stdout = "";
  let errorMsg = "";

  // Swap in silent mock
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const win = window as any;
  const origAC = win.AudioContext;
  const origWAC = win.webkitAudioContext;
  win.AudioContext = AudioContextMock;
  win.webkitAudioContext = AudioContextMock;

  const originalLog = console.log;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  console.log = (...args: any[]) => {
    stdout += args.map((a) => (typeof a === "object" && a !== null ? JSON.stringify(a) : String(a))).join(" ") + "\n";
  };

  try {
    // eslint-disable-next-line no-new-func
    new Function(code)();
  } catch (e: unknown) {
    const msg = (e as Error).message ?? String(e);
    // Suppress expected Web Audio errors that happen outside real browser context
    const isExpected = /audiocontext|audioparam|oscillator|gainnode|biquad|webaudio/i.test(msg);
    if (!isExpected) errorMsg = msg;
  } finally {
    console.log = originalLog;
    win.AudioContext = origAC;
    win.webkitAudioContext = origWAC;
  }

  return { stdout, errorMsg };
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export async function runMusic(code: string): Promise<RunResult> {
  const { stdout, errorMsg } = runWithMock(code);
  const previewHtml = buildPreviewHtml(code);
  return { stdout, stderr: "", error: errorMsg, previewHtml };
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
  const userFunctions = extractFunctions(code);

  return tests.map((test) => {
    const testCode = test.code ? test.code.replace("{{FUNC}}", userFunctions) : code;
    const { stdout: actual, errorMsg } = runWithMock(testCode);

    return {
      name: test.name,
      passed: !errorMsg && actual === test.expected,
      actual: errorMsg || actual,
      expected: test.expected,
    };
  });
}
