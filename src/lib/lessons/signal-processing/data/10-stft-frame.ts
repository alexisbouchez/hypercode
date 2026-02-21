import type { Lesson } from "../../types";

export const stftFrame: Lesson = {
	id: "stft-frame",
	title: "STFT Framing",
	chapterId: "fourier",
	content: `## Short-Time Fourier Transform Framing

The **Short-Time Fourier Transform (STFT)** analyzes how the frequency content of a signal changes over time. The key idea: divide the signal into short overlapping **frames**, then apply the DFT to each frame.

### Framing

Given a signal $x$ of length $L$, extract frames of size $F$ with a **hop size** $H$:

$$\\text{frame}_i = x[i \\cdot H : i \\cdot H + F]$$

Frames are extracted as long as the full frame fits within the signal. The number of frames is:

$$n_{\\text{frames}} = \\lfloor (L - F) / H \\rfloor + 1$$

**Overlap:** When $H < F$, frames overlap. A 50% overlap means $H = F/2$.

### STFT Frame Spectrum

For each frame, apply a window and compute the DFT magnitude:

$$|X_i[k]| = |\\text{DFT}(\\text{frame}_i \\cdot w)|$$

The result is a 2D time-frequency representation called a **spectrogram**.

### Example

Signal $[0,1,2,3,4,5,6,7]$, frame size $4$, hop $2$:
- Frame 0: $[0,1,2,3]$
- Frame 1: $[2,3,4,5]$
- Frame 2: $[4,5,6,7]$

### Your Task

Implement:
- \`frame_signal(x, frame_size, hop)\` — returns list of frames (each a list of floats)
- \`stft_frame_spectrum(frame, window)\` — returns $|\\text{DFT}(\\text{frame} \\cdot \\text{window})|$

\`\`\`python
import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def frame_signal(x, frame_size, hop):
    frames = []
    i = 0
    while i + frame_size <= len(x):
        frames.append(x[i:i + frame_size])
        i += hop
    return frames

def stft_frame_spectrum(frame, window):
    windowed = [f * w for f, w in zip(frame, window)]
    return [abs(v) for v in dft(windowed)]
\`\`\``,

	starterCode: `import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def hann_window(N):
    return [0.5 * (1 - math.cos(2 * math.pi * n / (N - 1))) for n in range(N)]

def frame_signal(x, frame_size, hop):
    # Extract overlapping frames; include frame only if it fits completely
    pass

def stft_frame_spectrum(frame, window):
    # Apply window then DFT, return magnitudes
    pass

x = list(range(8))
frames = frame_signal(x, 4, 2)
print(len(frames))
`,

	solution: `import math

def dft(x):
    N = len(x)
    X = []
    for k in range(N):
        val = 0 + 0j
        for n in range(N):
            val += x[n] * math.e ** (-2j * math.pi * k * n / N)
        X.append(val)
    return X

def hann_window(N):
    return [0.5 * (1 - math.cos(2 * math.pi * n / (N - 1))) for n in range(N)]

def frame_signal(x, frame_size, hop):
    frames = []
    i = 0
    while i + frame_size <= len(x):
        frames.append(x[i:i + frame_size])
        i += hop
    return frames

def stft_frame_spectrum(frame, window):
    windowed = [f * w for f, w in zip(frame, window)]
    return [abs(v) for v in dft(windowed)]

x = list(range(8))
frames = frame_signal(x, 4, 2)
print(len(frames))
`,

	tests: [
		{
			name: "frame_signal([0..7], 4, 2) produces 3 frames",
			code: `{{FUNC}}
x = list(range(8))
frames = frame_signal(x, 4, 2)
print(len(frames))`,
			expected: "3\n",
		},
		{
			name: "frame_signal frames have correct content",
			code: `{{FUNC}}
x = list(range(8))
frames = frame_signal(x, 4, 2)
print(frames[0])
print(frames[1])`,
			expected: "[0, 1, 2, 3]\n[2, 3, 4, 5]\n",
		},
		{
			name: "stft_frame_spectrum of [1,2,3,4] with hann_window",
			code: `{{FUNC}}
frame = [1.0, 2.0, 3.0, 4.0]
window = hann_window(4)
spec = stft_frame_spectrum(frame, window)
print([round(v, 4) for v in spec])`,
			expected: "[3.75, 2.7042, 0.75, 2.7042]\n",
		},
		{
			name: "frame_signal with hop=frame_size has no overlap",
			code: `{{FUNC}}
x = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
frames = frame_signal(x, 3, 3)
print(len(frames))
print(frames[0])`,
			expected: "2\n[1.0, 2.0, 3.0]\n",
		},
	],
};
