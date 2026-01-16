<p align="center"><code>npm i -g @openai/hypercode</code><br />or <code>brew install --cask hypercode</code></p>
<p align="center"><strong>Hypercode CLI</strong> is a coding agent from OpenAI that runs locally on your computer.
<p align="center">
  <img src="./.github/hypercode-cli-splash.png" alt="Hypercode CLI splash" width="80%" />
</p>
</br>
If you want Hypercode in your code editor (VS Code, Cursor, Windsurf), <a href="https://developers.openai.com/hypercode/ide">install in your IDE.</a>
</br>If you are looking for the <em>cloud-based agent</em> from OpenAI, <strong>Hypercode Web</strong>, go to <a href="https://chatgpt.com/hypercode">chatgpt.com/hypercode</a>.</p>

---

## Quickstart

### Installing and running Hypercode CLI

Install globally with your preferred package manager:

```shell
# Install using npm
npm install -g @openai/hypercode
```

```shell
# Install using Homebrew
brew install --cask hypercode
```

Then simply run `hypercode` to get started.

<details>
<summary>You can also go to the <a href="https://github.com/openai/hypercode/releases/latest">latest GitHub Release</a> and download the appropriate binary for your platform.</summary>

Each GitHub Release contains many executables, but in practice, you likely want one of these:

- macOS
  - Apple Silicon/arm64: `hypercode-aarch64-apple-darwin.tar.gz`
  - x86_64 (older Mac hardware): `hypercode-x86_64-apple-darwin.tar.gz`
- Linux
  - x86_64: `hypercode-x86_64-unknown-linux-musl.tar.gz`
  - arm64: `hypercode-aarch64-unknown-linux-musl.tar.gz`

Each archive contains a single entry with the platform baked into the name (e.g., `hypercode-x86_64-unknown-linux-musl`), so you likely want to rename it to `hypercode` after extracting it.

</details>

### Using Hypercode with your ChatGPT plan

Run `hypercode` and select **Sign in with ChatGPT**. We recommend signing into your ChatGPT account to use Hypercode as part of your Plus, Pro, Team, Edu, or Enterprise plan. [Learn more about what's included in your ChatGPT plan](https://help.openai.com/en/articles/11369540-hypercode-in-chatgpt).

You can also use Hypercode with an API key, but this requires [additional setup](https://developers.openai.com/hypercode/auth#sign-in-with-an-api-key).

## Docs

- [**Hypercode Documentation**](https://developers.openai.com/hypercode)
- [**Contributing**](./docs/contributing.md)
- [**Installing & building**](./docs/install.md)
- [**Open source fund**](./docs/open-source-fund.md)

This repository is licensed under the [Apache-2.0 License](LICENSE).
