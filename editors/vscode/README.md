# Yuu VS Code Extension (Local Dev)

This extension launches the integrated language server from the Yuu compiler (`yuu lsp`) using the official VS Code language client library.

## Features

- Registers `*.yuu` files as language `yuu`
- Starts the Yuu language server over stdio
- Supports diagnostics, completion, hover, and go-to-definition
- Command: `Yuu: Restart Language Server`

## Setup

1. Build the compiler in the repo root:

```bash
cargo build
```

2. Install extension dependencies:

```bash
cd editors/vscode
npm install
```

3. Open `editors/vscode` in VS Code and press `F5` to run an Extension Development Host.

## Configuration

- `yuu.lsp.path`:
Path to the `yuu` binary. If empty, the extension auto-detects:
`<workspace>/target/release/yuu`, then `<workspace>/target/debug/yuu`, then `yuu` from `PATH`.

- `yuu.lsp.args`:
Arguments passed to the server process. Default: `["lsp"]`.
