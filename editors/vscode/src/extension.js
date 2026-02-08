const fs = require("fs");
const path = require("path");
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;

function getWorkspaceRoot() {
  const folder = vscode.workspace.workspaceFolders?.[0];
  return folder ? folder.uri.fsPath : undefined;
}

function normalizeConfiguredPath(configuredPath, workspaceRoot) {
  if (!configuredPath) {
    return undefined;
  }

  if (path.isAbsolute(configuredPath)) {
    return configuredPath;
  }

  if (workspaceRoot) {
    return path.join(workspaceRoot, configuredPath);
  }

  return configuredPath;
}

function resolveServerCommand() {
  const config = vscode.workspace.getConfiguration("yuu");
  const workspaceRoot = getWorkspaceRoot();
  const configuredPath = config.get("lsp.path", "").trim();

  const normalizedConfiguredPath = normalizeConfiguredPath(
    configuredPath,
    workspaceRoot
  );
  if (normalizedConfiguredPath) {
    if (
      path.isAbsolute(normalizedConfiguredPath) &&
      !fs.existsSync(normalizedConfiguredPath)
    ) {
      vscode.window.showWarningMessage(
        `Configured yuu binary not found: ${normalizedConfiguredPath}`
      );
    } else {
      return normalizedConfiguredPath;
    }
  }

  if (workspaceRoot) {
    const releaseBinary = path.join(workspaceRoot, "target", "release", "yuu");
    if (fs.existsSync(releaseBinary)) {
      return releaseBinary;
    }

    const debugBinary = path.join(workspaceRoot, "target", "debug", "yuu");
    if (fs.existsSync(debugBinary)) {
      return debugBinary;
    }
  }

  return "yuu";
}

function resolveServerArgs() {
  const config = vscode.workspace.getConfiguration("yuu");
  const args = config.get("lsp.args", ["lsp"]);
  return Array.isArray(args) ? args : ["lsp"];
}

async function startLanguageClient(context) {
  if (client) {
    return;
  }

  const command = resolveServerCommand();
  const args = resolveServerArgs();
  const workspaceRoot = getWorkspaceRoot();

  const outputChannel = vscode.window.createOutputChannel("Yuu Language Server");
  context.subscriptions.push(outputChannel);

  const serverOptions = {
    run: {
      command,
      args,
      options: {
        cwd: workspaceRoot
      }
    },
    debug: {
      command,
      args,
      options: {
        cwd: workspaceRoot
      }
    }
  };

  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "yuu" }],
    outputChannel,
    synchronize: {
      configurationSection: "yuu"
    }
  };

  client = new LanguageClient(
    "yuuLanguageServer",
    "Yuu Language Server",
    serverOptions,
    clientOptions
  );

  try {
    await client.start();
  } catch (error) {
    client = undefined;
    vscode.window.showErrorMessage(
      `Failed to start Yuu language server with command "${command} ${args.join(
        " "
      )}": ${error}`
    );
  }
}

async function stopLanguageClient() {
  if (!client) {
    return;
  }
  const activeClient = client;
  client = undefined;
  await activeClient.stop();
}

async function restartLanguageClient(context) {
  await stopLanguageClient();
  await startLanguageClient(context);
}

async function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("yuu.restartLanguageServer", async () => {
      await restartLanguageClient(context);
      vscode.window.showInformationMessage("Yuu language server restarted.");
    })
  );

  await startLanguageClient(context);

  context.subscriptions.push({
    dispose: () => {
      void stopLanguageClient();
    }
  });
}

async function deactivate() {
  await stopLanguageClient();
}

module.exports = {
  activate,
  deactivate
};
