import { commands, window, workspace, ExtensionContext, Disposable } from 'vscode';
import { CancellationStrategy, LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

function startLanguageServer(command: string): Disposable {
    let serverOptions: ServerOptions = { command };
    
    let clientOptions: LanguageClientOptions = {
        documentSelector: [ { scheme: 'file', language: 'tic' } ],
    };

    let disposable = new LanguageClient(
        'tic-server',
        'Tic Language Server',
		serverOptions,
		clientOptions
    ).start();

    return disposable;
}

class ServerDisposable extends Disposable {
    public currentServer: Disposable | null = null;

    constructor() {
        super(() => this.currentServer?.dispose());
    }

    public replaceServer(newServer: Disposable | null) {
        this.currentServer?.dispose();
        this.currentServer = newServer;
    }
}

export function activate(context: ExtensionContext) {
    let server = workspace.getConfiguration().get('tic.languageServerPath', 'ticc-lsp');
    let serverDisposable = new ServerDisposable();
    context.subscriptions.push(serverDisposable);
    if (server !== null) {
        serverDisposable.replaceServer(startLanguageServer(server));
    }

    const restartCommandDisposable = commands.registerCommand('tic.restartServer', () => {
        if (server !== null) {
            window.showInformationMessage('Restarting tic language server');
            serverDisposable.replaceServer(startLanguageServer(server));
        } else {
            window.showErrorMessage('No tic language server configured');
        }
	});

    context.subscriptions.push(restartCommandDisposable);
    
    const shutdownCommandDisposable = commands.registerCommand('tic.shutdownServer', () => {
        if (serverDisposable.currentServer !== null) {
            window.showInformationMessage('Shutting down tic language server');
            serverDisposable.replaceServer(null);
        } else {
            window.showErrorMessage('Tic language server is not running');
        }
	});

	context.subscriptions.push(shutdownCommandDisposable);
}
