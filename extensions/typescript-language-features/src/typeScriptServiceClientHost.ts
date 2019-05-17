/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------------------------
 * Includes code from typescript-sublime-plugin project, obtained from
 * https://github.com/Microsoft/TypeScript-Sublime-Plugin/blob/master/TypeScript%20Indent.tmPreferences
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import { DiagnosticKind } from './features/diagnostics';
import FileConfigurationManager from './features/fileConfigurationManager';
import LanguageProvider from './languageProvider';
import * as Proto from './protocol';
import * as PConst from './protocol.const';
import TypeScriptServiceClient from './typescriptServiceClient';
import API from './utils/api';
import { CommandManager } from './utils/commandManager';
import { Disposable } from './utils/dispose';
import { DiagnosticLanguage, LanguageDescription } from './utils/languageDescription';
import LogDirectoryProvider from './utils/logDirectoryProvider';
import { PluginManager } from './utils/plugins';
import * as typeConverters from './utils/typeConverters';
import TypingsStatus, { AtaProgressReporter } from './utils/typingsStatus';
import VersionStatus from './utils/versionStatus';
import { IRevealArgs } from './commands/expandReveal';

// Style check diagnostics that can be reported as warnings
const styleCheckDiagnostics = [
	6133, 	// variable is declared but never used
	6138, 	// property is declared but its value is never read
	6192, 	// All imports are unused
	7027,	// unreachable code detected
	7028,	// unused label
	7029,	// fall through case in switch
	7030	// not all code paths return a value
];

export default class TypeScriptServiceClientHost extends Disposable {
	private readonly typingsStatus: TypingsStatus;
	private readonly client: TypeScriptServiceClient;
	private readonly languages: LanguageProvider[] = [];
	private readonly languagePerId = new Map<string, LanguageProvider>();
	private readonly versionStatus: VersionStatus;
	private readonly fileConfigurationManager: FileConfigurationManager;

	private reportStyleCheckAsWarnings: boolean = true;

	constructor(
		descriptions: LanguageDescription[],
		workspaceState: vscode.Memento,
		pluginManager: PluginManager,
		private readonly commandManager: CommandManager,
		logDirectoryProvider: LogDirectoryProvider,
		onCompletionAccepted: (item: vscode.CompletionItem) => void,
	) {
		super();
		const handleProjectCreateOrDelete = () => {
			this.triggerAllDiagnostics();
		};
		const handleProjectChange = () => {
			setTimeout(() => {
				this.triggerAllDiagnostics();
			}, 1500);
		};
		const configFileWatcher = this._register(vscode.workspace.createFileSystemWatcher('**/[tj]sconfig.json'));
		configFileWatcher.onDidCreate(handleProjectCreateOrDelete, this, this._disposables);
		configFileWatcher.onDidDelete(handleProjectCreateOrDelete, this, this._disposables);
		configFileWatcher.onDidChange(handleProjectChange, this, this._disposables);

		const allModeIds = this.getAllModeIds(descriptions);
		this.client = this._register(new TypeScriptServiceClient(
			workspaceState,
			version => this.versionStatus.onDidChangeTypeScriptVersion(version),
			pluginManager,
			logDirectoryProvider,
			allModeIds,
			(args, replacement) => this.applyReveal(args, replacement)
		));

		this.client.onDiagnosticsReceived(({ kind, resource, diagnostics }) => {
			this.diagnosticsReceived(kind, resource, diagnostics);
		}, null, this._disposables);

		this.client.onConfigDiagnosticsReceived(diag => this.configFileDiagnosticsReceived(diag), null, this._disposables);
		this.client.onResendModelsRequested(() => this.populateService(), null, this._disposables);

		this.versionStatus = this._register(new VersionStatus(resource => this.client.toPath(resource)));

		this._register(new AtaProgressReporter(this.client));
		this.typingsStatus = this._register(new TypingsStatus(this.client));
		this.fileConfigurationManager = this._register(new FileConfigurationManager(this.client));

		for (const description of descriptions) {
			const manager = new LanguageProvider(this.client, description, this.commandManager, this.client.telemetryReporter, this.typingsStatus, this.fileConfigurationManager, onCompletionAccepted);
			this.languages.push(manager);
			this._register(manager);
			this.languagePerId.set(description.id, manager);
		}

		import('./features/updatePathsOnRename').then(module =>
			this._register(module.register(this.client, this.fileConfigurationManager, uri => this.handles(uri))));

		import('./features/workspaceSymbols').then(module =>
			this._register(module.register(this.client, allModeIds)));

		this.client.ensureServiceStarted();
		this.client.onReady(() => {
			if (this.client.apiVersion.lt(API.v230)) {
				return;
			}

			const languages = new Set<string>();
			for (const plugin of pluginManager.plugins) {
				for (const language of plugin.languages) {
					languages.add(language);
				}
			}
			if (languages.size) {
				const description: LanguageDescription = {
					id: 'typescript-plugins',
					modeIds: Array.from(languages.values()),
					diagnosticSource: 'ts-plugin',
					diagnosticLanguage: DiagnosticLanguage.TypeScript,
					diagnosticOwner: 'typescript',
					isExternal: true
				};
				const manager = new LanguageProvider(this.client, description, this.commandManager, this.client.telemetryReporter, this.typingsStatus, this.fileConfigurationManager, onCompletionAccepted);
				this.languages.push(manager);
				this._register(manager);
				this.languagePerId.set(description.id, manager);
			}
		});

		this.client.onTsServerStarted(() => {
			this.triggerAllDiagnostics();
		});

		vscode.workspace.onDidChangeConfiguration(this.configurationChanged, this, this._disposables);
		this.configurationChanged();
	}

	private getAllModeIds(descriptions: LanguageDescription[]) {
		const allModeIds: string[] = [];
		for (const description of descriptions) {
			allModeIds.push(...description.modeIds);
		}
		return allModeIds;
	}

	public get serviceClient(): TypeScriptServiceClient {
		return this.client;
	}

	public reloadProjects(): void {
		this.client.executeWithoutWaitingForResponse('reloadProjects', null);
		this.triggerAllDiagnostics();
	}

	public async handles(resource: vscode.Uri): Promise<boolean> {
		const provider = await this.findLanguage(resource);
		if (provider) {
			return true;
		}
		return this.client.bufferSyncSupport.handles(resource);
	}

	private configurationChanged(): void {
		const typescriptConfig = vscode.workspace.getConfiguration('typescript');

		this.reportStyleCheckAsWarnings = typescriptConfig.get('reportStyleChecksAsWarnings', true);
	}

	private async findLanguage(resource: vscode.Uri): Promise<LanguageProvider | undefined> {
		try {
			const doc = await vscode.workspace.openTextDocument(resource);
			return this.languages.find(language => language.handles(resource, doc));
		} catch {
			return undefined;
		}
	}

	private triggerAllDiagnostics() {
		for (const language of this.languagePerId.values()) {
			language.triggerAllDiagnostics();
		}
	}

	private populateService(): void {
		this.fileConfigurationManager.reset();
		this.client.bufferSyncSupport.reOpenDocuments();
		this.client.bufferSyncSupport.requestAllDiagnostics();

		// See https://github.com/Microsoft/TypeScript/issues/5530
		vscode.workspace.saveAll(false).then(() => {
			for (const language of this.languagePerId.values()) {
				language.reInitialize();
			}
		});
	}

	private _protoDiagnosticCache = new Map<string, Map<DiagnosticKind, Proto.Diagnostic[]>>();

	private setProtoCache(
		kind: DiagnosticKind,
		resource: vscode.Uri,
		diagnostics: Proto.Diagnostic[]
	) {
		const fileCache = this._protoDiagnosticCache.get(resource.toString());
		if (fileCache) {
			fileCache.set(kind, diagnostics);
		}
		else {
			this._protoDiagnosticCache.set(resource.toString(), new Map<DiagnosticKind, Proto.Diagnostic[]>().set(kind, diagnostics));
		}
	}

	private clearProtoCache(
		kind: DiagnosticKind,
		resource: vscode.Uri,
	) {
		const fileCache = this._protoDiagnosticCache.get(resource.toString());
		if (fileCache) {
			fileCache.delete(kind);
			if (!fileCache.size) {
				this._protoDiagnosticCache.delete(resource.toString());
			}
		}
	}

	private async diagnosticsReceived(
		kind: DiagnosticKind,
		resource: vscode.Uri,
		diagnostics: Proto.Diagnostic[]
	): Promise<void> {
		const language = await this.findLanguage(resource);
		if (language) {
			// Retain the proto diagnostics for `reveal`-able diagnostics so the `reveal` can be applied later
			const containsRevealable = diagnostics.some(d => !!d.annotations && d.annotations.some(a => a.kind === 'reveal'));
			if (containsRevealable) {
				this.setProtoCache(kind, resource, diagnostics);
			}
			else {
				this.clearProtoCache(kind, resource);
			}
			language.diagnosticsReceived(
				kind,
				resource,
				this.createMarkerDatas(diagnostics, language.diagnosticSource, resource, kind));
		}
	}

	private configFileDiagnosticsReceived(event: Proto.ConfigFileDiagnosticEvent): void {
		// See https://github.com/Microsoft/TypeScript/issues/10384
		const body = event.body;
		if (!body || !body.diagnostics || !body.configFile) {
			return;
		}

		this.findLanguage(this.client.toResource(body.configFile)).then(language => {
			if (!language) {
				return;
			}

			language.configFileDiagnosticsReceived(this.client.toResource(body.configFile), body.diagnostics.map(tsDiag => {
				const range = tsDiag.start && tsDiag.end ? typeConverters.Range.fromTextSpan(tsDiag) : new vscode.Range(0, 0, 0, 1);
				const diagnostic = new vscode.Diagnostic(range, body.diagnostics[0].text, this.getDiagnosticSeverity(tsDiag));
				diagnostic.source = language.diagnosticSource;
				return diagnostic;
			}));
		});
	}

	/**
	 * Uses cached data from prior calls to `diagnosticsReceived` to update a diagnostic with new text
	 * and then retrigger `diagnosticsRecieved`
	 */
	private async applyReveal(args: IRevealArgs, replacement: Proto.ExpandRevealResponseBody) {
		const fileCache = this._protoDiagnosticCache.get(args.file.toString());
		if (!fileCache) {
			return;
		}
		const diagCache = fileCache.get(args.kind);
		if (!diagCache) {
			return;
		}
		const diagLocation = diagCache.findIndex(d => !!d.annotations && d.annotations.some(a => a.kind === 'reveal' && a.checker === args.checker && a.id === args.id));
		if (diagLocation === -1) {
			return;
		}
		const diag = diagCache[diagLocation];
		// annotations should already be sorted in `start` position order
		const annotationToExpandPos = diag.annotations!.findIndex(a => a.kind === 'reveal' && a.checker === args.checker && a.id === args.id);
		const annotationToExpand = diag.annotations![annotationToExpandPos];
		const newAnnotations = [
			...diag.annotations!.slice(0, annotationToExpandPos),
			...(replacement.annotations || []).map(a => ({ ...a, start: a.start + annotationToExpand.start })),
			...diag.annotations!.slice(annotationToExpandPos + 1).map(a => ({ ...a, start: a.start + (replacement.text.length - annotationToExpand.length) })),
		];
		const newText = diag.text.slice(0, annotationToExpand.start) + replacement.text + diag.text.slice(annotationToExpand.start + annotationToExpand.length);
		const newDiag: Proto.Diagnostic = {
			...diag,
			text: newText,
			annotations: newAnnotations,
		};
		const newDiagnosticList = [
			...diagCache.slice(0, diagLocation),
			newDiag,
			...diagCache.slice(diagLocation + 1)
		];
		await this.diagnosticsReceived(args.kind, args.file, newDiagnosticList);
	}

	private createMarkerDatas(
		diagnostics: Proto.Diagnostic[],
		source: string,
		file: vscode.Uri,
		kind: DiagnosticKind,
	): (vscode.Diagnostic & { reportUnnecessary: any })[] {
		return diagnostics.map(tsDiag => this.tsDiagnosticToVsDiagnostic(tsDiag, source, file, kind));
	}

	private tsDiagnosticToVsDiagnostic(diagnostic: Proto.Diagnostic, source: string, file: vscode.Uri, kind: DiagnosticKind): vscode.Diagnostic & { reportUnnecessary: any } {
		const { start, end, text } = diagnostic;
		const range = new vscode.Range(typeConverters.Position.fromLocation(start), typeConverters.Position.fromLocation(end));
		const converted = new vscode.Diagnostic(range, text, this.getDiagnosticSeverity(diagnostic));
		converted.source = diagnostic.source || source;
		if (diagnostic.code) {
			converted.code = diagnostic.code;
		}
		const relatedInformation = diagnostic.relatedInformation;
		if (relatedInformation) {
			converted.relatedInformation = relatedInformation.map((info: any) => {
				let span = info.span;
				if (!span) {
					return undefined;
				}
				return new vscode.DiagnosticRelatedInformation(typeConverters.Location.fromTextSpan(this.client.toResource(span.file), span), info.message);
			}).filter((x: any) => !!x) as vscode.DiagnosticRelatedInformation[];
		}
		if (diagnostic.reportsUnnecessary) {
			converted.tags = [vscode.DiagnosticTag.Unnecessary];
		}
		(converted as vscode.Diagnostic & { reportUnnecessary: any }).reportUnnecessary = diagnostic.reportsUnnecessary;
		if (diagnostic.annotations) {
			// Allow markdown message content to be passed thru
			converted.richMessage = this.convertAnnotationsIntoMarkdown(text, diagnostic.annotations, file, kind);
		}
		return converted as vscode.Diagnostic & { reportUnnecessary: any };
	}

	private convertAnnotationsIntoMarkdown(text: string, annotations: protocol.DiagnosticAnnotationSpan[], file: vscode.Uri, kind: DiagnosticKind) {
		// TODO: If the server returns overlapping annotation spans, we should handle that in some way
		// rather than assuming they are non-overlapping
		annotations.sort(compareAnnotations);

		let value = '';
		let lastEnd = 0;
		for (const a of annotations) {
			value += text.slice(lastEnd, a.start);
			const original = text.slice(a.start, lastEnd = a.start + a.length);
			if (a.kind === 'symbol') {
				const replacement = `[${original}](command:editor.action.peekDefinition?${encodeURIComponent(JSON.stringify([
					{ $mid: 1, scheme: 'file', authority: '', path: a.file.startsWith('/') ? a.file : `/${a.file}` }, // This is a serialized URL
					{ lineNumber: a.location.line, column: a.location.offset } // This is an IPosition from common/core/position
				]))} "Peek Definition")`; // TODO: Localize alt hover text???
				value += replacement;
			}
			else if (a.kind === 'reveal') {
				const args: IRevealArgs = {
					id: a.id,
					checker: a.checker,
					file,
					kind
				};
				value += `[${original}](command:_typescript.expandReveal?${encodeURIComponent(JSON.stringify(args))} "Expand Span")`;
			}
			else {
				value += original;
			}
		}

		value += text.slice(lastEnd);

		return {
			value,
			isTrusted: true
		};
	}

	private getDiagnosticSeverity(diagnostic: Proto.Diagnostic): vscode.DiagnosticSeverity {
		if (this.reportStyleCheckAsWarnings
			&& this.isStyleCheckDiagnostic(diagnostic.code)
			&& diagnostic.category === PConst.DiagnosticCategory.error
		) {
			return vscode.DiagnosticSeverity.Warning;
		}

		switch (diagnostic.category) {
			case PConst.DiagnosticCategory.error:
				return vscode.DiagnosticSeverity.Error;

			case PConst.DiagnosticCategory.warning:
				return vscode.DiagnosticSeverity.Warning;

			case PConst.DiagnosticCategory.suggestion:
				return vscode.DiagnosticSeverity.Hint;

			default:
				return vscode.DiagnosticSeverity.Error;
		}
	}

	private isStyleCheckDiagnostic(code: number | undefined): boolean {
		return code ? styleCheckDiagnostics.indexOf(code) !== -1 : false;
	}
}

function compareAnnotations(a: protocol.DiagnosticAnnotationSpan, b: protocol.DiagnosticAnnotationSpan) {
	return a.start - b.start;
}
