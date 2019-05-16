/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import * as vscode from 'vscode';
import { Command } from '../utils/commandManager';
import { Lazy } from '../utils/lazy';
import TypeScriptServiceClientHost from '../typeScriptServiceClientHost';
import { CommandTypes } from 'typescript/lib/protocol';
import { nulToken } from '../utils/cancellation';
import { DiagnosticKind } from '../features/diagnostics';

export interface IRevealArgs {
	id: number;
	checker: number;
	file: vscode.Uri;
	kind: DiagnosticKind;
}

export interface IApplyRevealCallback {
	(args: IRevealArgs, replacement: protocol.ExpandRevealResponseBody): Promise<void>;
}

export class TypeScriptExpandRevealCommand implements Command {
	public readonly id = '_typescript.expandReveal';

	public constructor(
		private readonly lazyClientHost: Lazy<TypeScriptServiceClientHost>,
	) { }

	public async execute(args: IRevealArgs) {
		const response = await this.lazyClientHost.value.serviceClient.execute(CommandTypes.ExpandReveal, { id: args.id, checker: args.checker }, nulToken);
		if (!response || response.type !== 'response') {
			return;
		}
		if (!response.success) {
			return;
		}
		const body = response.body as protocol.ExpandRevealResponseBody;
		await this.lazyClientHost.value.serviceClient.applyReveal(args, body);
	}
}
