'use strict'
import * as vscode from 'vscode'
import { Disposable, ExtensionContext, Range, TextEditor, TextEditorDecorationType } from 'vscode'
import * as path from 'path'
import * as http from 'http'
import * as fs from 'fs'

let iconFlag = false
let decorations: TextEditorDecorationType[] = []

export function activate(context: ExtensionContext) {
    render()
    context.subscriptions.push(
        vscode.commands.registerCommand('dreamist.switchIcons', () => {
            iconFlag = !iconFlag
            render()
        }),
        vscode.commands.registerCommand('dreamist.downloadIcons', downloadIcons),
        vscode.commands.registerCommand('dreamist.rubyNovel', insertRuby),
        vscode.commands.registerCommand('dreamist.publishNovel', publish),
        vscode.window.onDidChangeActiveTextEditor(editor => render()),
        vscode.workspace.onDidChangeTextDocument(event => render())
    )
}

function render() {
    for (const decoration of decorations) {
        decoration.dispose()
    }
    decorations = []

    for (const editor of vscode.window.visibleTextEditors) {
        decorateRuby(editor)
        decorateIcons(editor)
        decorateInvalidTitle(editor)
    }

    displayWordCount()
}

function decorateRuby(editor: TextEditor) {
    let ranges: Range[] = []
    let match: RegExpExecArray
    const rubyRegex = /｜(.+?)(《.+?》)/g
    while (match = rubyRegex.exec(editor.document.getText())) {
        const pos = editor.document.positionAt(match.index)
        ranges.push(
            new Range(pos.line, pos.character, pos.line, pos.character),
            new Range(
                pos.line, pos.character + 1 + match[1].length,
                pos.line, pos.character + 1 + match[1].length + match[2].length))
    }

    decorations.push(vscode.window.createTextEditorDecorationType({
        'color': '#888888'
    }))
    editor.setDecorations(
        decorations[decorations.length - 1],
        ranges)
}

function decorateIcons(editor: TextEditor) {
    let match: RegExpExecArray
    let iconLineDict: { [key: string]: number[] } = {}
    let iconLines: number[] = []
    const iconRegex = /^(\d\d)[「『]/gm
    while (match = iconRegex.exec(editor.document.getText())) {
        const icon = match[1]
        if (iconLineDict[icon] == null) iconLineDict[icon] = []
        iconLineDict[icon].push(editor.document.positionAt(match.index).line)
        iconLines.push(editor.document.positionAt(match.index).line)
    }

    decorations.push(vscode.window.createTextEditorDecorationType({
        'color': '#888888'
    }))
    editor.setDecorations(
        decorations[decorations.length - 1],
        iconLines.map(line => new vscode.Range(line, 0, line, 2)))

    for (const key in iconLineDict) {
        if (iconFlag) {
            decorations.push(vscode.window.createTextEditorDecorationType({
                'gutterIconPath': iconPath(key),
                'gutterIconSize': 'contain'
            }))
            editor.setDecorations(
                decorations[decorations.length - 1],
                iconLineDict[key].map(line => new vscode.Range(line, 0, line, 0)))
        }
    }
}

function decorateInvalidTitle(editor: TextEditor) {
    let titleSpeech: [string, number]
    let lineNumbers: number[] = []
    for (let i = 0; i < editor.document.lineCount; i++) {
        const line = editor.document.lineAt(i)
        if (line.text.length > 0) {
            if (line.text[0] == '#') {
                if (titleSpeech != null) {
                    lineNumbers.push(titleSpeech[1])
                }

                const match = /[「『](.+)[」』]/.exec(line.text)
                if (match != null) {
                    titleSpeech = [match[1], line.lineNumber]
                } else {
                    titleSpeech = null
                }
            } else {
                if (titleSpeech != null && line.text.includes(titleSpeech[0])) {
                    titleSpeech = null
                }
            }
        }
    }
    if (titleSpeech != null) {
        lineNumbers.push(titleSpeech[1])
    }

    decorations.push(vscode.window.createTextEditorDecorationType({
        'backgroundColor': '#FF8888'
    }))
    editor.setDecorations(
        decorations[decorations.length - 1],
        lineNumbers.map(line => new vscode.Range(line, 0, line + 1, 0)))
}

function displayWordCount() {
    const editor = vscode.window.activeTextEditor
    if (editor != null) {
        const document = editor.document
        let count = 0
        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i).text
            if (line.length > 0 && line[0] != '#') {
                for (let j = 0; j < line.length; j++) {
                    if (line.charCodeAt(j) > 127) count++
                }
            }
        }
        vscode.window.setStatusBarMessage(count.toString() + "文字")
    }
}

function downloadIcons() {
    const iconDict: { [key: string]: string } = JSON.parse(
        fs.readFileSync(path.join(vscode.workspace.rootPath, '.vscode', 'icon.json'), 'utf8'))

    for (const key in iconDict) {
        http.get(
            'http://placehold.jp/96/ffffff/008800/128x128.png?text=' + encodeURI(iconDict[key]),
            (response) => response.pipe(fs.createWriteStream(iconPath(key))))
    }
}

function iconPath(name: string): string {
    return path.join(vscode.workspace.rootPath, '.vscode', 'icon', name + '.png')
}

function insertRuby() {
    const rubyDict: { [key: string]: string } = JSON.parse(
        fs.readFileSync(path.join(vscode.workspace.rootPath, '.vscode', 'ruby.json'), 'utf8'))

    const editor = vscode.window.activeTextEditor
    editor.edit((edit) => {
        let text = editor.document.getText().replace(/｜(.+?)《.+?》/g, '$1')
        let sections: string[] = []
        for (let section of text.split('#')) {
            for (const key in rubyDict) {
                section = section.replace(new RegExp(key), '｜' + key + '《' + rubyDict[key] + '》')
            }
            sections.push(section)
        }
        edit.replace(new Range(0, 0, editor.document.lineCount + 1, 0), sections.join("#"))
    })
}

function publish() {
    const editor = vscode.window.activeTextEditor
    editor.edit((edit) => {
        edit.replace(new Range(0, 0, editor.document.lineCount + 1, 0), editor.document.getText().
            replace(/^\d\d([「『])/gm, '$1').
            replace(/([」』])$/gm, '$1\r\n').
            replace(/^---$/gm, '　～～～'))
    })
}
