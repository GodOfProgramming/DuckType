# simplescript-syntax-highlights README

## Features

Syntax highlights for SimpleScript

https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide

# Install vsce as a global dependency
$ npm install -g vsce

# Install js-yaml as a development only dependency in your extension
$ npm install js-yaml --save-dev

# Use the command-line tool to convert the yaml grammar to json
$ npx js-yaml syntaxes/abc.tmLanguage.yaml > syntaxes/abc.tmLanguage.json

$ code --install-extension /path/to/vsix

## Known Issues

It's beta, it may be weird

## Release Notes

Initial release of SimpleScript syntax highlighting

### 0.0.1

Have basic syntax, most things work

