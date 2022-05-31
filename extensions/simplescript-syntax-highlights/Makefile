.PHONY: build
build:
	npx js-yaml syntaxes/simplescript.tmLanguage.yaml > syntaxes/simplescript_unsubbed.tmLanguage.json

.PHONY: substitute
substitute: build
	substituter.rb syntaxes/simplescript_unsubbed.tmLanguage.json syntaxes/simplescript.tmLanguage.json

.PHONY: package
package: substitute
	vsce package

.PHONY: install
install: substitute
	code --install-extension simplescript-syntax-highlights-0.0.1.vsix
