.PHONY: setup
setup:
	npm install -g js-yaml vsce

.PHONY: build
build:
	npx js-yaml syntaxes/DuckType.tmLanguage.yaml > syntaxes/DuckType.unsubbed.tmLanguage.json

.PHONY: substitute
substitute: build
	substituter.rb syntaxes/DuckType.unsubbed.tmLanguage.json syntaxes/DuckType.tmLanguage.json

.PHONY: package
package: substitute
	vsce package

.PHONY: install
install: package
	code --install-extension ducktype-syntax-highlights-0.42.0.vsix
