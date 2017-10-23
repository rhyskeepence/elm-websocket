.PHONY: all
all: test

.PHONY: test
test:
	stack test

.PHONY: run-example
run-example:
	stack build --copy-bins --local-bin-path example
	(cd example && ./elm-websocket-code-generator)
	mkdir -p example/assets
	(cd example/client && elm-make --yes src/ExampleApp.elm --output ../assets/example-app.js)
	cp -R example/client/assets/* example/assets
	(cd example && ./elm-websocket-example)
