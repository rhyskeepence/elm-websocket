.PHONY: all
all: test

.PHONY: test
test:
	stack test

.PHONY: run-example
run-example:
	mkdir -p example/assets
	(cd example/client && elm-make --yes src/ExampleSparklines.elm --output ../assets/sparklines-app.js)
	cp -R example/client/assets/* example/assets
	stack build --copy-bins --local-bin-path example
	(cd example && ./elm-websocket-example)
