.PHONY: all
all: test

.PHONY: setup
setup:
	stack setup
	npm install -g chokidar-cli
	npm install -g forever
	npm install -g browser-sync

# Builds Elm-Websocket
.PHONY: test
test:
	stack test

# Build and run the example. Any changes will result in a reload.
# Also run `make browser-sync` for seamless browser restarts
.PHONY: run-example
run-example: build-example
	(cd example && forever start -c "./elm-websocket-example" elm-websocket-example)
	chokidar "example/server/shared" "example/server/src" "example/client/src" "example/client/assets" -i "example/client/src/Api.elm" -c "make restart-example"

.PHONY: build-example-code-generator
build-example-code-generator:
	stack build --copy-bins --local-bin-path example
	(cd example && ./elm-websocket-code-generator)

.PHONY: create-assets-directory
create-assets-directory:
	mkdir -p example/assets

.PHONY: build-example
build-example: build-example-code-generator create-assets-directory
	(cd example/client && elm-make --yes src/ExampleApp.elm --output ../assets/example-app.js)
	cp -R example/client/assets/* example/assets

.PHONY: start-example
restart-example: build-example
	(cd example && forever stopall)
	(cd example && forever start -c "./elm-websocket-example" elm-websocket-example)
	browser-sync reload

# Start a browser which will reload when the front-end assets change
browser-sync:
	browser-sync start --proxy "localhost:8080" --ws --files "example/assets"
