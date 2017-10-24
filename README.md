# Elm WebSocket

Generate an Elm Subscriber and JSON encoders/decoders for a Wai WebSocket server.

Elm encode/decode is generated thanks to [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export), with ADT generation thanks to (https://github.com/FPtje).

## Installation

TODO: Hackage

## Usage

## Example application

An example 'Task Management' application is included in this repository under the example directory. It can be built and run using the following command

```
$ make run-example
```

This will build the haskell websockets application, as well as the Elm client, and serve it at [localhost:8080](http://localhost:8080).


## Development

```
$ git clone https://github.com/rhyskeepence/elm-websocket.git
$ cd elm-websocket
$ make test
```
