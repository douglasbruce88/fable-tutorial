## Getting started

cd 1-counter
npm install
node node_modules/fable-compiler -w

Open index.html

## Elm Architecture

type Model
type Event - things that happen e.g. Increment/Decrement

updated : Model -> Event -> Model e.g. 5 -> Increment -> 6
render : (Event -> unit) -> Model -> Html (fully functional repainting of the DOM, this is the clever bit i.e. diff)

## Part 3

Run build.cmd