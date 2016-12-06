#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-virtualdom/Fable.Helpers.Virtualdom.fs"
#load "elmish.fsx"
open Fable.Core
open Fable.Import
open Fable.Import.Browser
open Fable.Helpers
open Elmish

// ------------------------------------------------------------------------------------------------
// Implementing TODO list app with Fable
// ------------------------------------------------------------------------------------------------

// TODO #1: The sample code keeps track of the current text in the input
// box - understand how this works! Our model is just `string` and every
// time the input changes, we trigger `Input` event with a new value of the
// input (which we get from the element). This way, the current state is
// always the value in the textbox. We will need this later when adding new
// todo items to the list.

// TODO #2: Add support for adding items & rendering them:
//  * Modify the model so that it contains the current input (string)
//    together with a list of work items (list of strings) and update
//    the `initial` value and the `render` function to render `<li>` nodes
//  * Add a new event (`Create`) and a click handler for the button that
//    triggers it. Modify the `update` function to add the new item.

// TODO #3: Add support for removing items once they are done. To do this,
// we will need to track a unique ID to identify items. Add `NextId` to
// your `Model` record & increment it each time you add an item. Change the
// list of items to be of type `int * string` with ID and text and when
// the `X` button is clicked, trigger a new event `Remove(id)`. Then you
// just need to change the `update` function to remove the item from the list!

// BONUS: If you have more time, you can add support for marking items
// as completed - every item can be complete or incomplete and clicking 
// on an item should swich the stats (use "class" => "done" to strike-through
// completed items). You can also add a view option for showing only 
// complete/incomplete items.

// ------------------------------------------------------------------------------------------------
// Domain model - update events and application state
// ------------------------------------------------------------------------------------------------

type Update =
  | Input of string
  | Create
  | Remove of int
  | ToggleDone of int
  | ToggleShowCompleted

type Model =
  { Input : string
    Items : (int * string * bool) list
    NextID : int
    ShowCompleted : bool }

// ------------------------------------------------------------------------------------------------
// Given an old state and update event, produce a new state
// ------------------------------------------------------------------------------------------------
let removeEltByID id = List.filter (fun (a, _, _) -> a <> id)
let toggleEltByID id = List.map (fun (a, b, c) -> if a = id then (a, b, not c) else (a, b, c))

let update state = function
  | Input s -> { state with Input = s }
  | Create -> {Items = if state.Input = "" then state.Items else (state.NextID, state.Input, false) :: state.Items
               Input = ""
               NextID = state.NextID + 1
               ShowCompleted = state.ShowCompleted }
  | Remove i -> {Items = removeEltByID i state.Items
                 Input = ""
                 NextID = state.NextID
                 ShowCompleted = state.ShowCompleted } 
  | ToggleDone id -> {Items = toggleEltByID id state.Items
                      Input = ""
                      NextID = state.NextID
                      ShowCompleted = state.ShowCompleted } 
  | ToggleShowCompleted -> { state with ShowCompleted = not state.ShowCompleted }
 
// ------------------------------------------------------------------------------------------------
// Render page based on the current state
// ------------------------------------------------------------------------------------------------

let doneClass = function
  | true ->  "class" => "done"
  | false -> "class" => ""

let render trigger state =
  let listElement (idx, item, isDone)  = 
    h?li [doneClass isDone
          "onclick" =!> fun _ -> trigger (ToggleDone idx)] 
         [text item
          h?a ["href" => "#"; "onclick" =!> fun _ -> trigger (Remove idx)] 
              [ h?span [] [ text "X" ] ] ]
  let lists = 
      state.Items 
      |> List.rev 
      |> List.map (fun (idx, item, isDone) -> if (isDone && not state.ShowCompleted ) then h?div[][] else listElement (idx, item, isDone) )

  h?div [] [
    h?ul [] lists
    h?input [
      "value" => state.Input
      "oninput" =!> fun d -> trigger (Input(unbox d?target?value)) ] []
    h?button
      [ yield "onclick" =!> fun _ -> trigger Create ]
      [ text "Add" ]
    h?button
      [ yield "onclick" =!> fun _ -> trigger ToggleShowCompleted ]
      [ text "Toggle" ]
  ]

// ------------------------------------------------------------------------------------------------
// Start the application with initial state
// ------------------------------------------------------------------------------------------------

let initial = { Input = ""
                Items = []
                NextID = 0
                ShowCompleted = true }

app "todo" initial render update
