open! Base

type t =
  { height : int
  ; width : int
  }
[@@deriving sexp, fields]

let create ~height ~width = { height; width }
let create_unlabeled height width = { height; width }

(* Exercise 03a:

   This module is a simple record that represent the parameters of the board. It is a
   record with two fields, height and width.

   Our goal is to write the [in_bounds] function which given a board, and a position, will
   return true or false based on if the Position is in the Board.

   Position is a record that contains two fields.  Take a look a the signature in
   position.mli to see that type.

   Because we use 0 indexing, the bottom left corner of the board is (0,0) and the top
   right is (width-1,height-1).

   Some hints:
   - >, >=, <, <= are the int comparison operators
   - && is how to represent boolean "and"
   - the following is a boolean statement whose value is true:
   {[
     3 < 5
   ]}

   Once the tests for 03a are passing, let's go to game.ml to finish up collision
   detection. *)
let in_bounds t { Position.row; col } = ( row < t.height && row >= 0 ) && ( col < t.width && col >= 0 )

let all_locations t =
  List.concat_map ( List.range 0 t.height ) ~f:(fun row ->
      List.map ( List.range 0 t.width ) ~f:( fun col -> { Position.row; col } ) )
;;

module Exercises = struct
  let exercise03a = in_bounds
end
