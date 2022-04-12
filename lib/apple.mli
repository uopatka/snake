open! Base

module Color : sig
  type t = Red
end

type t [@@deriving sexp_of]

(** [create] takes in a [Board.t] representing the area in which an apple can be
    generated, as well as a [Snake.t], and creates an [Apple.t] such that it appears in
    the board's play area and does not overlap with the snake.

    [create] returns [None] if there are no valid positions for the apple. *)
val create : board:Board.t -> snake:Snake.t -> t option

(** [location] returns the location of the apple on the board. *)
val location : t -> Position.t

(** [color] returns the color of the apple *)
val color : t -> Color.t

(** [amount_to_grow] returns the number of squares the snake should grow by due to eating
    this apple *)
val amount_to_grow : t -> int

module Exercises : sig
  val exercise05 : board:Board.t -> snake:Snake.t -> t option
  val create_with_location : Position.t -> t
end
