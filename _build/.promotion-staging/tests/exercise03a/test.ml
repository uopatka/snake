open! Base
open! Snake_lib

let%expect_test "Exercise 03a" =
  let board = Board.create ~height:10 ~width:15 in
  let test (row, col) expectation =
    let position = { Position.row; col } in
    let in_bounds = Board.in_bounds board position in
    if Bool.( <> ) in_bounds expectation
    then
      Stdio.printf
        !"in_bounds for %{sexp:Board.t} %{sexp:Position.t} returned %b unexpectedly"
        board
        position
        in_bounds
  in
  test (0, 0) true;
  [%expect {| |}];
  test (-1, 0) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col 0) (row -1)) returned true unexpectedly |}];
  test (0, -1) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col -1) (row 0)) returned true unexpectedly |}];
  test (0, 14) true;
  [%expect {| |}];
  test (0, 15) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col 15) (row 0)) returned true unexpectedly |}];
  test (9, 0) true;
  [%expect {| |}];
  test (10, 0) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col 0) (row 10)) returned true unexpectedly |}];
  test (9, 14) true;
  [%expect {| |}];
  test (9, 15) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col 15) (row 9)) returned true unexpectedly |}];
  test (10, 14) false;
  [%expect {| in_bounds for ((height 10) (width 15)) ((col 14) (row 10)) returned true unexpectedly |}]
;;
