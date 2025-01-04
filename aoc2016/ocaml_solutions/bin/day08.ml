open Core
open Ocaml_solutions.Util
open Ocaml_solutions.Util.Parse

(* Parsing *)

let p_rect s =
  Option.map
    ~f:(fun gs -> (ios gs.(1), ios gs.(2)))
    (re_group "rect ([0-9]+)x([0-9]+)" s)

type rotate = Column of int * int | Row of int * int

let p_rotate s =
  Option.map (re_group "rotate (?:column x=|row y=)([0-9]+) by ([0-9]+)" s)
    ~f:(fun gs ->
      if String.is_substring gs.(0) ~substring:"column" then
        Column (ios gs.(1), ios gs.(2))
      else Row (ios gs.(1), ios gs.(2)))

type instruction = Rot of rotate | Rect of (int * int)

let p_line s =
  match p_rect s with
  | Some (x, y) -> Rect (x, y)
  | None -> (
      match p_rotate s with
      | Some r -> Rot r
      | None -> failwith "no parser found")

(* Solving *)

let rect (x, y) =
  IISet.of_list
  @@ List.(
       range 0 x >>= fun a ->
       range 0 y >>= fun b -> [ (a, b) ])

let rot thing points =
  match thing with
  | Column (x, i) ->
      IISet.map points ~f:(fun (x', y) ->
          (x', if x' = x then (y + i) mod 6 else y))
  | Row (y, i) ->
      IISet.map points ~f:(fun (x, y') ->
          ((if y' = y then (x + i) mod 50 else x), y'))

let executeInstructions ins =
  let go ps = function Rot r -> rot r ps | Rect r -> Set.union (rect r) ps in
  List.fold ins ~init:IISet.empty ~f:go

(* IO stuff *)

let () =
  let res =
    List.map (In_channel.read_lines "../inputs/day08.txt") ~f:p_line
    |> executeInstructions
  in
  print_answer 8 (string_of_int (Set.length res), "\n" ^ ppSet res)
