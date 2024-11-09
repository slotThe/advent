open Core
open Angstrom

(* A set comprising tuples of integers as elements. *)
module IISet = Set.Make (struct
  type t = int * int

  let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
  let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
end)

let string_of_option to_str = function
  | None -> "None"
  | Some i -> "Some " ^ to_str i

let sort_string s =
  String.to_list s |> List.sort ~compare:Char.compare |> String.of_char_list

let read_single_line f = In_channel.read_lines f |> List.hd_exn
let sum = List.fold ~init:0 ~f:( + )

let frequencies xs =
  List.(
    sort_and_group xs ~compare:Char.compare
    |> map ~f:(fun xs -> (hd_exn xs, length xs))
    |> sort ~compare:(fun (_, a) (_, b) -> Int.compare b a))

(* [sliding_window n xs] create a sliding window of size [n] for the
   list [xs]. The implementation is naive in the sense that

       sliding_window 3 [1; 2; 3; 4]

   will return [[1; 2; 3]; [2; 3; 4]; [3; 4]; [4]].
*)
let sliding_window n xs =
  let rec go res = function
    | [] -> List.rev res
    | xs -> go (List.take xs n :: res) (List.drop xs 1)
  in
  go [] xs

module List = struct
  include List

  let replicate a n =
    let rec go xs = function 0 -> xs | n -> go (List.cons a xs) (n - 1) in
    go [] n
end

(* Pretty print a Set. *)
let ppSet set =
  let xmin, xmax, ymin, ymax =
    Set.fold set ~init:(0, 0, 0, 0) ~f:(fun (xmn, xmx, ymn, ymx) (x, y) ->
        (min xmn x, max xmx x, min ymn y, max ymx y))
  and r = List.range ~stop:`inclusive in
  List.map (r ymin ymax) ~f:(fun y ->
      List.map (r xmin xmax) ~f:(fun x ->
          if Set.mem set (x, y) then '#' else ' '))
  |> List.map ~f:(fun xs -> String.of_char_list xs)
  |> String.concat ~sep:"\n"

module Parse = struct
  let re_group re s = Re.(Option.map ~f:Group.all (exec_opt (Pcre.regexp re) s))
  let ios = int_of_string
end

module Fun = struct
  let ( << ) f g x = f (g x)
  let ( @. ) f g x = f (g x)
  let ( >> ) f g x = g (f x)
end

module Coord = struct
  type dir1D = L1 | R1

  let dir1D_of_string = function
    | "L" -> L1
    | "R" -> R1
    | _ -> failwith "dir1D_of_string: expected L or R"

  type dir2D = L | R | U | D

  let dir2D_of_string = function
    | "L" -> L
    | "R" -> R
    | "U" -> U
    | "D" -> D
    | _ -> failwith "dir2D_of_string: expected L, R, U, or D."

  let turn d (y, x) = match d with L1 -> (-x, y) | R1 -> (x, -y)
  let scale n (x, y) = (n * x, n * y)
  let coordsum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let walk p ~dir ~amount = coordsum p (scale amount dir)
  let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
end

module Parser = struct
  let eval p str =
    match parse_string ~consume:All p str with
    | Ok v -> v
    | Error msg -> failwith msg

  let num =
    int_of_string
    <$> lift2 ( ^ ) (option "+" (string "-")) (take_while1 Char.is_digit)
end
