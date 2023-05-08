open Core

let string_of_option to_str = function
  | None   -> "None"
  | Some i -> "Some " ^ to_str i

let sort_string s =
  String.to_list s |> List.sort ~compare:Char.compare |> String.of_char_list

let read_single_line f = In_channel.read_lines f |> List.hd_exn

let frequencies xs =
  List.(sort_and_group xs ~compare:Char.compare
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
  in go [] xs

module Fun = struct
  let (<<) f g x = f (g x)
  let (@.) f g x = f (g x)

  let (>>) f g x = g (f x)
end

module Coord = struct
  type dir1D = L1 | R1

  let dir1D_of_string = function
    | "L" -> L1
    | "R" -> R1
    | _   -> failwith "dir1D_of_string: expected L or R"

  type dir2D = L | R | U | D

  let dir2D_of_string = function
    | "L" -> L
    | "R" -> R
    | "U" -> U
    | "D" -> D
    | _   -> failwith "dir2D_of_string: expected L, R, U, or D."

  let turn d (y, x) = match d with
    | L1 -> (-x,  y)
    | R1 -> ( x, -y)

  let scale n (x, y) = (n * x, n * y)

  let coordsum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let walk p ~dir ~amount = coordsum p (scale amount dir)

  let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
end
