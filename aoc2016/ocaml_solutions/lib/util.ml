open Core

let (--) s t = List.init (t - s + 1) ~f:(fun i -> i + s)

let string_of_option to_str o = match o with
  | None   -> "None"
  | Some i -> "Some " ^ to_str i

let sort_string s =
  String.to_list s |> List.sort ~compare:Char.compare |> String.of_char_list

module Fun = struct
  let (<<) f g x = f (g x)
  let (@.) f g x = f (g x)

  let (>>) f g x = g (f x)
end

module Coord = struct
  type dir1D = L1 | R1

  let dir1D_of_string s = match s with
    | "L" -> L1
    | "R" -> R1
    | _   -> failwith "dir1D_of_string: expected L or R"

  type dir2D = L | R | U | D

  let dir2D_of_string s = match s with
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
