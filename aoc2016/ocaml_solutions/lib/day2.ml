open Core
open Util.Coord
open Util.Fun

let inp = In_channel.read_lines "../inputs/day02.txt"
          |> List.map ~f:(String.to_list
                          >> List.map ~f:(Char.to_string >> dir2D_of_string))

(* The 'trans' function takes arguments 'x', 'n', and 'aux', where 'a'
   is the currently relevant part of the coordinate, 'n' is what we are
   adding to it, and 'aux' is the other part of the coordinate, for
   possible comparisons. *)
let solve moves start trans =
  let move (x, y) = function
    | L -> (trans x (-1) y, y)
    | R -> (trans x   1  y, y)
    | U -> (x             , trans y   1  x)
    | D -> (x             , trans y (-1) x)
  in List.folding_map moves ~init:start
       ~f:(fun acc xs ->
         let res = List.fold xs ~init:acc ~f:move
         in res, res)

(*
    1 2 3
    4 5 6
    7 8 9
*)
let part1 = solve inp (1, 1)
                (fun a n _ -> let b = a + n in if b >= 0 && b <= 2 then b else a)
            |> List.map ~f:(fun (x, y) -> string_of_int (7 - (y * 3) + x))

(*
       1
     2 3 4
   5 6 7 8 9
     A B C
       D
*)
let part2 =
  let pos_to_hex =
    let r = List.range (-2) 3 in
    let positions =
      List.(rev r >>= fun x ->
            r     >>= fun y ->
            if abs x + abs y <= 2 then [(y, x)] else []) in
    List.(zip_exn positions (map (range 1 14) ~f:(Printf.sprintf "%x")))
  in solve inp (-2, 0) (fun a n aux -> (* Taxi metric *)
         let b = a + n in if abs b + abs aux <= 2 then b else a)
     |> List.(filter_map ~f:(fun (x1, x2) ->
                  Option.map
                    (find pos_to_hex ~f:(fun ((y1, y2), _) -> x1 = y1 && x2 = y2))
                    ~f:snd))

let day2 = String.(concat part1, concat part2)
