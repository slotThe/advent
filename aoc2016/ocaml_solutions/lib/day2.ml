open Core

type dir2D = L | R | U | D

let (<<) f g x = f (g x)

let dir2D_of_string s = match s with
  | 'L' -> L
  | 'R' -> R
  | 'U' -> U
  | 'D' -> D
  | _   -> failwith "rip"

let move trans (x, y) d = match d with
  | L -> (trans x (-1) y, y)
  | R -> (trans x 1 y, y)
  | U -> (x, trans y 1 x)
  | D -> (x, trans y (-1) x)

(* The 'trans' function takes arguments 'x', 'n', and 'aux', where 'a'
   is the currently relevant part of the coordinate, 'n' is what we are
   adding to it, and 'aux' is the other part of the coordinate, for
   possible comparisons. *)
let solve mvs start trans =
  List.fold mvs ~init:[start]
    ~f:(fun acc xs ->
      List.cons (List.fold xs ~init:(List.hd_exn acc) ~f:(move trans)) acc)
  |> (List.tl_exn << List.rev)

let inp = In_channel.read_all "../inputs/day02.txt"
          |> String.split_lines
          |> List.map ~f:(List.map ~f:dir2D_of_string << String.to_list)

let part1 = solve inp (1, 1)
                (fun a n _ -> let b = a + n in if b >= 0 && b <= 2 then b else a)
            |> List.map ~f:(fun (x, y) -> string_of_int (7 - (y * 3) + x))

let part2 =
  let decode2 =
    let r = List.range (-2) 3 in
    let positions =
      List.(rev r >>= fun x ->
            r     >>= fun y ->
            if abs x + abs y <= 2 then [(y, x)] else [])
    in List.(zip_exn positions (map (range 1 14) ~f:(Printf.sprintf "%x")))
  in solve inp (-2, 0) (fun a n aux ->
         let b = a + n
         in if abs b + abs aux <= 2  (* Taxicab metric *)
            then b
            else a)
     |> List.(map ~f:(fun (x1, x2) ->
                  find decode2 ~f:(fun ((y1, y2), _) -> x1 = y1 && x2 = y2)))
     |> (List.map ~f:snd << List.filter_opt)

let day2 = String.(concat part1, concat part2)
