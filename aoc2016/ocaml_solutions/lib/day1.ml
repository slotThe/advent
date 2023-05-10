open Core
open Util

(* Parsing *)

let p_input s = Coord.dir1D_of_string (String.sub s ~pos:0 ~len:1)
              , int_of_string (String.(sub s ~pos:1 ~len:(length s - 1)))

(* Solving *)

let completely_walk das =
  List.fold das ~init:((0, 0), (0, 1), [])
    ~f:(fun (oldpos, olddir, ps) (d, amount) ->
      let dir = Coord.turn d olddir in
      let pos = Coord.walk oldpos ~dir ~amount
      in pos
       , dir
       , List.append ps (List.map ~f:(fun amount -> Coord.walk oldpos ~dir ~amount)
                                  (List.range ~stop:`inclusive 1 amount)))

let find_dup xs =
  let rec go s = function
    | []       -> None
    | y :: yss -> if   IISet.mem s y
                  then Some y
                  else go (IISet.add s y) yss
  in go IISet.empty xs

let day1 =
  let inp = read_single_line "../../inputs/day01.txt" in
  let dirs = String.split_on_chars inp ~on:[','; ' ']
             |> List.filter_map ~f:(fun x ->
                    if not (String.is_empty x) then Some (p_input x) else None) in
  let pos, _, ps = completely_walk dirs in
  let one = Coord.manhattan (0, 0) pos
  and two = Option.(find_dup ps >>= fun x -> Some (Coord.manhattan (0, 0) x))
  in string_of_int one
   , string_of_option string_of_int two
