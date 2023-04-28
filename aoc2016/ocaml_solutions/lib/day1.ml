open Core

(* General utility *)

let (--) s t = List.init (t - s + 1) ~f:(fun i -> i + s)

let string_of_option to_str o = match o with
  | None   -> "None"
  | Some i -> String.concat ~sep:" " ["Some"; to_str i]

(* Coordinates *)

module Coord = struct
  type dir1D = L | R

  let turn d (y, x) = match d with
    | L -> (-x,  y)
    | R -> ( x, -y)

  let scale n (x, y) = (n * x, n * y)

  let coordsum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let walk p ~dir ~amount = coordsum p (scale amount dir)

  let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
end

(* Wow *)

module IISet =
  Set.Make(
      struct
        type t = int * int
        let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
        let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
        let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
      end
    )

(* Parsing *)

let p_input s =
  let pref = String.sub s ~pos:0 ~len:1
  and num  = String.sub s ~pos:1 ~len:(String.length s - 1)
  in match pref with
     | "L" -> (Coord.L, int_of_string num)
     | "R" -> (Coord.R, int_of_string num)
     | _   -> failwith "rip"

(* Solving *)

let completely_walk das =
  List.fold das ~init:((0, 0), (0, 1), [])
    ~f:(fun (oldpos, olddir, ps) (d, amount) ->
      let dir = Coord.turn d olddir in
      let pos = Coord.walk oldpos ~dir ~amount
      in pos
       , dir
       , List.append ps (List.map ~f:(fun x -> Coord.walk oldpos ~dir ~amount:x)
                                  (1 -- amount)))

let find_dup xs =
  let rec go s ys = match ys with
    | []       -> None
    | y :: yss -> if   IISet.mem s y
                  then Some y
                  else go (IISet.add s y) yss
  in go IISet.empty xs

open Option (* for >>= below *)

let day1 =
  let inp = In_channel.read_all "../inputs/day01.txt"
            |> String.tr ~target:'\n' ~replacement:' ' in
  let dirs = String.split_on_chars inp ~on:[','; ' ']
             |> List.filter ~f:(fun x -> not (String.is_empty x))
             |> List.map ~f:p_input in
  let pos, _, ps = completely_walk dirs in
  let one = Coord.manhattan (0, 0) pos
  and two = find_dup ps >>= fun x -> Some (Coord.manhattan (0, 0) x)
  in string_of_int one
   , string_of_option string_of_int two
