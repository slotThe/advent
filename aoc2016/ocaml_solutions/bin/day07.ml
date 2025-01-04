open Core
open Ocaml_solutions.Util.Fun
open Ocaml_solutions.Util

(* There *must* be the option to derive stuff like this… right? *)
module SSet = Set.Make (struct
  type t = char list

  let compare = List.compare Char.compare
  let t_of_sexp = List.t_of_sexp Char.t_of_sexp
  let sexp_of_t = List.sexp_of_t Char.sexp_of_t
end)

let p_line s =
  let parts = String.split_on_chars s ~on:[ '['; ']' ] in
  List.partition_map
    (List.zip_exn parts (List.range 0 (List.length parts)))
    ~f:(fun (a, i) ->
      let a' = String.to_list a in
      if i mod 2 = 0 then First a' else Second a')

let find_abba xs =
  List.exists (sliding_window 4 xs) ~f:(function
    | [ a; b; c; d ] -> Char.(a = d && b = c && a <> b)
    | _ -> false)

let part1 (supernet, hypernet) =
  List.exists supernet ~f:find_abba && not (List.exists hypernet ~f:find_abba)

let collect_bab xs =
  List.filter_map (sliding_window 3 xs) ~f:(function
    | [ a; b; c ] when Char.(a = c && a <> b) -> Some [ b; a; b ]
    | _ -> None)

let match_bab bab's xs = List.exists (sliding_window 3 xs) ~f:(Set.mem bab's)

let part2 (supernet, hypernet) =
  let babs = SSet.of_list @@ List.concat_map supernet ~f:collect_bab in
  List.filter ~f:(match_bab babs) hypernet |> not @. List.is_empty

let () =
  let inp = List.map ~f:p_line (In_channel.read_lines "../inputs/day07.txt") in
  print_answer 7
    ( string_of_int (List.count inp ~f:part1),
      string_of_int (List.count inp ~f:part2) )
