open Core
open Util
open Util.Parse

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let run_down start ends lrs tree =
  let l = List.length lrs in
  let rec go ix i node =
    if Set.exists ~f:(String.equal node) ends
    then i
    else let ix' = (ix + 1) mod l
         and (l, r) = Map.find_exn tree node in
         go ix' (i + 1) @@ match List.nth_exn lrs ix with | 'L' -> l | _ -> r
  in go 0 0 start

let mk_tree rest =
  let p_node s = re_group "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)" s in
  List.map rest
    ~f:(fun s -> match p_node s with
                 | Some [|_; a; b; c|] -> (a, (b, c))
                 | _ -> raise (invalid_arg "mk_tree"))
  |> SMap.of_alist_exn

let mk_start_end tree =
  let nodes = Map.keys tree in
  let starts = List.filter ~f:(String.is_suffix ~suffix:"A") nodes in
  let ends = SSet.of_list @@ List.filter ~f:(String.is_suffix ~suffix:"Z") nodes in
  starts, ends

let day08 =
  let (inp, tree) =
    match In_channel.read_lines "../../inputs/day08.txt" with
    | (inp :: _ :: rest)  -> String.to_list inp, mk_tree rest
    | _ -> raise (invalid_arg "day08") in
  let starts, ends = mk_start_end tree
  in soi @@ run_down "AAA" (SSet.of_list ["ZZZ"]) inp tree
   , soi @@ List.(reduce_exn ~f:lcm (map ~f:(fun s -> run_down s ends inp tree) starts))
