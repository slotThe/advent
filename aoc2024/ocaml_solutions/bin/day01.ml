open Core
open List

let a, b =
  In_channel.read_lines "../inputs/day01.txt"
  |> map ~f:(fun l -> String.split ~on:' ' l |> filter_map ~f:int_of_string_opt)
  |> transpose_exn
  |> map ~f:(sort ~compare:compare_int)
  |> fun xs -> (nth_exn xs 0, nth_exn xs 1)

module IMap = Map.Make (Int)

let () =
  let one = map2_exn a b ~f:(fun x y -> abs (x - y))
  and two =
    let bm =
      IMap.of_list_with_key_fold b ~get_key:Fn.id ~init:1 ~f:(fun a _ -> a + 1)
    in
    map a ~f:(fun x -> x * Option.value ~default:0 (Map.find bm x))
  in
  map ~f:(fun x -> reduce_exn ~f:( + ) x |> string_of_int) [ one; two ]
  |> Out_channel.output_lines stdout
