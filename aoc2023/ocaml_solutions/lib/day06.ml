open Core

let parse1 inp =
  List.map inp ~f:(fun l -> String.split ~on:' ' l
                            |> List.filter_map ~f:int_of_string_opt)
  |> (fun xs -> List.zip_exn (List.nth_exn xs 0) (List.nth_exn xs 1))

let parse2 inp =
  List.map inp ~f:(fun l -> List.drop (String.split ~on:' ' l) 1
                            |> String.concat |> int_of_string)
  |> (fun xs -> (List.nth_exn xs 0, List.nth_exn xs 1))

let part1 (time, distance) =
  List.range 0 time ~stop:`inclusive
  |> List.filter_map ~f:(fun t ->
         let d = t * (time - t)
         in if d > distance then Some d else None)
  |> List.length

let day06 =
  let inp = In_channel.read_lines "../../inputs/day06.txt"
  in List.map ~f:part1 (parse1 inp) |> List.reduce_exn ~f:( * ) |> string_of_int
   , part1 (parse2 inp) |> string_of_int
