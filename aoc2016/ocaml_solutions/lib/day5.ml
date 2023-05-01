open Core

let inp = Util.read_single_line "../../inputs/day05.txt"

let generate_hashes s =
  Seq.filter_map
    (fun i ->
      let hash = Md5.to_hex @@ Md5.digest_string @@ s ^ string_of_int i
      in if String.is_prefix hash ~prefix:"00000"
         then Some hash
         else None)
    (Seq.map succ (Seq.ints 0))

let test_hash n inp check = Seq.take n (Seq.filter check (generate_hashes inp))

let part1 =
  Seq.fold_left (fun a h -> a ^ Char.to_string h.[5]) "" (test_hash 8 inp (const true))

let part2 =
  Seq.fold_left (fun a b -> b :: a) []
    (test_hash 20 inp (fun h -> String.contains "01234567" h.[5]))
  |> List.dedup_and_sort ~compare:(fun h h' -> Char.compare h.[5] h'.[5])
  |> List.fold_left ~init:"" ~f:(fun s h -> s ^ Char.to_string h.[6])

let day5 = (part1, part2)
