open Core

let ( mod ) x y = ((x mod y) + y) mod y

let inp =
  In_channel.read_lines "../../inputs/day01.txt"
  |> List.map ~f:(fun x ->
    let y = int_of_string (String.drop_prefix x 1) in
    if String.is_prefix x ~prefix:"L" then -y else y)
;;

let one xs =
  List.folding_map
    xs
    ~f:(fun x y ->
      let r = (x + y) mod 100 in
      r, r)
    ~init:50
  |> List.filter ~f:(fun x -> phys_equal x 0)
  |> List.length
;;

let sgn n =
  match Int.sign n with
  | Neg -> -1
  | Zero -> 0
  | Pos -> 1
;;

let two (z, d) n =
  let d' = sgn n * d mod 100 in
  z + ((d' + abs n) / 100), (d + n) mod 100
;;

let () =
  let o = one inp in
  assert (phys_equal 1011 o);
  print_endline (string_of_int o);
  let t = List.fold ~init:(0, 50) ~f:two inp |> fst in
  assert (phys_equal 5937 t);
  print_endline (string_of_int t)
;;
