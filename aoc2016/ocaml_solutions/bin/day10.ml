open Angstrom
open Core
open Ocaml_solutions.Util

(* Types *)

type getter = Bot of int | Output of int [@@deriving sexp]

let is_bot = function Bot _ -> true | _ -> false

type instruction = Val of int * getter | Give of int * getter * getter

module GetterMap = Map.Make (struct
  type t = getter

  let compare x y =
    match (x, y) with
    | Bot n, Bot m -> Int.compare n m
    | Bot _, Output _ -> -1
    | Output _, Bot _ -> 1
    | Output n, Output m -> Int.compare n m

  let t_of_sexp = getter_of_sexp
  let sexp_of_t = sexp_of_getter
end)

(* Parsing *)

let p_out =
  string "output " *> Parser.num
  >>| (fun n -> Output n)
  <|> (string "bot " *> Parser.num >>| fun n -> Bot n)

let p_value =
  let* value = string "value " *> Parser.num in
  let* bot = string " goes to " *> p_out in
  return (Val (value, bot))

let p_ins =
  let* bot = string "bot " *> Parser.num in
  let* out1 = string " gives low to " *> p_out in
  let* out2 = string " and high to " *> p_out in
  return (Give (bot, out1, out2))

(* Solving *)

let cons_if_present el = function None -> [ el ] | Some xs -> List.cons el xs

let do_instruction (bots, delayed) = function
  | Val (data, key) -> (Map.update bots key ~f:(cons_if_present data), delayed)
  | Give (b, lo, hi) -> (bots, Map.add_exn delayed ~key:(Bot b) ~data:(lo, hi))

let rec solve delayed bs =
  let go bots (k, vs) =
    if Int.equal 2 (List.length vs) then
      let a, b = Map.find_exn delayed k in
      Map.update
        (Map.update (Map.remove bots k) a ~f:(cons_if_present (List.hd_exn vs)))
        b
        ~f:(cons_if_present (List.last_exn vs))
    else bots
  in
  let bots = Map.filteri bs ~f:(fun ~key ~data:_ -> is_bot key) in
  if 0 = Map.length bots then bs
  else
    solve delayed
      (Map.fold bots ~init:bs ~f:(fun ~key ~data m ->
           go m (key, List.sort ~compare:compare_int data)))

(* Main *)

let () =
  let bs, ds =
    In_channel.read_lines "../inputs/day10.txt"
    |> List.map ~f:(fun l -> Parser.eval (p_value <|> p_ins) l)
    |> List.fold ~f:do_instruction ~init:(GetterMap.empty, GetterMap.empty)
  in
  let solved = solve ds bs in
  let things =
    Map.filteri solved ~f:(fun ~key ~data:_ ->
        match key with Output 0 | Output 1 | Output 2 -> true | _ -> false)
  in
  print_answer 10
    ( "141",
      string_of_int
      @@ Map.fold things ~init:1 ~f:(fun ~key:_ ~data acc ->
             acc * List.hd_exn data) )
