open Angstrom
open Core
open Ocaml_solutions.Util

let p_capitals =
  many1
    (satisfy (fun c ->
         let i = Char.to_int c in
         65 <= i && i <= 90))
  >>| List.length

let p_expand expand =
  let* one = char '(' *> Parser.num <* char 'x' in
  let* two = Parser.num <* char ')' in
  take one >>| expand two

let p_all f = many1 (p_capitals <|> p_expand f) >>| sum

let () =
  let inp = read_single_line "../inputs/day09.txt" in
  let one = Parser.eval (p_all (fun n xs -> n * String.length xs)) inp in
  (* God I love recursion *)
  let rec two s = Parser.eval (p_all (fun n xs -> n * two xs)) s in
  print_answer 9 (string_of_int one, string_of_int (two inp))
