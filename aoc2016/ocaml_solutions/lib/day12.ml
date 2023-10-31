open Angstrom
open Core
open Util

type addr = R of char | I of int

type ins = Cpy of addr * char
         | Inc of char
         | Dec of char
         | Jnz of addr * int

let p_ins =
  let p_addr = (Parser.num >>| fun n -> I n) <|> (any_char >>| fun c -> R c) in
  let cpy = let* a = string "cpy " *> p_addr <* string " "
            and+ b = any_char
            in return (Cpy (a, b))
  and inc = string "inc " *> (any_char >>| fun c -> Inc c)
  and dec = string "dec " *> (any_char >>| fun c -> Dec c)
  and jnz = let* a = string "jnz " *> p_addr <* string " "
            and+ b = Parser.num
            in return (Jnz (a, b))
  in cpy <|> inc <|> dec <|> jnz

let execute hm ix xs =
  let rec go (hm, ix) =
    let get c = Map.find_exn hm c in
    let extract = function
      | R reg -> get reg
      | I num -> num
    and put key data = Map.set hm ~key ~data
    in if ix >= Array.length xs
       then hm
       else go @@ match xs.(ix) with
                  | Cpy (x, y) -> (put y (extract x), ix + 1)
                  | Inc x      -> (put x (get x + 1), ix + 1)
                  | Dec x      -> (put x (get x - 1), ix + 1)
                  | Jnz (x, y) ->
                     (hm, if phys_equal (extract x) 0 then ix + 1 else ix + y)
  in go (hm, ix)

let day12 =
  let solve start_c =
    let hm = In_channel.read_lines "../../inputs/day12.txt"
             |> List.map ~f:(Parser.eval p_ins)
             |> Array.of_list
             |> execute
                  (Map.of_alist_exn (module Char)
                     [('a', 0); ('b', 0); ('c', start_c); ('d', 0)])
                  0
    in Map.find_exn hm 'a'
  in string_of_int (solve 0)
   , string_of_int (solve 1)
