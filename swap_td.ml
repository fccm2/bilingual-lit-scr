module X = Xmlerr

let () =
  let x = X.parse_file ~filename:Sys.argv.(1) in
  let rec get_td acc = function
  | ((X.ETag "td") as td) :: tl -> (td::acc, tl)
  | elem :: tl -> get_td (elem :: acc) tl
  | [] -> assert false
  in
  let rec swap_td acc = function
  | ((X.Tag ("tr", _)) as tr) :: tl ->
      let td1, tl = get_td [] tl in
      let td2, tl = get_td [] tl in
      let acc = td1 @ td2 @ (tr :: acc) in
      swap_td acc tl
  | elem :: tl -> swap_td (elem :: acc) tl
  | [] -> (List.rev acc)
  in
  let x = swap_td [] x in
  X.print_html x
