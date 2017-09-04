open Angstrom;
open Lib.GqlParser;

let rec input_lines file =>
  switch (
    try [input_line file] {
    | End_of_file => []
    }
  ) {
  | [] => []
  | line => List.append line (input_lines file)
  };

let input_string file => input_lines file |> Lib.StrUtils.unlines;

let print_all printer entities =>
  List.map printer entities
  |> List.filter ((!=) "")
  |> Lib.StrUtils.join_with "\n\n\n"
;

let entities = parse_to_entities (input_string stdin);
let result = print_all Lib.GraphenePrinter.print_type entities;
print_endline result;
