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

let schema = parse_schema (input_string stdin);
let result = Lib.GraphenePrinter.print_schema schema;
print_endline result;
