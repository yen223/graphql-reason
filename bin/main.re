open Angstrom;
open Lib.GqlParser;

/* let buf = Bytes.create 1000000;
let _ = input stdin buf 0 1000000; */

let rec input_lines file =>
  switch (
    try [input_line file] {
    | End_of_file => []
    }
  ) {
  | [] => []
  | line => List.append line (input_lines file)
  };

let input_string file => input_lines file |> Lib.StrUtils.join_with "\n";

let result = parse_and_print (input_string stdin);
print_endline result;
