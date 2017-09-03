let rec join_with (sep: string) (strs: list string) => switch strs {
  | [] => ""
  | [x] => x
  | [x, ...xs] => x ^ sep ^ join_with sep xs
};

let indent = (^) "  ";
let surround s t str => s ^ str ^ t;
let lines = join_with "\n";
let is_whitespace c => switch c {
  | ' ' | '\n' | '\r' | '\t' => true
  | _ => false
};
