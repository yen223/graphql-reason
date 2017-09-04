let join_with (sep: string) (strs: list string) => switch strs {
  | [] => ""
  | [x, ...xs] => List.fold_left (fun a b => a ^ sep ^ b) x xs
};

let indent = (^) "  ";
let surround s t str => s ^ str ^ t;
let unlines = join_with "\n";

let is_whitespace = fun
  | ' ' | '\n' | '\r' | '\t' => true
  | _ => false
;
