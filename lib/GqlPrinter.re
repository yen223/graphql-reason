open GraphQLTypes;
open StrUtils;
open List;

let (<<) f g x => f(g(x));
let str_keyval key value => key ^ ": " ^ value;
let block header block_lines => {
  let inner = List.map indent block_lines;
  header ^ " {\n" ^ (unlines inner) ^ "\n}"
};

let print_enum_value (EnumValue name _ _) => name;

let print_deprecated = fun
  | IsDeprecated reason => "@deprecated(reason: \"" ^ reason ^ "\")"
  | NotDeprecated => ""
;

let rec print_type = fun
  | Scalar {name, description} => "scalar " ^ name
  | Object {name, description, fields, interfaces} => {
      let print_interfaces interfaces => switch interfaces {
        | [] => ""
        | _  => " implements " ^ (join_with ", " (List.map print_type interfaces))
      };
      let header = "type " ^ name ^ (print_interfaces interfaces);
      let block_lines = (map print_field fields) |> sort String.compare;
      block header block_lines
    }
  | Interface {name, description, fields, possible_types} => {
      let header = "interface " ^ name;
      let block_lines = (map print_field fields) |> sort String.compare;
      block header block_lines
    }
  | Union {name, description, possible_types} =>
      "union " ^ name ^ " = " ^ (possible_types |> map print_type |> join_with " | ")
  | Enum {name, description, enum_values} => {
      let header = "enum " ^ name;
      block header (List.map print_enum_value enum_values)
    }
  | InputObject {name, description, input_value_types} => {
      let header = "input " ^ name;
      let block_lines = (map print_input_value input_value_types) |> sort String.compare;
      block header block_lines
    }
  | ListType typ  => "[" ^ print_type typ ^ "]"
  | NonNull typ   => print_type typ ^ "!"
  | LazyType name => name
  | Schema fields => {
      let header = "schema";
      let block_lines = (map print_field fields) |> sort String.compare;
      block header block_lines
    }

and print_field (f: field) => {
  let Field {name, description, args, output_type, deprecated} = f;
  let print_args args => switch args {
  | []  => ""
  | _   => List.map print_input_value args
          |> List.sort String.compare
          |> join_with ", "
          |> surround "(" ")";
  };

  name ^ (print_args args) ^ ": " ^ (print_type output_type) ^ " " ^ print_deprecated deprecated
}
and print_input_value inp => {
  let InputValue {name, description, default_value, graphql_type} = inp;
  name ^ ": " ^ (print_type graphql_type)
};
