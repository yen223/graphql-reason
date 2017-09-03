open Angstrom;
open StrUtils;

let is_whitespace c => switch c {
  | ' ' | '\n' | '\r' | '\t' => true
  | _ => false
};

let chars_to_string cs => {
  let buf = Buffer.create (List.length cs);
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf
};

let skip_whitespace = skip_while is_whitespace;
let ( >>> ) a b => a *> skip_whitespace *> b;

let ( <<< ) a b => a <\* skip_whitespace <\* b;

let to_some x => Some x;
let opt p => p >>| to_some <|> return None;
let token a => (skip_while is_whitespace) *> a <\* (skip_while is_whitespace);
let between a b p => a *> p <\* b;
let parens p => between (token (char '(')) (token (char ')')) p;
let braces p => between (token (char '{')) (token (char '}')) p;
let comma = token (string ",") <?> "Comma error!";
let quoted c => between (char c) (char c) (((!=) c) |> satisfy |> many1) >>| chars_to_string;


let is_name_char = fun | 'a' .. 'z'
                       | 'A' .. 'Z'
                       | '_'  => true
                       | _ => false;
let alpha_num = {
  let is_alpha_num c => switch c {
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' => true
    | _ => false
  };
  many (satisfy is_alpha_num)
};

let parse_name = token (
    (satisfy is_name_char) >>= fun c =>
    alpha_num >>= fun cs =>
    return ([c, ...cs] |> chars_to_string)
  ) <?> "Name must be [a-zA-Z_]"
;

let base_type = GraphQLTypes.(
  parse_name >>= fun t =>
  option None (char '!' >>| to_some) >>= fun c =>
  return (switch c {
    | Some _ => NonNull (LazyType t)
    | None   => LazyType t
  })
);

let parse_type = GraphQLTypes.(
  fix (fun parse_type => {
    char '[' >>> parse_type <<< char ']' >>= fun t =>
    option None (char '!' >>| to_some)   >>= fun c =>
    return (switch c {
    | Some _ => NonNull (ListType t)
    | None   => ListType t
    })
  } <|> base_type)
)
;

let parse_argument =
  GraphQLTypes.(
    parse_name       >>= fun name =>
    token (char ':') >>= fun _ =>
    parse_type       >>= fun graphql_type =>
    InputValue {
      name,
      description: None,
      default_value: None,
      graphql_type,
    } |> return
  )
;

let parse_key_value =
  parse_name       >>= fun key =>
  token (char ':') >>= fun _ =>
  quoted '"'       >>= fun value =>
  return (key, value)
;

let parse_deprecation =
  token (string "@deprecated")          >>= fun _ =>
  parens (token (string "reason:") *> quoted '"') >>= fun reason =>
  reason |> return
;
let parse_field = GraphQLTypes.(
  token (parse_name)                                >>= fun name =>
  option [] (parens (sep_by1 comma parse_argument)) >>= fun args =>
  token (char ':')                                  >>= fun _ =>
  token parse_type                                  >>= fun output_type =>
  opt parse_deprecation                             >>= fun dep =>
  Field {
    name,
    description: None,
    args,
    output_type,
    deprecated: switch dep {
    | Some reason => IsDeprecated reason
    | None        => NotDeprecated
    }
  } |> return
);

let parse_type_interface =
  token (string "implements") *> (sep_by1 comma parse_type) >>= fun t =>
  return t
;

let parse_object = GraphQLTypes.(
  token (string "type") *> parse_name     >>= fun name =>
  token (option [] parse_type_interface)  >>= fun interfaces =>
  token (braces (many parse_field))       >>= fun fields =>
  Object {name, description: None, fields, interfaces} |> return
);

let parse_enum = GraphQLTypes.(
  token (string "enum") *> parse_name       >>= fun name =>
  token (braces (many @@ token parse_name)) >>= fun values =>
  Enum {
    name,
    description: None,
    enum_values: List.map (fun name => EnumValue name None NotDeprecated) values,
  } |> return
);

let parse_union = GraphQLTypes.(
  token (string "union") >>> parse_name <<< token (char '=') >>= fun name =>
  token (sep_by1 (token (char '|')) (parse_type <\* skip_whitespace)) >>= fun possible_types =>
  Union {name, description: None, possible_types} |> return
);

let parse_scalar = GraphQLTypes.(
  token (string "scalar") *> parse_name >>= fun name =>
  Scalar {name, description: None} |> return
);

let parse_interface = GraphQLTypes.(
  token (string "interface") *> parse_name >>= fun name =>
  token (braces (token (many parse_field)))        >>= fun fields =>
  Interface {
    name,
    description: None,
    fields,
    possible_types: [],
  } |> return
);

let parse_input = GraphQLTypes.(
  token (string "input") *> parse_name >>= fun name =>
  token (braces (many parse_argument)) >>= fun input_value_types =>
  InputObject {
    name,
    description: None,
    input_value_types,
  } |> return
);

let parse_schema = GraphQLTypes.(
  token (string "schema") >>= fun _ =>
  token (braces (many parse_field)) >>= fun fields =>
  Schema fields |> return
);

let parse_entity = parse_schema
               <|> parse_input
               <|> parse_interface
               <|> parse_scalar
               <|> parse_union
               <|> parse_enum
               <|> parse_object;
let parse_all =
  many (parse_entity |> token) >>= fun res =>
  skip_whitespace              >>= fun _ =>
  return res
;

let print_all ent_list => ent_list |> List.map GqlPrinter.print_type |> join_with "\n\n";
let parse_and_print str => switch (parse_only parse_all (`String str)){
  | Result.Ok v => print_all v
  | Result.Error message => failwith message
};
let parse_to_entities str => switch (parse_only parse_all (`String str)) {
  | Result.Ok v => v
  | Result.Error message => failwith message
};

let test_parser p str => switch (parse_only p (`String str)) {
  | Result.Ok v => v
  | Result.Error message => failwith message
};
let rec input_lines file =>
  switch (
    try [input_line file] {
    | End_of_file => []
    }
  ) {
  | [] => []
  | line => List.append line (input_lines file)
  };

let input_string file => input_lines file |> join_with "\n";
let actual = open_in "./actual.gql" |> input_string;
