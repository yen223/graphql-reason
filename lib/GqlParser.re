open Angstrom;
open StrUtils;


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
  let is_alpha_num = fun
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' => true
    | _ => false
  ;
  many (satisfy is_alpha_num)
};

let parse_name = token (
    (satisfy is_name_char) >>= fun c =>
    alpha_num >>= fun cs =>
    return ([c, ...cs] |> chars_to_string)
  ) <?> "Name must be [a-zA-Z_]"
;

let base_type = GraphQL.(
  parse_name >>= fun t =>
  option None (char '!' >>| to_some) >>= fun c =>
  return (switch c {
    | Some _ => NonNull (LazyType t)
    | None   => LazyType t
  })
);

let parse_type = GraphQL.(
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
  GraphQL.(
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
  token (string "@deprecated")                    >>= fun _ =>
  parens (token (string "reason:") *> quoted '"') >>= fun reason =>
  reason |> return
;

let parse_field = GraphQL.(
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

let parse_object = GraphQL.(
  token (string "type") *> parse_name     >>= fun name =>
  token (option [] parse_type_interface)  >>= fun interfaces =>
  token (braces (many parse_field))       >>= fun fields =>
  Object {name, description: None, fields, interfaces} |> return
);

let parse_enum = GraphQL.(
  token (string "enum") *> parse_name       >>= fun name =>
  token (braces (many @@ token parse_name)) >>= fun values =>
  Enum {
    name,
    description: None,
    enum_values: List.map (fun name => EnumValue name None NotDeprecated) values,
  } |> return
);

let parse_union = GraphQL.(
  token (string "union") >>> parse_name <<< token (char '=') >>= fun name =>
  token (sep_by1 (token (char '|')) (parse_type <\* skip_whitespace)) >>= fun possible_types =>
  Union {name, description: None, possible_types} |> return
);

let parse_scalar = GraphQL.(
  token (string "scalar") *> parse_name >>= fun name =>
  Scalar {name, description: None} |> return
);

let parse_interface = GraphQL.(
  token (string "interface") *> parse_name >>= fun name =>
  token (braces (token (many parse_field)))        >>= fun fields =>
  Interface {
    name,
    description: None,
    fields,
    possible_types: [],
  } |> return
);

let parse_input = GraphQL.(
  token (string "input") *> parse_name >>= fun name =>
  token (braces (many parse_argument)) >>= fun input_value_types =>
  InputObject {
    name,
    description: None,
    input_value_types,
  } |> return
);

let parse_key k => GraphQL.(
  token (string k)       >>= fun _ =>
  token (char ':')       >>= fun _ =>
  token (parse_name)     >>= fun name =>
  LazyType name |> return
);

type entity_or_schema =
  | Schema' {query: GraphQL.t, mutation: option GraphQL.t}
  | Entity' GraphQL.t
;

let parse_schema =
  token (string "schema")    >>= fun _ =>
  token (string "{")         >>= fun _ =>
  (parse_key "query")        >>= fun query =>  /*TODO: query doesn't always come before mutation */
  opt (parse_key "mutation") >>= fun mutation =>
  token (string "}")         >>= fun _ =>
  Schema' {query, mutation} |> return
;

let parse_entity = parse_input
               <|> parse_interface
               <|> parse_scalar
               <|> parse_union
               <|> parse_enum
               <|> parse_object;

let parse_entity_or_schema =
  parse_schema <|> (parse_entity >>| (fun x => Entity' x))
;

let build_schema entities' => GraphQL.({
  let is_schema = fun
    | Schema' _ => true
    | Entity' _ => false
  ;
  let (schemas', entities) = List.partition is_schema entities';
  let types = entities
              |> List.map (fun (Entity' x) => x)
              |> build_type_map;
  let (Schema' {query, mutation}) = List.hd schemas';  /* TODO: no head! */
  Schema {query, mutation, types}
});


let parse_all =
  many (parse_entity_or_schema |> token) >>= fun res =>
  skip_whitespace                        >>= fun _ =>
  build_schema res |> return
;

let parse_schema str => switch (parse_only parse_all (`String str)) {
  | Result.Ok v => v
  | Result.Error message => failwith message
};

let test_parser p str => switch (parse_only p (`String str)) {
  | Result.Ok v => v
  | Result.Error message => failwith message
};
