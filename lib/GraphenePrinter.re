open GraphQL;
open StrUtils;

let indent = (^) "    ";
let block attrs => List.map indent attrs;
let quote s => "\"" ^ s ^ "\"";
let triple_quote s => "\"\"\"" ^ s ^ "\"\"\"";
let uncamel_case s =>
    Str.global_replace (Str.regexp "[A-Z]") "_\\0" s
    |> String.lowercase_ascii;
let call fun_name fun_args => fun_name ^ "(" ^ fun_args ^ ")";

let mk_class name bases attrs => {
    let str_bases = switch bases {
    | [] => ""
    | [x, ...xs] => "(" ^ (join_with "," bases) ^ ")"
    };
    "class " ^ name ^ str_bases ^ ":\n" ^ (block attrs |> unlines)
};

let meta_class attrs => ["class Meta:", ...block attrs];
let rec print_type_name = fun
  | Scalar      {name, _}
  | Object      {name, _}
  | Interface   {name, _}
  | Union       {name, _}
  | Enum        {name, _}
  | InputObject {name, _}
  | LazyType name => name
  | ListType t => call "List" (print_type_name t)
  | NonNull t => print_type_name t
  | Schema _ => "Schema"
;

let is_some = fun |Some _ => true
                  |None   => false;

let opt_map f o => switch o {
  | Some n => Some (f n)
  | None => None
};

let print_as_argument (InputValue {name, description, default_value, graphql_type}) => {
  let str_desc = opt_map (fun d => "description=" ^ triple_quote d) description;
  let str_default = opt_map (fun d => "default_value=" ^ quote d) default_value;
  let field_args = [
    str_desc,
    str_default,
  ]
  |> List.filter is_some
  |> List.map (fun | Some x => x
                   | None => failwith "Invariant violation!")
  |> join_with ", ";
  name ^ "=" ^ call "Argument" (print_type_name graphql_type ^ field_args)
};

let print_as_field field_type name description args typ deprecated => {
  let str_required = switch typ {
    | NonNull _ => Some "required=True"
    | _ => None
  };
  let str_deprecated = switch deprecated {
    | IsDeprecated s => Some ("deprecation_reason=" ^ quote s)
    | NotDeprecated  => None
  };
  let field_args = [
    Some (print_type_name typ),
    str_required,
    opt_map (fun d => "description=" ^ triple_quote d) description,
    str_deprecated,
  ]
  |> List.filter is_some
  |> List.map (fun | Some x => x
                   | None => failwith "Invariant violation!")
  |> (fun x => List.append x (List.map print_as_argument args))
  ;
  uncamel_case name ^ " = " ^ call field_type (join_with ", " field_args)
};

let print_field (Field {name, description, args, output_type, deprecated}) =>
  print_as_field "Field" name description args output_type deprecated;

let print_input_value (InputValue {name, description, default_value, graphql_type}) =>
  print_as_field "InputField" name description [] graphql_type NotDeprecated;

let print_type = fun
  /* | Scalar {name, description} => Scalar {name, description} */
  | Object {name, description, fields, interfaces} => {
    mk_class name ["ObjectType"] (List.map print_field fields)
  }
  | Interface {name, description, fields, possible_types} => {
    mk_class name ["Interface"] (List.map print_field fields)
  }
  | Union {name, description, possible_types} => {
    let type_list = (List.map print_type_name possible_types)
                    |> join_with ", ";
    let types = "types = [" ^ type_list ^ "]";
    let meta = meta_class [types];
    mk_class name ["Union"] meta
  }
  | Enum {name, description, enum_values} => {
    let enum_values_lines = List.mapi (
        fun idx (EnumValue n desc depr) => n ^ " = " ^ string_of_int idx
    ) enum_values;
    mk_class name ["Enum"] enum_values_lines
  }
  | InputObject {name, description, input_value_types} => {
    let attrs = List.map print_input_value input_value_types;
    mk_class name ["InputObjectType"] attrs
  }
  | _ => ""
;
