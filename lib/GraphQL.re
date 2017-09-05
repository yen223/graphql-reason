type name = string;
type description = option string;
type default_value = option string;
type deprecated =
  | IsDeprecated string
  | NotDeprecated
;
type enum_value = EnumValue name description deprecated;
type enum_values = list enum_value;
type field = Field {
  name,
  description,
  args: (list input_value_type),
  output_type: t,
  deprecated,
}
and fields = list field
and input_value_type = InputValue {
  name,
  description,
  default_value,
  graphql_type: t,
}
and possible_types = list t
and t =
  | Scalar      {name, description}
  | Object      {name, description, fields, interfaces: list t}
  | Interface   {name, description, fields, possible_types}
  | Union       {name, description, possible_types}
  | Enum        {name, description, enum_values}
  | InputObject {name, description, input_value_types: (list input_value_type)}
  | ListType    t
  | NonNull     t /*TODO: Figure out way to avoid nested NonNulls. GADT? */
  | LazyType    string  /*TODO: Figure out way to avoid this */
;

module TypeMap = Map.Make String;
type type_map = TypeMap.t t;
type schema = Schema {query: t, mutation: option t, types: type_map};
type directive_location =
  | QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
;
type directive = Directive {
  name,
  description,
  locations: list directive_location,
  args: list input_value_type,
};

let rec type_name = fun
| Scalar      {name, _}
| Object      {name, _}
| Interface   {name, _}
| Union       {name, _}
| Enum        {name, _}
| InputObject {name, _}
| LazyType name => name
| ListType typ
| NonNull typ => type_name typ
;

let build_type_map entities => TypeMap.({
  let rec aux mp ls => {
    switch ls {
    | [] => mp
    | [x, ...xs] => aux (add (type_name x) x mp) xs
    };
  };
  aux empty entities;
});
