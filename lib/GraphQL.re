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
  | Schema      fields  /*TODO: Schema should be separate */
;

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
