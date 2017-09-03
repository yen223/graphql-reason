type name = string;
type description = option string;
type default_value = option string;
type deprecated =
  | IsDeprecated string
  | NotDeprecated
;
type enum_value = EnumValue name description deprecated;
type enum_values = list enum_value;
type graphql_type =
  | Scalar      {name, description}
  | Object      {name, description, fields, interfaces: list graphql_type}
  | Interface   {name, description, fields, possible_types}
  | Union       {name, description, possible_types}
  | Enum        {name, description, enum_values}
  | InputObject {name, description, input_value_types: (list input_value_type)}
  | ListType    graphql_type
  | NonNull     graphql_type /*TODO: Figure out way to avoid nested NonNulls. GADT? */
  | LazyType    string  /*TODO: Figure out way to avoid this */
  | Schema      fields  /*TODO: Schema should be separate */

and field = Field {
  name,
  description,
  args: (list input_value_type),
  output_type: graphql_type,
  deprecated,
}
and fields = list field
and input_value_type = InputValue {
  name,
  description,
  default_value,
  graphql_type,
}
and possible_types = list graphql_type
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
