let normString : String -> String =
    std.function.compose
      (std.string.replace_regex "\\s+" " ")
      std.string.trim
  in
let TrimmedString : _ -> String -> String = fun _label => normString
  in
let NotRequiredIfList = fun label args =>
  args
  |> std.array.map
    (
      fun arg =>
        if arg."type" == 'List-Text then
          if std.record.has_field "optional" arg && arg."optional" == false then
            std.contract.blame_with_message
              (
                "Incorrect values for argument "
                ++ arg.name
                ++ ": Arguments of type "
                ++ std.string.from_enum arg.type
                ++ " must always be optional"
              )
              label
          else
            arg
        else
          arg
    )
in
let EitherNameOrShortName = fun label options =>
  options
  |> std.array.map
    (
      fun option =>
        let nameIsSet =
          std.record.has_field "name" option
          && option.name != ""
        in
        let shortNameIsSet =
          std.record.has_field "shortName" option
          && option.shortName != ""
        in
        if !nameIsSet && !shortNameIsSet then
          std.contract.blame_with_message
            "Option must have either a name or a shortName"
            label
        else
          option
    )
in
let Argument = {
  name | String,
  description | String,
  type
    | [|
      'Text,
      'Int,
      'Float,
      'Bool,
      'List-Text,
      'List-Int,
      'List-Float,
      'List-Bool,
    |],
  "optional"
    | optional
    | Bool,
  "default"
    | optional
    | String
    | Number
    | Bool
    | Array String
    | Array Number
    | Array Bool,
}
in
let Option = {
  name
    | doc "The name of the option (without the leading --)"
    | optional
    | TrimmedString,
  shortName
    | doc "The short name of the option"
    | optional
    | std.string.Character,
  description
    | doc "The description of the option"
    | TrimmedString,
  argument
    | doc "The argument of the option"
    | optional
    | Argument
    | EitherNameOrShortName,
  "optional"
    | optional
    | Bool,
  "default"
    | optional
    | String
    | Number
    | Bool
    | Array String
    | Array Number
    | Array Bool,
}
in
let Command = {
  name
    | doc "The name of the command"
    | TrimmedString,
  description
    | doc "The description of the command"
    | TrimmedString,
  options
    | doc "The list of options"
    | optional
    | Array Option,
  arguments
    | doc "The list of arguments"
    | Array Argument
    | NotRequiredIfList,
  funcName
    | doc "The function to be called"
    | optional
    | TrimmedString,
}
in
{
  "_⛔️_"
    | doc "A warning for the exported file to not edit it manually"
    | default
    =
      normString
        m%"
        This file is generated from cli-spec.ncl.
        DO NOT EDIT IT MANUALLY!
      "%,
  name
    | doc "The name of the CLI app"
    | TrimmedString,
  description
    | doc "The description of the CLI app"
    | TrimmedString,
  version
    | doc "The version of the CLI app"
    | TrimmedString,
  funcName
    | doc "The function to be called"
    | TrimmedString,
  commands
    | doc "The list of available sub-commands"
    | Array Command,
}
