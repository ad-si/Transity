let normString : String -> String =
    std.function.compose
      (std.string.replace_regex "\\s+" " ")
      std.string.trim
  in
let TrimmedString : _ -> String -> String = fun _label => normString
  in
let Command = {
  name
    | doc "The name of the command"
    | TrimmedString,
  description
    | doc "The description of the command"
    | TrimmedString,
  arguments
    | doc "The list of arguments"
    | Array String,
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
