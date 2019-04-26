#! /usr/bin/env bash

set -e

journalPath="$1"

# The ugly `{{PLACEHOLDER}}` workaround is necessary to make it work
# if no note is specified for a transaction,

{
  echo date,id,from,to,amount,note,tags;
  ledger \
    --file "$journalPath" \
    --csv-format '%(
        format_date(date,"%Y-%m-%d")),%(
        quoted(code)),%(
        quoted("")),%(
        quoted(payee)),%(quoted(display_amount)),%(
        quoted(join(trim(xact.note | "{{PLACEHOLDER}}")))),%(
        quoted("- " + display_account + "\\\\n- " +
          join(trim(note | "{{PLACEHOLDER}}")))
        )\n' \
    --sort date \
    csv;
} \
| sed "s/{{PLACEHOLDER}}//g" \
| sed 's/\\\\"/""/g'  # Fix escaping of `"` in CSV

