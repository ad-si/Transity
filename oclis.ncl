let journalArgs = [
  {
    name = "JOURNAL_FILE",
    description = "Path to the journal file",
    type = 'Text,
  },
  {
    name = "JOURNAL_FILE",
    description = "Additional journal files",
    type = 'List-Text,
  },
]
in
{
  name = "transity",
  description = m%"
    Transity is a full fledged, CLI based plain text accounting tool.
  "%,
  version = "0.8.0",
  funcName = "runApp",
  commands = [
    {
      name = "balance",
      description = "Simple balance of the owner's accounts",
      arguments = journalArgs,
    },
    {
      name = "balance-all",
      description = "Simple balance of all accounts",
      arguments = journalArgs,
    },
    {
      name = "transactions",
      description = "All transactions and their transfers",
      arguments = journalArgs,
    },
    {
      name = "transfers",
      description = "All transfers with one transfer per line",
      arguments = journalArgs,
    },
    {
      name = "entries",
      description = m%"
        All individual deposits & withdrawals, space separated
      "%,
      arguments = journalArgs,
    },
    {
      name = "entities",
      description = "[WIP] List all referenced entities",
      arguments = journalArgs,
    },
    {
      name = "entities-sorted",
      description = m%"
        [WIP] List all referenced entities sorted alphabetically
      "%,
      arguments = journalArgs,
    },
    {
      name = "ledger-entries",
      description = "All entries in Ledger format",
      arguments = journalArgs,
    },
    {
      name = "csv",
      description = "Transfers, comma separated (printed to stdout)",
      arguments = journalArgs,
    },
    {
      name = "tsv",
      description = "Transfers, tab separated (printed to stdout)",
      arguments = journalArgs,
    },
    {
      name = "xlsx",
      description = "XLSX file with all transfers (printed to stdout)",
      arguments = journalArgs,
    },
    {
      name = "entries-by-account",
      description = m%"
        All individual deposits & withdrawals, grouped by account
      "%,
      arguments = journalArgs,
    },
    {
      name = "gplot",
      description = m%"
        Code and data for gnuplot impulse diagram
        to visualize transfers of all accounts
      "%,
      arguments = journalArgs,
    },
    {
      name = "gplot-cumul",
      description = m%"
        Code and data for cumuluative gnuplot step chart
        to visualize balance of all accounts
      "%,
      arguments = journalArgs,
    },
    {
      name = "unused-files",
      description = m%"
        Recursively list all files in a directory
        which are not referenced in the journal
      "%,
      arguments =
        [
          {
            name = "DIRECTORY",
            description = "Path to the directory",
            type = 'Text,
          },
        ]
        @ journalArgs,
    },
    {
      name = "help",
      description = "Print this help dialog",
      arguments = journalArgs,
    },
    {
      name = "version",
      description = "Print currently used version",
      arguments = journalArgs,
    },
  ]
}
