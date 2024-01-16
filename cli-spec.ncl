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
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "balance-all",
      description = "Simple balance of all accounts",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "transactions",
      description = "All transactions and their transfers",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "transfers",
      description = "All transfers with one transfer per line",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "entries",
      description = m%"
        All individual deposits & withdrawals, space separated
      "%,
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "entities",
      description = "[WIP] List all referenced entities",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "entities-sorted",
      description = m%"
        [WIP] List all referenced entities sorted alphabetically
      "%,
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "ledger-entries",
      description = "All entries in Ledger format",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "csv",
      description = "Transfers, comma separated (printed to stdout)",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "tsv",
      description = "Transfers, tab separated (printed to stdout)",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "xlsx",
      description = "XLSX file with all transfers (printed to stdout)",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "entries-by-account",
      description = m%"
        All individual deposits & withdrawals, grouped by account
      "%,
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "gplot",
      description = m%"
        Code and data for gnuplot impulse diagram
        to visualize transfers of all accounts
      "%,
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "gplot-cumul",
      description = m%"
        Code and data for cumuluative gnuplot step chart
        to visualize balance of all accounts
      "%,
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "unused-files",
      description = m%"
        Recursively list all files in a directory
        which are not referenced in the journal
      "%,
      arguments = ["DIRECTORY", "JOURNAL_FILE"],
    },
    {
      name = "help",
      description = "Print this help dialog",
      arguments = ["JOURNAL_FILE"],
    },
    {
      name = "version",
      description = "Print currently used version",
      arguments = ["JOURNAL_FILE"],
    },
  ]
}
