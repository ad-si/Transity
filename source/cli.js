#! /usr/bin/env node

const path = require('path')

const subcommands = {
  'help': {
    desc: 'Display current usage information',
    func: printUsage,
  },
  'balance': {
    desc: 'Display current balance of all accounts',
    func: (args, config) => {
      // Lazy load transity to make cli more repsonsive
      const transity = require('.')
      transity.renderBalance(config)
    },
  },
  'transactions': {
    desc: 'Display all transactions',
    func: (args, config) => {
      // Lazy load transity to make cli more repsonsive
      const transity = require('.')
      transity.renderTransactions(config)
    },
  },
}

function printUsage () {
  console.info('Commands:')
  // eslint-disable-next-line no-console
  for (const [key, value] of Object.entries(subcommands)) {
    console.info(`  ${key.padEnd(15)}${value.desc}`)
  }
}

function main (args) {
  const availableCommands = Object.keys(subcommands)
  const calledCommand = args[0] || ''

  if (
    !availableCommands.includes(calledCommand) ||
    calledCommand === 'help'
  ) {
    if (calledCommand) {
      console.error(`Command "${calledCommand}" is not available!\n`)
    }
    printUsage()
    return
  }

  // Lazy load dependencies to make cli more repsonsive
  const Config = require('@datatypes/config')
  const config = new Config({
    appName: 'transity',
  })

  config
    .loadDefaultFiles()
    .loadCliArguments()
    .loadEnvironment()
    .merge({
      directory: path.resolve('.'),
    })

  subcommands[calledCommand]
    .func(args.slice(1), config.config)
}


main(process.argv.slice(2))
