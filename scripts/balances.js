import path from "path"

import fse from "fs-extra"
import yaml from "js-yaml"

import getPaypalBalances from "./getPaypalBalances"
import getAwsBalance from "./getAwsBalance"
import getMbsBalance from "./getMbsBalance"
import getPostBalance from "./getPostBalance"


const envFilePath = path.resolve(__dirname, "../../environment.yaml")
const accountMap = {
  aws: {
    provider: "aws",
    url: "aws.amazon.com",
    getBalance: getAwsBalance,
  },
  mbs: {
    provider: "mbs",
    url: "mbs.de",
    getBalance: getMbsBalance,
  },
  paypalEur: {
    provider: "paypal",
    url: "paypal.de/eur",
    getBalance: getPaypalBalanceFunc("eur"),
  },
  paypalUsd: {
    provider: "paypal",
    url: "paypal.de/usd",
    getBalance: getPaypalBalanceFunc("usd"),
  },
  deutschepost: {
    provider: "deutschepost",
    url: "portokasse.deutschepost.de",
    getBalance: getPostBalance,
  },
}


let paypalBalancesPromise = null
function getPaypalBalanceFunc (commodity) {
  return async config => {
    if (paypalBalancesPromise == null) {
      paypalBalancesPromise = getPaypalBalances(config)
    }

    const balances = await paypalBalancesPromise
    const balanceMap = {
      eur: balances[0],
      usd: balances[1],
    }
    return balanceMap[commodity]
  }
}

function prettyFormat (account, balance) {
  const paddingStart = 28
  return account.padStart(paddingStart) + ": " + balance
}

function prettyPrint (account, balance) {
  console.info(prettyFormat(account, balance))
}


async function main () {
  try {
    const envYaml = await fse.readFile(envFilePath)
    const env = yaml.safeLoad(envYaml)
    const config = {
      isDevMode: false,
    }
    const merge = Object.assign

    Object
      .values(accountMap)
      .forEach(async account => {
        const balanceConfig = merge(config, env[account.provider])

        try {
          const balance = await account.getBalance(balanceConfig)
          prettyPrint(account.url, balance)
        }
        catch (error) {
          console.error(error)
        }
      })
  }
  catch (error) {
    if (error.code === "ENOENT") {
      console.warn(
        "Retrieve the environment.yaml file from gopass" +
        "and add it to the project's root directory",
      )
    }
    else {
      console.error(error)
    }
  }
}


main()
