import init, { get_balance } from "./pkg/transity.js"

const inputElement = document.getElementById("input")

async function setup() {
  await init()
  writeToOutput()
}

function writeToOutput() {
  const journal = inputElement.value
  document.getElementById("output").innerHTML = get_balance(journal)
}

inputElement.addEventListener("input", () => {
  writeToOutput()
})

setup()
