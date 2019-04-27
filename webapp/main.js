const Webapp = require('../output/Webapp')
const inputElement = document.getElementById('input')

function writeToOutput () {
  const journal = inputElement.value
  document
      .getElementById('output')
      .innerHTML = Webapp.getBalance(journal)
}

inputElement.addEventListener('input', event => {
  writeToOutput()
})

writeToOutput()
