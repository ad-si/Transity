import fse from "fs-extra"
import path from "path"
import csvParse from "csv-parse/lib/sync"

async function searchAndReplace () {
  const replacementsFilePath = path.resolve(process.argv[2])
  const textFilePath = path.resolve(process.argv[3])

  const [textFileContent, replacementsContent] = await Promise
    .all([
      fse.readFile(textFilePath, "utf8"),
      fse.readFile(replacementsFilePath, "utf-8"),
    ])
  let fixedText = textFileContent
  const records = csvParse(
    replacementsContent,
    {
      delimiter: "\t",
      comment: "#",
    },
  )

  records.forEach(pair => {
    const [match, replacement] = pair
    const pattern = new RegExp(`(\\W|^)${match}(\\W|$)`, "igm")
    const normReplacement = (/^\\u.*/g).test(replacement)
      ? String.fromCharCode(`0x ${replacement.substr(2)}`)
      : replacement

    fixedText = fixedText.replace(pattern, `$1${normReplacement}$2`)
  })

  console.info(fixedText)
}


if (!process.argv[2] || !process.argv[3]) {
  const commandName = __filename.replace(__dirname + "/", "")
  console.info(`Usage: ${commandName} <tsv-file> <text-file>`)
}
else {
  searchAndReplace()
}
