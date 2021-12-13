const fs = require("fs")
const archiver = require("archiver")


exports.writeToZipImpl = (nothing, outPathMaybe, files) => {

  return function (onError, onSuccess) {
    const output = outPathMaybe === nothing
      ? process.stdout
      : fs.createWriteStream(outPathMaybe.value0)

    output.on("close", () => {
      onSuccess()
    })


    const archive = archiver("zip")

    archive.on("warning", warning => {
      console.warn(warning)
    })

    archive.on("error", error => {
      onError(error)
    })

    archive.pipe(output)

    files.forEach(file => {
      archive.append(file.content, { name: file.path })
    })

    archive.finalize()

    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess()
    }
  }
}
