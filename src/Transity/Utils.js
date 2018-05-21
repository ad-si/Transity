exports.parseToUnixTimeImpl = function (dateString) {
  return Date.parse(dateString) || null
}
