exports.parseToUnixTime = function (dateString) {
  return Date.parse(dateString) || 0
}
