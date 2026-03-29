export function parseToUnixTimeImpl (dateString) {
  return Date.parse(dateString) || null
}
