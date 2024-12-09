#' @importFrom jsonlite fromJSON
#' @examples
#' jsoncars <- jsonlite::toJSON(mtcars, pretty = TRUE)
#' fromJSON(jsoncars)
#' @export
jsonlite::fromJSON

#' @importFrom jsonlite read_json
#' @export
jsonlite::read_json
