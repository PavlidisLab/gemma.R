#' Set Gemma User
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `setGemmaUser` without specifying the username or password.
#'
#' @usage setGemmaUser(username = NULL, password = NULL)
#'
#' @examples
#' setGemmaUser('username','password') # login
#' setGemmaUser() # logout
#'
#' @export
setGemmaUser <- function(username = NULL, password = NULL) {
  options(gemma.username = username)
  options(gemma.password = password)
}
