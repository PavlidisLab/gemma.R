#' Full API wrapper
#' 
#' Exposes the full Gemma API. Auto generated using 
#' \href{https://openapi-generator.tech/}{OpenAPI generator} and used internally
#' in endpoint functions. Exposed to user to allow access unsupported endpoints
#' by the main package.
#' @inheritSection DefaultApi Methods
#' 
#' @export
#' @keywords misc
#' 
#' @examples
#' 
gemma_api = DefaultApi$new()
gemma_api$api_client$base_path = gsub('/$','',gemmaPath())
