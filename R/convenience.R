#' Set Gemma User
#'
#' Allows the user to access information that requires logging in to Gemma. To log out, run `setGemmaUser` without specifying the username or password.
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

# DWR requests seem to have three parts:
# 1. An initial request to establish a trackable ID
# 2. Repeated polling until the request finishes
# 3. Receipt of status (once polling reveals completion)

#' Get differential expression data
#'
#' This occurs asynchronously as data will not always be available upon request. If the data is not available,
#' the function will continue to run and poll for updates every 3 seconds until it becomes available.
#'
#' @param dataset The experiment ID (integer)
#' @param diffExSet The result set ID for the differential expression analysis we want (integer) @seealso getDatasetDEA
#'
#' @return A list of `data.table`s that were read in, typically the first one is analysis results (elements, p-values) and the second is full data (elements, FC, t- and p-values)
#'
#' @examples
#' async::synchronise(getDiffExData(1, 59016))
#'
#' @export
getDiffExData <- async::async(function(dataset, diffExSet) {
  REQ1 <- 'callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=ExpressionExperimentDataFetchController
c0-methodName=getDiffExpressionDataFile
c0-id=0
c0-param0=number:{diffExSet}
batchId=4
'
  REQ2 <- 'callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=ProgressStatusService
c0-methodName=getSubmittedTask
c0-id=0
c0-param0=string:{taskID}
batchId={countN}'
  REQ3 <- 'callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={dataset}
httpSessionId=
scriptSessionId=1
c0-scriptName=TaskCompletionController
c0-methodName=checkResult
c0-id=0
c0-param0=string:{taskID}
batchId={countN + 1}
'

  # Make the initial request
  async::http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/ExpressionExperimentDataFetchController.getDiffExpressionDataFile.dwr'), glue::glue(REQ1))$then(function(response) {
    if(response$status != 200) {
      warning(paste0('Unable to get differential expression data file: ', response$status))
      return(NULL)
    }

    # Receive a task ID to track
    taskID <- gsub('\'|"', '', strsplit(gsub('.*remoteHandleCallback\\(([^\\)]+)\\).*', '\\1', rawToChar(response$content)), ',')[[1]])[3]
    countN <- 5

    requery <- async::async(function(taskID, countN) {
      # Poll for completion
      async::http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/ProgressStatusService.getSubmittedTask.dwr'), glue::glue(REQ2))$then(function(response) {
        if(response$status != 200) {
          warning(paste0('Unable to get task data: ', response$status))
          return(NULL)
        }

        # Detect completion
        if(grepl('done:true', rawToChar(response$content), fixed = T)) {
          # Query where to find the result
          async::http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/TaskCompletionController.checkResult.dwr'), glue::glue(REQ3))$then(function(response) {
            if(response$status != 200) {
              warning(paste0('Unable to get task result: ', response$status))
              return(NULL)
            }

            # Extract the file location
            fileLoc <- gsub('\'|"', '', strsplit(gsub('.*remoteHandleCallback\\(([^\\)]+)\\).*', '\\1', rawToChar(response$content)), ',')[[1]])[3]

            # Make a temp file to unzip to
            tmp <- tempfile()
            base <- getOption('gemma.base', 'https://gemma.msl.ubc.ca/')
            async::http_get(paste0(substring(base, 1, nchar(base) - 1), fileLoc), file = tmp)$then(function(...) {
              filenames <- unzip(tmp, list = TRUE)$Name

              # Unzip results and fread in
              ret <- setNames(lapply(filenames, function(x) data.table::fread(cmd = glue('unzip -p {tmp} {x}'),
                                                                              colClasses = c(Element_Name = 'character', Gene_Symbol = 'character', Gene_Name = 'character'))),
                              filenames)
              unlink(tmp)
              ret
            })
          })
        } else {
          # Fail to detect completion, try again in 3 seconds
          async::delay(3)$then(function(...) {
            requery(taskID, countN + 1)
          })
        }
      })
    })

    requery(taskID, countN)
  })
})
