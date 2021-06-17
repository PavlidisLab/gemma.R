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
#' @param eeID The experiment ID (integer)
#' @param rsID The result set ID for the differential expression analysis we want (integer) @seealso getDatasetDEA
#'
#' @return A list of `data.table`s that were read in, typically the first one is analysis results (elements, p-values) and the second is full data (elements, FC, t- and p-values)
#'
#' @examples
#' synchronise(getDiffExData(1, 59016))
#'
#' #' @export
getDiffExData <- async(function(eeID, rsID) {
  # Make the initial request
  http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/ExpressionExperimentDataFetchController.getDiffExpressionDataFile.dwr'),
            glue('callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={eeID}
httpSessionId=
scriptSessionId=1
c0-scriptName=ExpressionExperimentDataFetchController
c0-methodName=getDiffExpressionDataFile
c0-id=0
c0-param0=number:{rsID}
batchId=4
'))$then(function(response) {
  if(response$status != 200) {
    warning(paste0('Unable to get differential expression data file: ', response$status))
    return(NULL)
  }

  # Receive a task ID to track
  taskID <- response$content %>% rawToChar %>% {
    gsub('.*remoteHandleCallback\\(([^\\)]+)\\).*', '\\1', .)
  } %>% strsplit(',') %>% .[[1]] %>% { gsub('\'|"', '', .) } %>% .[3]
  countN <- 5

  requery <- async(function(taskID, countN) {
    # Poll for completion
    http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/ProgressStatusService.getSubmittedTask.dwr'),
              glue('callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={eeID}
httpSessionId=
scriptSessionId=1
c0-scriptName=ProgressStatusService
c0-methodName=getSubmittedTask
c0-id=0
c0-param0=string:{taskID}
batchId={countN}'))$then(function(response) {
  if(response$status != 200) {
    warning(paste0('Unable to get task data: ', response$status))
    return(NULL)
  }

  # Detect completion
  if(response$content %>% rawToChar %>% {
    grepl('done:true', ., fixed = T)
  }) {
    # Query where to find the result
    http_post(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/'), 'dwr/call/plaincall/TaskCompletionController.checkResult.dwr'),
              glue('callCount=1
page=/expressionExperiment/showExpressionExperiment.html?id={eeID}
httpSessionId=
scriptSessionId=1
c0-scriptName=TaskCompletionController
c0-methodName=checkResult
c0-id=0
c0-param0=string:{taskID}
batchId={countN + 1}
'))$then(function(response) {
  if(response$status != 200) {
    warning(paste0('Unable to get task result: ', response$status))
    return(NULL)
  }

  # Extract the file location
  response$content %>% rawToChar %>% {
    gsub('.*remoteHandleCallback\\(([^\\)]+)\\).*', '\\1', .)
  } %>% strsplit(',') %>% .[[1]] %>% { gsub('\'|"', '', .) } %>% .[3] %>% {
    # Make a temp file to unzip to
    tmp <- tempfile()
    http_get(paste0(getOption('gemma.base', 'https://gemma.msl.ubc.ca/') %>% substring(1, nchar(.) - 1), .), file = tmp)$then(function(...) {
      filenames <- unzip(tmp, list = TRUE)$Name

      # Unzip results and fread in
      ret <- lapply(filenames, function(x) fread(cmd = glue('unzip -p {tmp} {x}'),
                                                 colClasses = c(Element_Name = 'character', Gene_Symbol = 'character', Gene_Name = 'character'))) %>%
        setNames(filenames)
      unlink(tmp)
      ret
    })
  }
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