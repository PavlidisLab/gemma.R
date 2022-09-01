library(magrittr)
library(here)
setwd(here())

download.file('https://oss.sonatype.org/content/repositories/snapshots/org/openapitools/openapi-generator-cli/6.1.0-SNAPSHOT/openapi-generator-cli-6.1.0-20220829.044839-95.jar',destfile = 'inst/script/openapi-generator.jar')
download.file('https://dev.gemma.msl.ubc.ca/rest/v2/openapi.json','inst/script/openapi.json')

system('java -jar inst/script/openapi-generator.jar generate -i inst/script/openapi.json -g r --skip-validate-spec --additional-properties=operationIdNaming=snake_case,packageName=gemma.R -o inst/script/auto_gen_code')

auto_gen_files = readLines('inst/script/auto_gen_code/.openapi-generator/FILES')

R_files = auto_gen_files[grepl('^R/',auto_gen_files)]

for (f in R_files){
    file = readLines(glue::glue('inst/script/auto_gen_code/{f}'))
    
    file[grepl('@export',file)] %<>% gsub("#' @export","#' @keywords internal",.)
    file[grepl('[optional]',file,fixed = TRUE)] %<>% 
        gsub('[optional]','optional',x=.,fixed = TRUE)
    
    writeLines(file,f)
}

# remove forced authentication
default = readLines('R/default_api.R')
auth_code = grepl("# HTTP basic auth",default)
for (i in which(auth_code)){
    default[i+1] = default[i+1]  %>% 
        gsub('is.null','!is.null',.)
    default[i+2] = paste0("  ",default[i+4])
    default[i+4] = ""
}

writeLines(default,'R/default_api.R')

devtools::document()
