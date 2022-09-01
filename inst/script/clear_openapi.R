auto_gen_files = readLines('inst/script/auto_gen_code/.openapi-generator/FILES')
R_files = auto_gen_files[grepl('^R/',auto_gen_files)]
unlink('inst/script/auto_gen_code/',recursive = TRUE)
R_files %>% sapply(function(x){
    unlink(x)
})