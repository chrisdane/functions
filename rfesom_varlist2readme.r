#!/usr/bin/env Rscript

#rfesom_varlist2readme <- function(namelist.var_fname, readme) {
    
args <- commandArgs(trailingOnly=T)
namelist.var_fname <- args[1]
readme <- args[2]

    message("update ", readme, " based on ", namelist.var_fname, " ...")

    # read rfesom variable namelist entries
    namelist <- readLines(namelist.var_fname)
    namelist <- namelist[regexpr("longname <- ", namelist) != -1]
    # remove first entry
    namelist <- namelist[-1]
    namelist <- substr(namelist, 
                       start=attributes(regexpr("longname <- ", namelist))$match.length + 
                             regexpr("longname <- ", namelist), 
                       stop=nchar(namelist))
    # replace ""
    namelist <- gsub("\"", "", namelist)
    
    # append varname list to rfesom readme
    system(paste0("echo '## Available variables' >> ", readme))
    system(paste0("echo >> ", readme))
    system(paste0("echo '(automated list based on ", basename(namelist.var_fname), ")' >> ", readme))
    system(paste0("echo >> ", readme))
    system(paste0("echo '| Variable name                                     |' >> ", readme))
    system(paste0("echo '|---------------------------------------------------|' >> ", readme))
    for (vari in namelist) {
        system(paste0("echo '| ", vari, " |' >> ", readme))
    }
    system(paste0("echo >> ", readme))

#} # rfesom_varlist2readme 
