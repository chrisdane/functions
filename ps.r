#!/bin/Rscript --vanilla

message("********** ps.r **********")

rm(list=ls())

ncol_ps <- 8000 # just a guess
ps <- system(paste0("ps au --cols ", ncol_ps), intern=T) # ps auf ('f'orest) also nice
colnames <- strsplit(ps[1], "\\s+")[[1]]
ps <- ps[2:length(ps)]
if (max(nchar(ps)) >= ncol_ps) {
    message("Note: the longest entry of 'ps' is >= 'ncol_ps' (", ncol_ps, "')")
}

df <- lapply(ps, function(x) strsplit(x, "\\s+")[[1]])

users <- sapply(df, "[", 1)
whats <- sapply(df, function(x) paste0(x[11:length(x)], collapse=" "))

users_unique <- unique(users)

# drop root
if (any(users_unique == "root")) {
    users_unique <- users_unique[-which(users_unique == "root")]
}

li <- vector("list")
shells <- c("bash", "tcsh", "csh", "zsh")
shells <- c(shells, paste0("bin/", shells), paste0("/bin/", shells))
shells <- c(shells, paste0("-", shells))
default_whats <- c("dbus-launch", "eog", "less", "vim", "emacs", 
                   "more", "ncview", "gedit", "evince")
default_whats <- paste0(default_whats, " ")
default_whats <- c(default_whats, "more", "less") # why there are 'more' and 'less' processes without args?
li_cnt <- 0
for (i in 1:length(users_unique)) {
    inds <- which(users == users_unique[i])
    non_def_whats <- c()
    what_cnt <- 0
    for (whati in 1:length(inds)) {
        what <- whats[inds][whati]
        if (!(what %in% shells) &&
            all(sapply(default_whats, regexpr, what) == -1)) {
            # user runs a non-default program
            what_cnt <- what_cnt + 1
            non_def_whats[what_cnt] <- what
        }
    }
    if (length(non_def_whats) > 0) {
        li_cnt <- li_cnt + 1
        tmp <- vector("list", l=2)
        names(tmp) <- c("user", "what")
        tmp[[1]] <- users_unique[i]
        tmp[[2]] <- non_def_whats
        li[[li_cnt]] <- tmp
    }
}

if (length(li) > 0) { # if any non-default prog

    # alphabetical order
    abcinds <- sort(sapply(li, "[[", "user"), index.return=T)$ix

    # print
    for (i in 1:length(li)) {
        message(" ", sprintf(paste0("%", nchar(length(li)), "i"), i), " ", li[[abcinds[i]]]$user)
        message(paste0(paste0("      ", li[[abcinds[i]]]$what), collapse="\n"))
    }
}
message("all", ifelse(length(li) > 0, " other ", " "), 
        "other processes are one of: ", paste0(shells, collapse=","), 
        " (leading dash - for login shell) or contain any of '", 
        paste0(default_whats, collapse="','"), "'")
message("********** ps.r **********")

