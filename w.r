###!/bin/Rscript --vanilla

message("********** w.r **********")

rm(list=ls())

w <- system("w", intern=T)
header <- w[1]
colnames <- strsplit(w[2], "\\s+")[[1]]
w <- w[3:length(w)]

df <- lapply(w, function(x) strsplit(x, "\\s+")[[1]])

users <- sapply(df, "[", 1)
whats <- sapply(df, function(x) paste0(x[8:length(x)], collapse=" "))

users_unique <- unique(users)
li <- vector("list")
default_whats <- c("-bash", "bash", "tcsh")
li_cnt <- 0
for (i in 1:length(users_unique)) {
    inds <- which(users == users_unique[i])
    non_def_whats <- c()
    what_cnt <- 0
    for (whati in 1:length(inds)) {
        what <- whats[inds][whati] 
        if (!(what %in% default_whats)) {
            # user runs a non-default program
            what_cnt <- what_cnt + 1
            non_def_whats[what_cnt] <- whats[inds[whati]]
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

# alphabetical order
abcinds <- sort(sapply(li, "[[", "user"), index.return=T)$ix

# print
for (i in 1:length(li)) {
    message(" ", sprintf(paste0("%", nchar(length(li)), "i"), i), " ", li[[abcinds[i]]]$user)
    message(paste0(paste0("      ", li[[abcinds[i]]]$what), collapse="\n"))
}
message("all other processes are one of: ", paste0(default_whats, collapse=", "))
message("********** w.r **********")

