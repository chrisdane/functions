#!/usr/bin/env Rscript

ol <- OlsonNames()
locs <- c("Australia/Melbourne")
# ol[which(regexpr("london", ol, ignore.case=T) != -1)]

now <- Sys.time()
here <- as.POSIXlt(now)
for (i in 1:length(locs)) {
    ind <- which(ol == locs[i])
    there <- as.POSIXlt(now, tz=ol[ind])
    message(locs[i], " (", there$zone, "): ", there, appendLF=F)
    dt <- (there$gmtoff - here$gmtoff)/3600 # s --> h
    if (dt == 0) { # same tz
    } else {
        if (dt < 0) { # there is earlier than here
            dt_text <- "earlier"
        } else if (dt > 0) { # there is later than here
            dt_text <- "later"
        }   
        message(" (", abs(dt), " hours ", dt_text, " than here)")
    }
}
