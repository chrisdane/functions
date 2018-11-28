progress_function <- function(n, i, indent="   ") {

    #for (i in 1:n) { # for testing
        if (i != n) {
            if (n >= 20 && i %% as.integer(n/10) == 0) {
                pcnt <- as.integer(round(i/n*100))
                if (pcnt == 10) {
                    cat(paste0(paste(rep(" ", e=nchar(options("prompt")$prompt)), collapse=""),
                               indent, round(i/n*100), "% "))
                #} else if (pcnt == 100) {
                #    cat(paste0(round(i/n*100), "%\n"))
                } else {
                    cat(paste0(round(i/n*100), "% "))
                }
            }
        } else if (i == n) {
        #    cat(paste0(round(i/n*100), "%\n"))
            cat("\n")
        }
    #}

} # progress function
