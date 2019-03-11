## R
# https://stackoverflow.com/questions/25136059/how-to-show-working-directory-in-r-prompt

myRPrompt <- function(...) {

    verbose <- F
    p <- getwd() # absolute path as pwd
    if (verbose) message("getwd(): ", getwd())
    home <- regexpr(path.expand("~"), p)
    if (home != -1) { # remove home part
        p <- substr(p, start=home + attr(home, "match.length"), stop=nchar(p))
        if (p == "") p <- "/" # if in home itself (~/)
        if (verbose) message("home: ", p)
    }

    # shorten directory names: only one letter after every .Platform$file.sep except the last directory
    seps <- gregexpr(.Platform$file.sep, p)[[1]]
    if (length(seps) > 1) {
        if (verbose) message("seps: ", paste0(seps, collapse=", "))
        ps <- rep(NA, t=length(seps) - 1)
        for (i in 1:(length(seps) - 1)) { # keep the last (top) directory full name
            ps[i] <- substr(p, start=seps[i] + 1, stop=seps[i] + 1)
        }
        p <- paste0(.Platform$file.sep, paste0(ps, collapse=.Platform$file.sep), .Platform$file.sep, basename(p))
        if (verbose) message("seps p: ", p)
    }

	# add "~" if on home in front
    if (home != -1) p <- paste0("~", p)

    # add trailing slash if not in "/" or "~/"
    if (substr(p, nchar(p), nchar(p)) != "/") p <- paste0(p, "/")

    # add machine and trailing R>
    #p <- paste0(Sys.info()[4], ":", p, " R>")
    p <- paste0(Sys.info()[4], ":", p, ">")

    # apply color in bash style
    # unfortunately, this breaks arrow up/down behavior in R terminal:
    # https://github.com/jalvesaq/colorout/issues/7#issuecomment-207849620
    if (F) {
        if (F) { # bash style
            p <- paste0("\x1b[34m", p, "\x1b[0m") # blue
        } else if (T) { # # apply color from crayon package
            #library(crayon)
            if (any(search() == "package:crayon")) p <- crayon::blue(p)
        }
    }

    # attach trailing space
    p <- paste0(p, " ")

} # myRPrompt

# overwrite base::setwd to run my prompt on every dir change
setwd <- function(...) {
    base::setwd(...)
    options(prompt=myRPrompt())
}

