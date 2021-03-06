mytxtProgressBar <- function (min = 0, max = 1, initial = 0, char = "=", width = NA, 
    title, label, style = 1, file = "", indent = "") 
{
    if (!identical(file, "") && !(inherits(file, "connection") && 
        isOpen(file))) 
        stop("'file' must be \"\" or an open connection object")
    if (!style %in% 1L:3L) 
        style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (is.na(width)) {
        width <- getOption("width")
        if (style == 3L) 
            width <- width - 10L
        width <- trunc(width/nw)
    }
    if (max <= min) 
        stop("must have 'max' > 'min'")
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb < nb) {
            cat(strrep(char, nb - .nb), file = file)
            flush.console()
        }
        else if (.nb > nb) {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb <= nb) {
            cat("\r", strrep(char, nb), sep = "", file = file)
            flush.console()
        }
        else {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc) 
            return()
        # change
        #cat(paste0("\r  |", strrep(" ", nw * width + 6)), file = file)
        cat(paste0("\r", indent, "[", strrep("-", nw * width + 6)), file = file)
        #cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
        cat(paste(c("\r", indent, "[", rep.int(char, nb), rep.int("-",
            nw * (width - nb)), sprintf("] %3d%%", pc)), collapse = ""), 
            file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        cat("\n", file = file)
        flush.console()
        .killed <<- TRUE
    }
    up <- switch(style, up1, up2, up3)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
}

# for testing
if (F) {
    n <- 100
    pb <- mytxtProgressBar(min=0, max=n, style=3,
                            char="#", width=30,
                            indent="   ") # 5 " " for default print()
    n <- 100
    for (i in 1:n) {
        Sys.sleep(0.05)
        setTxtProgressBar(pb, i)
    }
    close(pb)
}
