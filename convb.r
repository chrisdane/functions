## R
convb <- function(x, unit) {
   
    # convert human readable to bytes
    # https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r/49380514

    if (!is.finite(x) || !is.character(unit)) stop("x must be finite and unit must be character")
    unit[unit == ""] <- "1"
    mult <- c("1"=1,  "B"=1,  "K"=1024,  "M"=1024^2,  "G"=1024^3,  "T"=1024^4,  "P"=1024^5)
    x * unname(mult[unit])

} # convb function
