mylocator <- function(...) {

    # open plot with world map if no plot is already open 
    if (is.null(dev.list())) {
        library(maps)
        message("run maps::map(\"world\", interiod=F) ...")
        map("world", interior=F)
        message("par(\"usr\") = ", appendLF=F)
        dput(par("usr"))
    } 

    options(locatorBell=F) # turn off system beep
    message("To exit the locator, hit any mouse button but the first while the mouse is over the plot device ...")
    locator(type="o", ...)

} # mylocator
