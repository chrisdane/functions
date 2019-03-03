mylocator <- function(...) {

    options(locatorBell=F) # turn off system beep
    message("To exit the locator, hit any mouse button but the first while the mouse is over the plot device ...")
    locator(type="o", ...)

} # mylocator
