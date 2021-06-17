# r

update.check <- function() {
    libpaths <- .libPaths()
    yn <- rep(F, t=length(libpaths)) # default: dont update
    # first loop just show packages to upgrade
    for (libpathi in seq_along(libpaths)) {
        message("*************************************************\n",
                "libpath ", libpathi, "/", length(libpaths), ": ", libpaths[libpathi])
        if (file.access(libpaths[libpathi], mode=2) == -1) { # dir not writeable
            message("--> directory not writeable. skip")
        } else {
            inst <- packageStatus(lib=libpaths[libpathi])
            inds <- which(inst$inst$Status == "upgrade") # ok upgrade unavailable
            if (length(inds) > 0) {
                message("These ", length(inds), " packages need an update:")
                print(inst$inst[inds,c("Package", "Version", "Status")])
            }
            yn[libpathi] <- askYesNo(paste0("\nrun `utils::update.packages(lib=\"", libpaths[libpathi], 
                                            "\", instlib=\"", libpaths[libpathi], "\", ask=F, checkBuilt=T)`?"))
        }
    } # for libpathi
    if (any(is.na(yn))) stop("this should not happen")
    # second loop: actual update
    for (libpathi in seq_along(yn)) {
        if (yn[libpathi]) {
            message("\n***************************************************\n",
                    "run `utils::update.packages(lib=\"", libpaths[libpathi], "\", instlib=\"", 
                    libpaths[libpathi], "\", ask=F, checkBuilt=T)` ...\n")
            utils::update.packages(lib=libpaths[libpathi], instlib=libpaths[libpathi], ask=F, checkBuilt=T)
        }
    } # if any packages need update and update is wanted
    #return(inst)
} # update.check

