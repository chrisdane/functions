## R
update.check <- function() {
    inst <- packageStatus()
    inds <- which(inst$inst$Status == "upgrade") # ok upgrade unavailable
    if (length(inds) > 0) {
        message("These packages need an update:")
        print(inst$inst[inds,c("Package", "Version", "Status", "LibPath")])
        message("")
        yn <- menu(choices=c("yes", "no"), 
                   title="Run update.packages(instlib=.libPaths()[1], ask=F, checkBuilt=T) now?")
        if (yn == 1) {
            update.packages(instlib=.libPaths()[1], ask=F, checkBuilt=T)
        }
    } else {
        message("All packages uptodate.")
    }
    inds <- which(inst$inst$Status == "unavailable")
    if (length(inds) > 0) {
        message("However, these packages are unavailable:")
        print(inst$inst[inds,c("Package", "Version", "Status", "LibPath")])
    }

    #return(inst)
}
