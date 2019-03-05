## R
update.check <- function() {
    inst <- packageStatus()
    inds <- which(inst$inst$Status != "ok") # ok upgrade unavailable
    if (length(inds) > 0) {
        message("These packages need an update:")
        print(inst$inst[inds,c("Package", "Version", "Status", "LibPath")])
        message("Run update.packages(instlib=.libPaths()[1], ask=F, checkBuild=T) now?")
        yn <- askYesNo("")
        if (yn) {
            update.packages(instlib=.libPaths()[1], ask=F, checkBuild=T)
        }
    }
    #return(inst)
}
