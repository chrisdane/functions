load_package <- function(packagename, indent="   ") {
        
    if (!exists("verbose")) verbose <- 1

    # need to load package
    if (!any(search() == paste0("package:", packagename))) {
        if (verbose > 0) {
            message(paste0(indent, "Load '", packagename, "' package ..."))
        }
        suppressMessages(suppressWarnings(require(packagename, character.only=T)))
        #tc <- tryCatch(suppressMessages(suppressWarnings(library(i, character.only=T))),
        #               error=function(e) e, warning=function(w) w)
 
        # no success
        if (!any(search() == paste0("package:", packagename))) {
            message(paste0(indent, "****** load_package() ******"))
            message(paste0(indent, "Could not load '", packagename, "' package from"))
            message(paste0(indent, "   .libPaths=", ifelse(length(.libPaths()) > 1, "c('", "'"),
                         paste0(.libPaths(), collapse="','"), ifelse(length(.libPaths()) > 1, "')", "'"), "."))
            message(paste0(indent, "You can add library paths by providing"))
            message(paste0(indent, "   rpackagepaths='/path/to/installed/libraries/' or"))
            message(paste0(indent, "   rpackagepaths=c('/path1/to/packages/', 'path2/to/packages/')"))
            message(paste0(indent, "in namelist.rfesom.r or you can install the package with"))
            message(paste0(indent, "   install.packages('", packagename, "')"))
            message(paste0(indent, "****************************"))
            success <- F

        # success
        } else {
            if (verbose > 0) {
                #message(paste0(indent, "Done."))
            }
            success <- T
        }
    
    # package is already loaded
    } else {
        if (verbose > 0) {
            #message(paste0(indent, "Package '", packagename, "' is already loaded ..."))
        }
        success <- T
    }

} # load_package function
