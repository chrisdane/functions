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
            message(indent, "****** load_package() ******")
            message(indent, "Could not load '", packagename, "' package from")
            message(indent, "   .libPaths=", ifelse(length(.libPaths()) > 1, "c('", "'"),
                         paste0(.libPaths(), collapse="','"), 
                         ifelse(length(.libPaths()) > 1, "')", "'"), ".")
            message(indent, "If ", packagename, " is installed somewhere else, you can add library paths by providing")
            message(indent, "   rpackagepaths='/path/to/installed/libraries/'")
            message(indent, "or")
            message(indent, "   rpackagepaths=c('/path1/to/packages/', 'path2/to/packages/')")
            message(indent, "to to the runscript and rerun the script")
            message(indent, "or install the package know with")
            message(indent, "   install.packages('", packagename, "')")
            message(indent, "or")
            message(indent, "   install.packagse('", packagename, "', lib=/where/the/package/should/be/installed')")
            message(indent, "****************************")
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
