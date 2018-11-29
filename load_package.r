load_package <- function(packagename) {

    if (!any(search() == paste0("package:", packagename))) {
        if (verbose > 0) {
            print(paste0(indent, "Load '", packagename, "' package ..."))
        }
        suppressMessages(suppressWarnings(require(packagename, character.only=T)))
        if (!any(search() == paste0("package:", packagename))) {
            print(paste0(indent, "Warning: Could not load '", packagename, "' package from"))
            print(paste0(indent, "         .libPaths=", ifelse(length(.libPaths()) > 1, "c('", "'"),
                         paste0(.libPaths(), collapse="','"), ifelse(length(.libPaths()) > 1, "')", "'"), "."))
            print(paste0(indent, "         You can add library paths by providing"))
            print(paste0(indent, "            rpackagepaths='/path/to/installed/libraries/' or"))
            print(paste0(indent, "            rpackagepaths=c('/path1/to/packages/', 'path2/to/packages/')"))
            print(paste0(indent, "         in namelist.rfesom.r or you can install the package with"))
            print(paste0(indent, "            install.packages('", packagename, "')."))
            success <- F
        } else {
            if (verbose > 0) {
                #print(paste0(indent, "Done."))
            }
            success <- T
        }
    } else {
        if (verbose > 0) {
            #print(paste0(indent, "Package '", packagename, "' is already loaded ..."))
        }
        success <- T
    }

} # load_package function
