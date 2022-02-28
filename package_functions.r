# r

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
                         paste0(.libPaths(), 
                                collapse=paste0("',\n", paste0(rep(" ", t=nchar(indent)), collapse=""), paste0(rep(" ", t=15), collapse=""), "'")),
                    ifelse(length(.libPaths()) > 1, "')", "'"), ".")
            message(indent, "If ", packagename, " is installed somewhere else, you can add library paths by providing")
            message(indent, "   rpackagepaths='/path/to/installed/packages'")
            message(indent, "or")
            message(indent, "   rpackagepaths=c('/path1/to/packages', 'path2/to/packages')")
            message(indent, "to to the runscript and rerun the script.")
            message(indent, "You can install the package now with")
            message(indent, "   install.packages('", packagename, "')")
            message(indent, "or")
            message(indent, "   install.packages('", packagename, "', lib=/where/the/package/should/be/installed')")
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


update_check <- function() {

    # update packages package-wise
    # -> `utils::update.packages()` can only check a whole directory and update all packages
    # -> a specific package can be updated by simply `utils::install.packages(pkgname)`

    libpaths <- .libPaths()
    
    # update only 
    # -> it works better to update packages with the r version according to the package location
    #libpaths <- libpaths[1] # this only applies to my file structure
    
    li <- vector("list", l=length(libpaths))
    # first loop just show packages to upgrade
    for (libpathi in seq_along(libpaths)) {
        message("*************************************************\n",
                "libpath ", libpathi, "/", length(libpaths), ": ", libpaths[libpathi])
        li[[libpathi]] <- NULL # default: no update needed/possible
        if (file.access(libpaths[libpathi], mode=2) == -1) { # dir not writeable
            message("--> directory not writeable. skip")
        } else {
            inst <- packageStatus(lib=libpaths[libpathi])
            inds <- which(inst$inst$Status == "upgrade") # ok upgrade unavailable
            if (length(inds) > 0) {
                df <- inst$inst[inds,c("Package", "Version", "Status")]
                numbers <- seq_along(df[,1])
                df <- cbind(No=numbers, df)
                print(df, row.names=F)
                excludes <- readline(prompt="\nExclude any packages? ([enter] for no exclusion or e.g. \"1\", \"1,3\", \"1,3,5\", \"1-3\", \"^4\", \"all\")")
                if (excludes == "") { # [enter]
                    excludes <- NULL
                } else if (grepl(",", excludes)) {
                    excludes <- strsplit(excludes, ",")[[1]]
                    excludes <- as.integer(excludes)
                } else if (grepl("-", excludes)) {
                    excludes <- strsplit(excludes, "-")[[1]]
                    excludes <- as.integer(excludes)
                    if (length(excludes) != 2) stop("\"", paste(excludes, collapse="-"), "\" invalid")
                    excludes <- excludes[1]:excludes[2]
                } else if (substr(excludes, 1, 1) == "^") {
                    excludes <- substr(excludes, 2, nchar(excludes))
                    excludes <- as.integer(excludes)
                    if (!(excludes %in% numbers)) stop("provided ^", exludes, " invalid")
                    excludes <- numbers[-excludes]
                } else if (excludes == "all") {
                    excludes <- numbers
                } else {
                    excludes <- as.integer(excludes)
                }
                if (length(excludes) > 0) {
                    if (any(!(excludes %in% numbers))) {
                        stop("provided ", paste(excludes, collapse=", "), " is not completely in ", 
                             paste(unique(range(numbers)), collapse="-"))
                    }
                    df <- df[-excludes,]
                }
                if (dim(df)[1] == 0) {
                    message("--> excluded all packages")
                } else {
                    li[[libpathi]] <- list(path=libpaths[libpathi],
                                           packages=df$Package)
                }
            } else {
                message("--> all up-to-date")
            } # if any package in current libpath needs update
        } # if current libpath is writeable or not
    } # for libpathi
    # second loop: actual update
    for (i in seq_along(li)) {
        if (!is.null(li[[i]])) {
            message("\n***************************************************\n",
                    "run `utils::install.packages(pkg=c(\"", paste(li[[i]]$packages, collapse="\", \""), 
                    "\"), lib=", li[[i]]$path, ")` ...\n")
            utils::install.packages(pkgs=li[[i]]$packages, lib=li[[i]]$path)
        }
    }

} # update_check()

remove_depends <- function(pkg, lib=.libPaths()[1], recursive=F) {
    if (missing(pkg)) stop("must provide at least one package")
    library("tools")
    d <- tools::package_dependencies(db=utils::installed.packages(lib=lib), recursive=recursive)
    depends <- if(!is.null(d[[pkg]])) d[[pkg]] else character()
    needed <- unique(unlist(d[!names(d) %in% c(pkg,depends)]))
    toRemove <- depends[!depends %in% needed]
    if (length(toRemove)) {
        toRemove <- c(pkg, sort(toRemove))
        if (!interactive()) {
            warning("this is a non-interactive session, will not remove packages\n",
                    paste(toRemove, collapse=", "))
        } else {
            toRemove <- utils::select.list(choices=toRemove,
                                           preselect=toRemove, # default: all
                                           multiple=T,
                                           title="Select packages to remove")
            utils::remove.packages(toRemove, lib=lib)
        }
        #return(toRemove) # if list is wanted
    } else {
        invisible(character())
    }
} # remove_depends

# check if an installed package cannot be loaded
broken_pkgs <- function(pkgs=NULL, lib=.libPaths()[1]) {
    if (is.null(pkgs)) {
        pkgs <- installed.packages(lib=lib)[,1]
    }
    checks <- list(); cnt <- 0
    message("check ", length(pkgs), " packages from ", lib, " ...")
    for (i in seq_along(pkgs)) {
        check <- tryCatch(ncol(asNamespace(pkgs[i])$.__NAMESPACE__.$S3methods),
                          error = function(e) c(pkgs[i], e))
        if (class(check) == "list") {
            cnt <- cnt + 1
            checks[[cnt]] <- check[2:3]
            names(checks)[cnt] <- pkgs[i]
        }
    }
    return(checks)
} # broken_pkgs

# check a package if any other package depends on that package or not
nodep_pkgs <- function(pkgs=NULL, lib=.libPaths()[1]) {
    if (is.null(pkgs)) {
        pkgs <- installed.packages(lib=lib)[,1]
    }
    nodeps <- c()
    message("check ", length(pkgs), " packages from ", lib, " ...")
    for (i in seq_along(pkgs)) {
        dep <- tools::dependsOnPkgs(pkgs[i], dependencies=c("Depends", "Imports", "LinkingTo"), lib=lib)
        if (length(dep) == 0) nodeps <- c(nodeps, pkgs[i])
    }
    return(unname(nodeps))
} # nodep_pkgs

