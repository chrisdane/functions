# r

update.check <- function() {

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

} # update.check()

