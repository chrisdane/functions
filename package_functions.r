# r

# load function for packages
load_packages <- function(pkgs) {
    status <- sapply(pkgs, function(pkg) {
                     if (!any(search() == paste0("package:", pkg))) {
                         library(pkg, character.only=T, logical.return=T)
                     } else {
                         message("package '", pkg, "' already loaded.")
                     }})
    if (length(unlist(status)) > 0) stop()
} # load_packages

my_install.packages <- function(pkg="asdasd", dry=T) {
   
    # notes
    # - do not use LD_LIBRARY_PATH for package installation
    # - same compiler must be used that was used for building R itself
    # - install.packages("...", INSTALL_opts="--no-test-load")

    # defaults
    lib <- .libPaths()[1]
    configure.args <- getOption("configure.args")
    configure.vars <- getOption("configure.vars")
    LD_LIBRARY_PATH <- NULL # recommended to keep NULL

    # get hostname
    hostname <- Sys.info()["nodename"]
    
    # get version of this r binary
    rversion <- paste0(version["major"], ".", version["minor"]) # e.g. "3.6.1"

    # get shared lib paths of this r binary
    rbin <- paste0(R.home(), "/bin/exec/R")
    cmd <- paste0("ldd ", rbin, " | awk 'NF == 4 {print $3}; NF == 2 {print $1}'")
    r_shared_lib_paths <- system(cmd, intern=T) # e.g. "/sw/spack-rhel6/r-3.6.1-lnvx6h/rlib/R/lib/libR.so"
    r_shared_lib_paths <- sort(unique(dirname(r_shared_lib_paths)))
    if (any(r_shared_lib_paths == ".")) r_shared_lib_paths <- r_shared_lib_paths[-which(r_shared_lib_paths == ".")] # remove .

    # which package
    if (pkg == "units") {
        
        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "3.6") {
            message(pkg, " package defined on ", hostname, " for r 3.6")
            udunits2_path <- "/sw/spack-rhel6/udunits2-2.2.24-74lq3k"
            configure.args <- paste0("--with-udunits2-include=", udunits2_path, "/include ",
                                     "--with-udunits2-lib=", udunits2_path, "/lib")
            configure.vars <- paste0("LIBS=-Wl,-rpath,", udunits2_path, "/lib")
        
        } else if (grepl("levante", hostname)) {
            message(pkg, " package defined on ", hostname)
            udunits2_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
            configure.args <- paste0("--with-udunits2-include=", udunits2_path, "/include ",
                                     "--with-udunits2-lib=", udunits2_path, "/lib")
            configure.vars <- paste0("LIBS=-Wl,-rpath,", udunits2_path, "/lib")
            
        }
    
    } else if (pkg == "RNetCDF") {
            
        # default install call yields:
        # checking netcdf linker flags... -L/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z/lib -lnetcdf
        # libnetcdf.so.19: cannot open shared object file: No such file or directory
        #install.packages("RNetCDF", configure.args="--libdir=...) # does not work
        
        if (grepl("levante", hostname)) {
            message(pkg, " package defined on ", hostname)
            netcdf_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
            configure.vars <- paste0("LIBS=-Wl,-rpath,", netcdf_path, "/lib")
        }
    
    } else if (pkg == "sf") {

        # Optional Packages:
        #  --with-PACKAGE[=ARG]             use PACKAGE [ARG=yes]
        #  --without-PACKAGE                do not use PACKAGE (same as --with-PACKAGE=no)
        #  --with-gdal-config=GDAL_CONFIG   the location of gdal-config
        #  --with-data-copy=yes/no          local copy of data directories in package, default no
        #  --with-proj-data=DIR             location of PROJ data directory
        #  --with-sqlite3-lib=LIB_PATH      the location of sqlite3 libraries
        #  --with-proj-include=DIR          location of proj header files
        #  --with-proj-api=yes/no           use the deprecated proj_api.h even when PROJ 6 is available; default no
        #  --with-proj-lib=LIB_PATH         the location of proj libraries
        #  --with-proj-share=SHARE_PATH     the location of proj metadata files
        #  --with-geos-config=GEOS_CONFIG   the location of geos-config
        
        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "3.6") {
            message(pkg, " package defined on ", hostname, " for r 3.6")
            gdal_path <- "/sw/spack-rhel6/gdal-3.1.3-f7koyc"
            proj_path <- "/sw/spack-rhel6/proj-7.1.0-w57onb"
            geos_path <- "/sw/spack-rhel6/geos-3.8.1-ru2zkr"
            configure.args <- paste0("--with-gdal-config=", gdal_path, "/bin/gdal-config ",
                                     "--with-proj-include=", proj_path, "/include ",
                                     "--with-proj-lib=", proj_path, "/lib ",
                                     "--with-geos-config=", geos_path, "/bin/geos-config")
            stop("todo")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", udunits2_path, "/lib")
        
        } else if (grepl("levante", hostname)) {
            message(pkg, " package defined on ", hostname)
            if (T) { # combination 1
                # gdal 3.4.0
                gdal_path <- "/sw/spack-levante/gdal-3.4.0-ivy5wt" 
                # linked against /sw/spack-levante/proj-8.1.0-i6a6ah/lib/libproj.so.22
                #                /sw/spack-levante/sqlite-3.36.0-qxobjy/lib/libsqlite3.so.0
                # proj 8.1.0
                proj_path <- "/sw/spack-levante/proj-8.1.0-i6a6ah"
                # linked against /sw/spack-levante/sqlite-3.36.0-qxobjy/lib/libsqlite3.so.0
                # problem: cant find proj_api.h; is not in proj_path
            } else if (F) { # combination 2
                # gdal 3.4.0
                gdal_path <- "/sw/spack-levante/gdal-3.4.0-ivy5wt" 
                # linked against /sw/spack-levante/proj-8.1.0-i6a6ah/lib/libproj.so.22
                #                /sw/spack-levante/sqlite-3.36.0-qxobjy/lib/libsqlite3.so.0
                # proj 5.2.0
                proj_path <- "/sw/spack-levante/proj-5.2.0-w7auht"
                # problem: cant find libproj.so.13; is in proj_path
                projlib_path <- "/sw/spack-levante/proj-8.1.0-i6a6ah"
                projheader_path <- "/sw/spack-levante/proj-8.1.0-i6a6ah"
            } else if (F) {
                # gdal 3.2.3
                gdal_path <- "/sw/spack-levante/gdal-3.2.3-7adjaz" 
                # linked against /sw/spack-levante/proj-8.1.0-i6a6ah/lib/libproj.so.22
                #                /sw/spack-levante/sqlite-3.36.0-qxobjy/lib/libsqlite3.so.0
            }
            # geos 3.9.1
            geos_path <- "/sw/spack-levante/geos-3.9.1-6w4536"
            # sqlite3 3.376.0
            sqlite3_path <- "/sw/spack-levante/sqlite-3.36.0-qxobjy"
            #sqlite3_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
            #gdallib_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
            #projlib_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
            #projapi_path <- "/sw/spack-levante/proj-5.2.0-w7auht/include" # proj_api.h
            configure.args <- paste0("--with-gdal-config=", gdal_path, "/bin/gdal-config ",
                                     "--with-proj-include=", proj_path, "/include ",
                                     #"--with-proj-include=", projheader_path, "/include ",
                                     "--with-proj-lib=", proj_path, "/lib ",
                                     #"--with-proj-lib=", projlib_path, "/lib ",
                                     "--with-proj-api=no ", 
                                     "--with-geos-config=", geos_path, "/bin/geos-config ",
                                     "--with-sqlite3-lib=", sqlite3_path, "/lib")
            configure.vars <- paste0("LIBS=-Wl,-rpath,", gdal_path, "/lib")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", gdal_path, "/lib,-rpath,", proj_path, "/lib")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", gdallib_path, "/lib")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", proj_path, "/lib")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", projlib_path, "/lib")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", proj_path, "/lib,-rpath,", libproj_path, "/lib")
            # env variable `PROJ_LIB` must, if set, point to path of proj.db
            #proj.db_path <- paste0(proj_path, "/share/proj")
            #proj.db_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z/share/proj"
            #Sys.setenv(PROJ_LIB=proj.db_path)
            #Sys.getenv("PROJ_LIB")
            Sys.unsetenv("PROJ_LIB")
        }
   
    } else if (pkg == "XML") {

        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "4.1") {
            message(pkg, " package defined on ", hostname)
            libxml2_path <- "/sw/spack-rhel6/miniforge3-4.9.2-3-Linux-x86_64-pwdbqi"
            configure.vars <- paste0("LIBS=-Wl,-rpath,", libxml2_path, "/lib")
        }

    } # which package
    
    # checks
    if (!is.character(lib)) stop("`lib` must be of type character")
    if (!is.null(configure.args)) if (!is.character(configure.args)) stop("`configure.args` must be of type character")
    if (!is.null(configure.vars)) if (!is.character(configure.vars)) stop("`configure.vars` must be of type character")

    # add user LD_LIBRARY_PATH if set; not recommended
    LD_LIBRARY_PATH_old <- Sys.getenv("LD_LIBRARY_PATH")
    if (!is.null(LD_LIBRARY_PATH)) {
        if (!is.character(LD_LIBRARY_PATH)) stop("provided `LD_LIBRARY_PATH` is not of type character")
        warning("add provided LD_LIBRARY_PATH =\n   \"", LD_LIBRARY_PATH, "\"\n",
                "to current LD_LIBRARY_PATH =\n   \"", 
                LD_LIBRARY_PATH_old, "\"\nThis is not recommended!")
        LD_LIBRARY_PATH_new <- strsplit(LD_LIBRARY_PATH, ":")[[1]]
        LD_LIBRARY_PATH_new <- normalizePath(LD_LIBRARY_PATH_new) # yields warning if not existing
        LD_LIBRARY_PATH_new <- paste(LD_LIBRARY_PATH_new, collapse=":")
        Sys.setenv(LD_LIBRARY_PATH=paste0(LD_LIBRARY_PATH_new, ":", LD_LIBRARY_PATH_old))
    }

    # remove possible LD_LIBRARY_PATH duplicates and sort alphabetically
    LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH")
    LD_LIBRARY_PATH <- sort(unique(strsplit(LD_LIBRARY_PATH, ":")[[1]]))
    LD_LIBRARY_PATH <- paste(LD_LIBRARY_PATH, collapse=":")
    Sys.setenv(LD_LIBRARY_PATH=LD_LIBRARY_PATH)
    
    # get final LD_LIBRARY_PATH
    LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH")
    
    # verbose 
    msg <- paste0("install package \"", pkg, "\" on ", hostname, "\n",
                  "to path \"", lib, "\"\n",
                  "with ", R.version.string, "\n",
                  "R binary = ", rbin, "\n",
                  "shared lib paths of this binary (ldd <binary>) =\n",
                  paste(paste0("   ", r_shared_lib_paths), collapse="\n"), "\n",
                  "configure.args =")
    if (!is.null(configure.args)) {
        msg <- paste0(msg, "\n", paste(paste0("   ", strsplit(configure.args, " ")[[1]]), collapse="\n"))
    } else {
        msg <- paste0(msg, " NULL")
    }
    msg <- paste0(msg, "\n",
                  "configure.vars =")
    if (!is.null(configure.vars)) {
        msg <- paste0(msg, "\n", paste(paste0("   ", strsplit(configure.vars, " ")[[1]]), collapse="\n"))
    } else {
        msg <- paste0(msg, " NULL")
    }
    msg <- paste0(msg, "\n",
                  "LD_LIBRARY_PATH =\n", paste(paste0("   ", strsplit(LD_LIBRARY_PATH, ":")[[1]]), collapse="\n"))
    message(msg)

    # construct install cmd
    cmd <- paste0("utils::install.packages(pkgs=\"", pkg, "\", ",
                  "lib=\"", lib, "\", ",
                  "configure.args=", capture.output(dput(configure.args)), ", ",
                  "configure.vars=", capture.output(dput(configure.vars)), ")")
    message("\nrun `", cmd, "` ...\n")
    
    # return list
    res <- list(pkg=pkg, lib=lib,
                configure.args=configure.args, configure.vars=configure.vars,
                LD_LIBRARY_PATH_old=LD_LIBRARY_PATH_old, LD_LIBRARY_PATH=LD_LIBRARY_PATH, 
                cmd=parse(text=cmd))
    
    # install package and check shared lib paths of installed package if not dry run
    if (dry) {
        message("`dry=T` --> dont do it")
    
    } else if (!dry) {
        utils::install.packages(pkg, lib=lib, configure.args=configure.args, configure.vars=configure.vars)
        
        # check shared lib paths of installed package
        # --> need to restore old LD_LIBRARY_PATH before if necessary
        Sys.setenv(LD_LIBRARY_PATH=LD_LIBRARY_PATH_old)
        shared_lib <- paste0(lib, "/", pkg, "/libs/", pkg, ".so")
        if (!file.exists(shared_lib)) {
            warning(pkg, " lib ", shared_lib, " does not exist. installation went wrong")
        } else {
            cmd <- paste0("ldd ", shared_lib)
            message("\nrun `", cmd, "` ...")
            pkg_shared_lib_paths <- system(cmd, intern=T) # e.g. "/sw/spack-rhel6/r-3.6.1-lnvx6h/rlib/R/lib/libR.so"
            if (any(grepl(" => not found", pkg_shared_lib_paths))) {
                warning("--> some shared lib paths were not found:\n",
                        paste(pkg_shared_lib_paths[which(grepl(" => not found", pkg_shared_lib_paths))], collapse="\n"))
            } else {
                message("--> ok, all shared libs exist")
            }
        }

    } # if dry or not
    
    # return summary list
    message("\nfinished\n")
    return(res) # install call
    
} # my_install.packages function


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

# old
load_package <- function(packagename, indent="   ") { # used by rfesom
        
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

