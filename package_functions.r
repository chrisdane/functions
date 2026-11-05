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

        } else if (grepl("albedo|^prod-[0-9]+$", hostname)) {
            message(pkg, " package defined on ", hostname)
            # the `netcdf-c` *module* only prepends PATH/PKG_CONFIG_PATH/CMAKE_PREFIX_PATH,
            # not LD_LIBRARY_PATH -- so "libnetcdf.so.19: cannot open shared object file"
            # at `dyn.load()` time even though the install itself succeeds; confirm the
            # path via `nc-config --libdir` if this hash ever changes
            netcdf_path <- "/albedo/soft/sw/spack-sw/netcdf-c/4.8.1-wucmueg"
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

    } else if (pkg == "terra") {

        # notes (albedo, confirmed by testing 2026-10-26; re-derive paths below if
        # any of these spack package hashes change):
        # - `r/4.2.2_conda`'s toolchain is ABI-incompatible with the only GDAL on
        #   the system (`gdal/3.4.3`, built with spack's own compiler/glibc/OpenSSL)
        #   -- this function can't fix that one (it can't change which R binary is
        #   already running): switch to the plain `r/4.2.2` (spack) module instead
        #   (`module load r/4.2.2`), not `r/4.2.2_conda`, *before* starting R
        # - everything else below this function sets up itself (PATH for
        #   `gdal-config`/`geos-config`, `CC`, `CPPFLAGS`, `LIBRARY_PATH`) --
        #   confirmed by testing that *not* doing so (e.g. only relying on the
        #   caller's shell having `module load gdal/3.4.3 gcc/12.1.0` already)
        #   fails with "gdal-config not found" as soon as that assumption breaks
        # - the system libstdc++ (`/usr/lib64/libstdc++.so.6.0.25`) lacks
        #   GLIBCXX_3.4.29/3.4.30, needed by libgdal.so/libgeos.so/libproj.so (all
        #   built with a newer spack gcc) -- hence `CC` pinned to gcc 12.1.0's own
        #   `bin/gcc` below (its `lib64/libstdc++.so.6` has the needed symbol
        #   versions); `CXX` is not pinned (terra's configure calls `R CMD config
        #   CXX`, which stays `/usr/bin/g++` regardless -- did not matter in
        #   practice once the right libstdc++/libs were reachable some other way)
        # - missing `-ljson-c`/`-liconv`/pnetcdf-symbol/libstdc++-symbol errors at
        #   link time are all fixed at once by reusing `libgdal.so`'s own full
        #   dependency chain (`readelf -d <gdal lib>/libgdal.so | grep RPATH`) as
        #   `LIBRARY_PATH` (an env var gcc/ld already consult for implicit `-L`
        #   dirs), rather than chasing 1 missing symbol at a time. NOT via
        #   `configure.vars`'s `LIBS=` for this part: confirmed by testing that
        #   `R CMD INSTALL --configure-vars=...` hands it to a shell as
        #   `LIBS=<value> ./configure`, which parses "VAR=val arg1 arg2 ..." as
        #   "VAR=val" applying only to running "arg1" as the *command* -- fatal
        #   as soon as `LIBS=`'s value has any of its own unquoted spaces (and
        #   wrapping it in `'...'` ourselves collides with R's own quoting:
        #   "unexpected EOF" instead) -- `configure.vars` below is only used for
        #   `-rpath`, 1 colon-joined value with no spaces
        # - `gdal-config --cflags` doesn't itself include PROJ's own include dir
        #   -> `proj.h`/PROJ version check fails without `CPPFLAGS` *and*
        #   `--with-proj-include=`/`--with-proj-data=` (the former alone is not
        #   enough -- terra's own `proj_conf_test.c` version check compiles with
        #   `${CC} ${CFLAGS} ${PROJ_CPPFLAGS}`, not `${CPPFLAGS}`)
        # - `geos-config` is not on PATH via any module -- add its `bin/` directly
        # - the R library's own bundled `Rcpp` (1.0.10) cannot compile terra's
        #   current generated C++ (an `Rcpp::class_<...>::constructor<...>()` call
        #   with more template arguments than that Rcpp version's module
        #   generator supports) -- install a newer `Rcpp` (fixed: 1.1.2 worked)
        #   into a writable, earlier `.libPaths()` entry *first*
        # - once installed, EVERY future session needs `LD_LIBRARY_PATH` set
        #   in the *shell*, before R starts (see the long comment further
        #   below for why `Sys.setenv()` from inside R cannot do this) --
        #   `source ~/scripts/r/functions/load_terra_env.sh && R`
        if (grepl("albedo|^prod-[0-9]+$", hostname)) {
            message(pkg, " package defined on ", hostname)
            gdal_path <- "/albedo/soft/sw/spack-sw/gdal/3.4.3-sdthrwf"
            proj_path <- "/albedo/soft/sw/spack-sw/proj/8.2.1-2ndneg5"
            geos_path <- "/albedo/soft/sw/spack-sw/geos/3.9.1-rasz7sl"
            gcc_path <- "/albedo/soft/sw/spack-sw/gcc/12.1.0-aosheo5"
            # set up PATH/CC ourselves -- don't rely on the caller having done
            # `module load gdal/3.4.3 gcc/12.1.0` beforehand (confirmed by
            # testing: without this, "gdal-config not found" even though
            # everything else below is set correctly)
            Sys.setenv(PATH=paste0(gdal_path, "/bin:", gcc_path, "/bin:", geos_path, "/bin:", Sys.getenv("PATH")))
            Sys.setenv(CC=paste0(gcc_path, "/bin/gcc"))
            # libgdal.so's own RPATH (`readelf -d .../libgdal.so | grep RPATH`), in order
            gdal_rpath <- c(paste0(gcc_path, "/lib64"),
                            paste0(gdal_path, "/lib"), paste0(gdal_path, "/lib64"),
                            "/albedo/soft/sw/spack-sw/openjpeg/2.3.1-hufbzie/lib",
                            "/albedo/soft/sw/spack-sw/netcdf-c/4.8.1-ymqhh7u/lib",
                            "/albedo/soft/sw/spack-sw/hdf/4.2.15-tvforuz/lib",
                            "/albedo/soft/sw/spack-sw/libtirpc/1.2.6-7ryxcju/lib",
                            "/albedo/soft/sw/spack-sw/krb5/1.19.3-yas3mzz/lib",
                            "/albedo/soft/sw/spack-sw/libgeotiff/1.6.0-6dp6qwe/lib",
                            paste0(proj_path, "/lib"),
                            "/albedo/soft/sw/spack-sw/sqlite/3.38.5-6qjkd2h/lib",
                            "/albedo/soft/sw/spack-sw/sqlite/3.38.5-7b3wslx/lib",
                            "/albedo/soft/sw/spack-sw/googletest/1.10.0-udi7kz7/lib64",
                            "/albedo/soft/sw/spack-sw/libtiff/4.3.0-hvksrq3/lib",
                            "/albedo/soft/sw/spack-sw/zstd/1.5.2-z4w3egu/lib",
                            "/albedo/soft/sw/spack-sw/libjpeg-turbo/2.1.3-ut4wrxn/lib64",
                            "/albedo/soft/sw/spack-sw/json-c/0.15-s3qnuwz/lib64",
                            "/albedo/soft/sw/spack-sw/hdf5/1.12.2-5azmciz/lib",
                            "/albedo/soft/sw/spack-sw/libaec/1.0.6-7uqktkj/lib64",
                            "/albedo/soft/sw/spack-sw/giflib/5.2.1-vzzxgc6/lib",
                            paste0(geos_path, "/lib"),
                            "/albedo/soft/sw/spack-sw/gettext/0.21-ktexb4a/lib",
                            "/albedo/soft/sw/spack-sw/xz/5.2.5-ugfcym7/lib",
                            "/albedo/soft/sw/spack-sw/libiconv/1.16-tv5pua3/lib",
                            "/albedo/soft/sw/spack-sw/readline/8.1-nangkhx/lib",
                            "/albedo/soft/sw/spack-sw/ncurses/6.2-5y2nqxd/lib",
                            "/albedo/soft/sw/spack-sw/bzip2/1.0.8-hkoj4zp/lib",
                            "/albedo/soft/sw/spack-sw/expat/2.4.8-wtvbl7r/lib",
                            "/albedo/soft/sw/spack-sw/libbsd/0.11.5-ees63ob/lib",
                            "/albedo/soft/sw/spack-sw/libmd/1.0.4-j2twx6a/lib",
                            "/albedo/soft/sw/spack-sw/curl/7.83.0-2xe5wbk/lib",
                            "/albedo/soft/sw/spack-sw/zlib/1.2.12-rfttgpq/lib")
            # extra `-L` dirs (fixes link-time "cannot find -l...") via
            # `LIBRARY_PATH` (an env var gcc/ld already consult for implicit
            # `-L` dirs -- safe with many colon-joined paths, no embedded
            # spaces) rather than `configure.vars`'s `LIBS=`: that gets handed
            # to a shell as `LIBS=<value> ./configure`, which parses
            # "VAR=val arg1 arg2 ..." as "VAR=val" applying only to running
            # "arg1" as the *command* -- confirmed by testing (`sh -c
            # 'FOO=-La -Lb echo hi'` runs "-Lb" as a command: "-Lb: command
            # not found") -- fatal as soon as `LIBS=`'s value has 1 of its own
            # *unquoted* spaces, and R's own `--configure-vars=...` quoting
            # does not protect a value that itself needs internal quotes
            # (tried wrapping in `'...'` ourselves too -- collides with R's
            # own outer quoting, "unexpected EOF" instead). `configure.vars`
            # below is left for just `-rpath` -- 1 colon-joined value, no
            # spaces. `LD_LIBRARY_PATH` is *also* set here (same paths as
            # `LIBRARY_PATH`), needed for R's own post-build "testing if
            # installed package can be loaded" step to succeed (confirmed by
            # testing: without it, that step's `dyn.load()` fails with
            # "libgdal.so.30: cannot open shared object file" even though the
            # actual compile+link just succeeded and `-Wl,-rpath,...` is right
            # there in the link command).
            #
            # IMPORTANT: this `Sys.setenv()` only helps *within this same R
            # process* -- e.g. it reaches the compiler and the test-load
            # because `R CMD INSTALL` spawns those as fresh child processes,
            # which inherit the env at their own startup. It does NOT mean a
            # later, *separate* `library(terra)` call in a brand-new R session
            # will work: glibc's dynamic loader parses `LD_LIBRARY_PATH` once
            # at process startup (before `main()` runs) and does not re-read
            # it later just because `Sys.setenv()` changed it mid-session --
            # confirmed by testing (`Sys.setenv(LD_LIBRARY_PATH=...)` then
            # `library(terra)` in 1 session: still "cannot open shared object
            # file"). So `-Wl,-rpath,...` genuinely isn't being honored by
            # this particular build for some reason (not investigated further
            # -- diminishing returns), and every *future* session needs
            # `LD_LIBRARY_PATH` set in the *shell*, before R starts --
            # `source ~/scripts/r/functions/load_terra_env.sh && R` does this
            Sys.setenv(LIBRARY_PATH=paste0(paste(gdal_rpath, collapse=":"), ":", Sys.getenv("LIBRARY_PATH")))
            Sys.setenv(LD_LIBRARY_PATH=paste0(paste(gdal_rpath, collapse=":"), ":", Sys.getenv("LD_LIBRARY_PATH")))
            configure.vars <- paste0("LIBS=-Wl,-rpath,", paste(gdal_rpath, collapse=":"))
            configure.args <- paste0("--with-proj-include=", proj_path, "/include ",
                                     "--with-proj-data=", proj_path, "/share/proj")
            Sys.setenv(CPPFLAGS=paste0("-I", proj_path, "/include"))
            rcpp_version <- tryCatch(utils::packageVersion("Rcpp"), error=function(e) NA)
            if (is.na(rcpp_version) || rcpp_version < "1.1.2") {
                message("--> installed `Rcpp` (", rcpp_version, ") is too old to compile terra's current C++ -- ",
                        "run `install.packages(\"Rcpp\")` first (fixed working version: 1.1.2), ",
                        "into a writable .libPaths() entry earlier than the R-bundled one")
            }
        }

    # `spam`/`Rcpp` needed no special `configure.args`/`configure.vars` here (a
    # plain `install.packages()` sufficed, after switching to the right R module)
    # -- only mentioned as a reminder that a package's compiled `.so` is tied to
    # whichever R/toolchain built it: switching R modules (e.g. away from
    # `r/4.2.2_conda` to `r/4.2.2`) can surface "unable to load shared object"
    # errors for *already-installed* packages whose `.so` was linked against a
    # now-different (or no longer existing) toolchain/library -- the fix is just
    # reinstalling that package fresh under the currently active R module

    } else if (pkg == "XML") {

        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "4.1") {
            message(pkg, " package defined on ", hostname)
            libxml2_path <- "/sw/spack-rhel6/miniforge3-4.9.2-3-Linux-x86_64-pwdbqi"
            configure.vars <- paste0("LIBS=-Wl,-rpath,", libxml2_path, "/lib")
        } else if (any(sapply(c("levante", "lvt"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "4.1") {
            message(pkg, " package defined on ", hostname)
            libxml2_path <- "/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z"
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

        if (pkg == "terra" && exists("gdal_rpath")) {
            # `configure.vars`'s `-Wl,-rpath,...` is not honored by terra's
            # own final link step (confirmed by testing: `readelf -d
            # terra.so` showed only R's own lib dir, none of `gdal_rpath`) --
            # patch the installed terra.so's RPATH directly instead, so
            # future sessions find GDAL/PROJ/geos/etc. on their own, without
            # any LD_LIBRARY_PATH for those specifically (gcc 12.1.0's own
            # libstdc++.so.6, needed for GLIBCXX_3.4.29/30, still can't be
            # fixed this way -- see ~/scripts/r/functions/load_terra_env.sh
            # and the long comment above for why)
            patchelf <- "/albedo/soft/sw/spack-sw/patchelf/0.17.2-fhafnlu/bin/patchelf"
            terra_so <- paste0(lib, "/terra/libs/terra.so")
            if (file.exists(terra_so) && file.exists(patchelf)) {
                existing_rpath <- system(paste0(patchelf, " --print-rpath ", terra_so), intern=T)
                new_rpath <- paste0(existing_rpath, ":", paste(gdal_rpath, collapse=":"))
                message("\npatch `", terra_so, "`'s RPATH (see note above) ...")
                system(paste0(patchelf, " --set-rpath '", new_rpath, "' ", terra_so))
            } else {
                warning("expected `terra_so` = \"", terra_so, "\" and/or `patchelf` = \"", patchelf,
                         "\" missing -- could not patch terra.so's RPATH; do it manually, see the note above", call.=F, immediate.=T)
            }
        }

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
    
    li <- vector("list", length=length(libpaths))
    # first loop just show packages to upgrade
    for (libpathi in seq_along(libpaths)) {
        message("*************************************************\n",
                "libpath ", libpathi, "/", length(libpaths), ": ", libpaths[libpathi])
        li[[libpathi]] <- NULL # default: no update needed/possible
        if (file.access(libpaths[libpathi], mode=2) == -1) { # dir not writeable
            message("--> directory not writeable. skip")
        } else {
            inst <- utils::packageStatus(lib.loc=libpaths[libpathi])
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
                                collapse=paste0("',\n", paste0(rep(" ", times=nchar(indent)), collapse=""), paste0(rep(" ", times=15), collapse=""), "'")),
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

