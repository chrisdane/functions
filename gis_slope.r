# r

# calculate slope and aspect of topography using landsat::slopeasp()
#myslope <- function(ncin, nclon="lon", nclat="lat", ncvar,
#                    lon, lat, z, ncout, ...) {
    #message("start myslope()")
    #if (!missing(nclon) && !missing(lon)) stop("provide either `nclon` or `lon`")
    #if (!missing(nclat) && !missing(lat)) stop("provide either `nclat` or `lat`")
    #if (!missing(ncin) && !missing(z)) stop("provide either `ncin` or `z`")
    #if (!missing(ncvar) && !missing(z)) stop("provide either `ncvar` or `z`")
    #if (missing(ncin)) { # no file provided to read
    #    if (missing(lon)) stop("provide `lon`")
    #    if (missing(lat)) stop("provide `lat`")
    #    if (missing(z)) stop("provide `z`")
    #    if (missing(ncout)) stop("provide `ncout`")
    #} else { # file provided to read
    #    if (!is.character(nclon)) stop("provide `nclon` of type character")
    #    if (!is.character(nclat)) stop("provide `nclat` of type character")
    #    if (missing(ncvar)) stop("provide `ncvar`")
        if (T) { # test
            #rm(list=ls()); graphics.off()
            method <- "landsat::slopeasp"
            ncin <- "/home/mozi/data/post/fesom/timmean/bathy/LSea5_s5_regular_dx0.100_dy0.100_fesom_timmean_bathy_global.nc"
            nclon <- "lon"; nclat <- "lat"
            ncvar <- "bathy"
            ncfactor <- NULL
            #ncfactor <- list(operator="/", value=1000) # km --> m
        }
        library(ncdf4)
        message("read ", ncin, " ...")
        nc <- ncdf4::nc_open(ncin)
        lon <- nc$dim[[nclon]]$vals
        lat <- nc$dim[[nclat]]$vals
        z <- ncvar_get(nc, ncvar)
        if (!is.null(ncfactor)) {
            cmd <- paste0("z <- z ", ncfactor$operator, " ", ncfactor$value)
            message("run `", cmd, "` ...")
            eval(parse(text=cmd))
        }
        ncout <- paste0(dirname(ncin), "/", 
                        tools::file_path_sans_ext(basename(ncin)), 
                        "_slope.nc")
    #}
    message("diff(lon):")
    print(summary(diff(lon)))
    message("diff(lat):")
    print(summary(diff(lat)))
    dlon_deg <- mean(diff(lon), na.rm=T)
    dlat_deg <- mean(diff(lat), na.rm=T)
    # average distance in m --> distance at 45°N

    message("run ", method, "() on ", 
            paste(class(z), collapse=","), " obj with ",
            "nlon=", length(lon), ", nlat=", length(lat), ", ",
            "dlon=", dlon_deg, "°, dlat=", dlat_deg, "°, ",
            "dlon=", dlon_m, "m, dlat=", dlat_m, "m ...")
    if (method == "landsat::slopeasp") {
        library(landsat)
        #data(dem, package="landsat")
        # z must be class SpatialGridDataFrame, dataframe, or matrix
        elapsed <- system.time({
            slope <- landsat::slopeasp(z, EWres=dlon_m, NSres=dlat_m) # z and res must be in m
        })
        aspect <- slope$aspect
        slope <- slope$slope
    } else if (method == "starsExtra::slope") {
        library(starsExtra)
        #data(dem, package="starsExtra")
        # z must be class stars 
        elapsed <- system.time({
            slope <- starsExtra::slope(z)
        })
    }
    message("took ", elapsed)

    # save result as nc
    if (F) {
        message("save ", ncout, " ...")
        londim <- ncdim_def("lon", "degrees_east", lon)
        latdim <- ncdim_def("lat", "degrees_north", lat)
        slopevar <- ncvar_def("slope", "deg", list(londim, latdim))
        aspectvar <- ncvar_def("aspect", "deg", list(londim, latdim))
        ncout <- nc_create(ncout, list(slopevar, aspectvar))
        ncvar_put(ncout, slopevar, slope$slope)
        ncvar_put(ncout, aspectvar, slope$aspect)
        if (!missing(ncin)) ncatt_put(ncout, 0, "input", ncin)
        nc_close(ncout)
    }

    message("finished myslope()")

#} # myslope


