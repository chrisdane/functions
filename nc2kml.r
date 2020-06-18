## r

# nc variable to kml shape

nc2kml <- function(lon, lat, mat2d, 
                   nc_filename, nc_varname, nc_lonfrom360to180, nc_latrev,
                   fout) {

    library(sp)
    #library(rgdal)
    library(plotKML)

    if (missing(mat2d)) {
        if (missing(nc_filename) || missing(nc_varname)) {
            stop("must provide either `mat2d` or `nc_filename` and `nc_varname`")
        }
        library(ncdf4)
        message("open `nc_filename` \"", nc_filename, "\" ...")
        ncid <- ncdf4::nc_open(nc_filename)
        message("read variable `nc_varname` \"", nc_varname, "\" ...")
        mat2d <- ncdf4::ncvar_get(ncid, nc_varname)
        if (missing(lon)) lon <- ncid$dim$lon$vals
        if (missing(lat)) lat <- ncid$dim$lat$vals
        if (is.null(lon) || is.null(lat)) {
            stop("could not determine lon and lat from nc_filename \"", nc_filename, "\"")
        }
        message("lon from `nc_filename` \"", nc_filename, "\":")
        print(head(lon))
        print(tail(lon))
        if (missing(nc_lonfrom360to180)) {
            stop("`nc_lonfrom360to180` must be either T of F")
        }
        if (nc_lonfrom360to180) {
            message("`nc_lonfrom360to180` = T --> change lon from 0,360 to -180,180 ...")
            library(abind)
            mat2d <- abind::abind(mat2d[(which.min(abs(lon - 180)):dim(mat2d)[1]),], 
                                  mat2d[1:(which.min(abs(lon - 180))-1),], along=1)
            lon <- lon - 180
            print(head(lon))
            print(tail(lon))
        }
        message("lat from `nc_filename` \"", nc_filename, "\":")
        print(head(lat))
        print(tail(lat))
        if (missing(nc_latrev)) {
            stop("`nc_latrev` must be either T of F")
        }
        if (nc_latrev) {
            message("`nc_latrev` = T --> flip lat ...")
            mat2d <- mat2d[,dim(mat2d)[2]:1]
            lat <- rev(lat)
            print(head(lat))
            print(tail(lat))
        }
    } else {
        if (missing(lon)) stop("must provide lon")
        if (missing(lat)) stop("must provide lat")
    } # if missing mat2d
    if (length(dim(mat2d)) != 2) stop("mat2d must have 2 dims")

    # export to kml
    if (missing(fout)) fout <- "fout.kml"
    if (file.exists(fout)) {
        message("`fout` \"", fout, "\" already exists. remove ...")
        file.remove(fout)
    }
    message("save `fout` \"", fout, "\" ...")
    mat2d_sp <- expand.grid(lon=lon, lat=lat)
    mat2d_sp$z <- as.vector(mat2d)
    
    # data.frame -> SpatialPointsDataFrame  
    sp::coordinates(mat2d_sp) <- c("lon", "lat")
    sp::proj4string(mat2d_sp) <- sp::CRS("+proj=longlat")
    
    # SpatialPointsDataFrame -> SpatialPixelsDataFrame
    # problem: lon/lat are irregular: need to `rasterize`
    #sp::points2grid(mat2d_sp, tolerance=0.01)
    #https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation
    mat2d_sp <- sp::SpatialPixelsDataFrame(points=mat2d_sp, tolerance=0.01, data=mat2d_sp@data)
    #r <- raster::raster(mat2d_sp["z"])
    if (F) plotKML::plotKML(mat2d_sp)
    plotKML::kml(mat2d_sp, file.name=fout, colour=)

    message("nc2kml() finished")

} # nc2kml
