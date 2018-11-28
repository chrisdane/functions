m2lon <- function(dm, alat) {
    ## R-function for converting meter to longitudes in degree
    ## Input:
    ##      dm = distance in meters
    ##      alat = average latitude between the two fixes
    ## Output:
    ##      dlon = longitude difference in degrees
    ## # Reference:
    ##      American Practical Navigator, Vol II, 1975 Edition, p 5
    ##      http://www.movable-type.co.uk/scripts/latlong.html
    rlat <- alat * pi/180 # alat in radian
    p    <- 111415.13 * cos(rlat) - 94.55 * cos(3 * rlat)
    dlon <- dm / p
    dlon
}
