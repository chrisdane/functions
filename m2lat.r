m2lat <- function(dm, alat) {
    ## R-function for converting meters to latitudes in degree
    ## Input:
    ##      dm = meters
    ##      alat = average latitude between the two fixes
    ## Output:
    ##      dlat   = latitude difference in degrees
    ## # Reference:
    ##      American Practical Navigator, Vol II, 1975 Edition, p 5
    ##      http://www.movable-type.co.uk/scripts/latlong.html
    rlat <- alat * pi/180 # alat in radian
    m    <- 111132.09 - 566.05 * cos(2 * rlat) + 1.2 * cos(4 * rlat)
    dlat   <- dm / m 
    dlat
}
