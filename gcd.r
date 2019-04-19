## R
## great circle distance

# test:
# gcd(long1=-20, lat1=45, long2=-35, lat2=55)

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2, R=6371) {
    # Earth mean radius [km]
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(list(dist=d, r=R)) # Distance in km
}

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2, R=6371) {
    # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c
    return(list(dist=d, r=R)) # Distance in km
}

# Calculates the geodesic distance between two points specified by radian latitude/longitude using
# Vincenty inverse formula for ellipsoids (vif)
gcd.vif <- function(long1, lat1, long2, lat2) {
 
    # WGS-84 ellipsoid parameters
    a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
    b <- 6356752.314245  # ength of minor axis of the ellipsoid (radius at the poles)
    f <- 1/298.257223563 # flattening of the ellipsoid

    L <- long2-long1 # difference in longitude
    U1 <- atan((1-f) * tan(lat1)) # reduced latitude
    U2 <- atan((1-f) * tan(lat2)) # reduced latitude
    sinU1 <- sin(U1)
    cosU1 <- cos(U1)
    sinU2 <- sin(U2)
    cosU2 <- cos(U2)

    cosSqAlpha <- NULL
    sinSigma <- NULL
    cosSigma <- NULL
    cos2SigmaM <- NULL
    sigma <- NULL

    lambda <- L
    lambdaP <- 0
    iterLimit <- 100
    while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
        sinLambda <- sin(lambda)
        cosLambda <- cos(lambda)
        sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                          (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
        if (sinSigma==0) return(0)  # Co-incident points
        cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
        cosSqAlpha <- 1 - sinAlpha*sinAlpha
        cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
        if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
        C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
        lambdaP <- lambda
        lambda <- L + (1-C) * f * sinAlpha *
                  (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
        iterLimit <- iterLimit - 1
    }
    if (iterLimit==0) return(NA)  # formula failed to converge
    uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
    A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
    B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
    deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                             B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
    s <- b*A*(sigma-deltaSigma) / 1000

    return(list(dist=s, r_eq=a, r_pole=b)) # Distance in km
}

## FESOMs dist_on_earth() function from gen_support.F90 
gcd.fesom <- function(lon1, lat1, lon2, lat2, r_earth=6367.5) {
    # oce_modules.F90
    alpha <- acos(cos(lat1)*cos(lat2)*cos(lon1-lon2)+sin(lat1)*sin(lat2))
    dist <- r_earth*abs(alpha)
    return(list(dist=dist, r=r_earth))
}

## http://www.teos-10.org/pubs/gsw/html/gsw_distance.html
gcd.teos10 <- function(lon1, lat1, lon2, lat2, p1=0, p2=0, earth_radius=6371000) {
	dlong <- lon2 - lon1
	dlat <- lat2 - lat1

	a <- (sin(0.5*dlat))^2 + cos(lat1)*cos(lat2)*(sin(0.5*dlong))^2
	angles <- 2 * atan2(sqrt(a), sqrt(1 - a))

	p_mid <- 0.5*(p1 + p2)
	lat_mid <- 0.5*(lat1 + lat1)
	z <- gsw::gsw_z_from_p(p_mid,lat_mid) # Note. z is height and is negative in the ocean.
																
	distance <- (earth_radius + z)*angles # Note. The output is in m not km.
	return(list(dist=distance, r=earth_radius))
}

# Calculates the geodesic distance between two points specified by degrees (DD) latitude/longitude using
# Haversine formula (hf), Spherical Law of Cosines (slc) and Vincenty inverse formula for ellipsoids (vif)
gcd <- function(long1_deg, lat1_deg, long2_deg, lat2_deg, p1=0, p2=0) {
    # https://www.r-bloggers.com/great-circle-distance-calculations-in-r/

    # Convert degrees to radians
    long1_rad <- deg2rad(long1_deg)
    lat1_rad <- deg2rad(lat1_deg)
    long2_rad <- deg2rad(long2_deg)
    lat2_rad <- deg2rad(lat2_deg)

    haversine <- gcd.hf(long1_rad, lat1_rad, long2_rad, lat2_rad)
	sphere <- gcd.slc(long1_rad, lat1_rad, long2_rad, lat2_rad)
	vincenty <- gcd.vif(long1_rad, lat1_rad, long2_rad, lat2_rad)
	fesom.dist_on_earth <- gcd.fesom(long1_rad, lat1_rad, long2_rad, lat2_rad)

    li <- list(haversine=haversine, sphere=sphere, vincenty=vincenty,
               fesom.dist_on_earth=fesom.dist_on_earth)

    if (exists("rdist.earth")) { # fields package is loaded
		rdist.earth <- fields::rdist.earth(matrix(c(long1_deg, lat1_deg), ncol=2), 
                                           matrix(c(long2_deg, lat2_deg), ncol=2), 
                                           miles=F, R=6371)[1,]
        li <- c(li, rdist.earth=rdist.earth)
    }

	if (exists("gsw_z_from_p")) { # gsw package is loaded
        teos10 <- gcd.teos10(long1_rad, lat1_rad, long2_rad, lat2_rad, p1=p1, p2=p2)
        li <- c(li, teos10=teos10)
    }

    return(li)
}


