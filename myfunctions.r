# r

## my collection of  R functions that I need again and again

## section 1: geo-physical stuff

# leap years
is.leap <- function(years) {
    return(((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0))
}

# convert decimal year yyyy.f to YYYY-MM-DD HH:MM:SS 
yearsdec_to_ymdhms <- function(yearsdec, verbose=F) {
    # simpler: lubridate::date_decimal() but this function has a bug:
    # as.POSIXlt(lubridate::date_decimal(seq(2000, b=1/12, l=12)))$mon+1 = 
    # 1  1  3  4  5  6  7  8  9 10 10 12
    #yearsdec <- seq(-1-2*1/12, 11/12, b=1/12) # with -0.
    #yearsdec <- c(seq(-2-2*1/12, -1.0-1/12, b=1/12), seq(0, b=1/12, l=14)) # wout -0.
    #yearsdec <- seq(1, b=1/12, l=12)
    #yearsdec <- seq(1, 2, l=365)
    #yearsdec <- seq(0.00000000000000000000000, 0.08333333333329999426109, l=30)
    #yearsdec <- seq(0.08333333333329999426109, 0.16666666666669999186112, l=30)
    #yearsdec <- seq(0.83333333333329995262773, 0.91666666666670004737227, l=30)
    #yearsdec <- seq(0.91666666666670004737227, 1, l=30)
    if (missing(yearsdec)) stop("yearsdec missing")
    if (!is.numeric(yearsdec)) stop("yearsdec must be numeric")
    #options("digits"=22, scipen=999999999)
    nonNAinds <- which(!is.na(yearsdec))
    # rounding issue: https://github.com/tidyverse/lubridate/issues/833
    round <- 13 # round necessary; this needs to be imroved 
    #yearsf <- round(as.numeric(yearsdec), 13)
    yearsf <- round(yearsdec[nonNAinds], round)
    #                                   yearsdec             lubri            my
    #1  -1.1666666666666667406815349750104360282 -2-11-01 04:00:00 -2-11-1 0:0:0
    #2  -1.0833333333333334813630699500208720565 -2-12-01 14:00:00 -2-12-1 0:0:0
    #3  -1.0000000000000000000000000000000000000 -1-01-01 00:00:00  -1-1-1 0:0:0
    #4  -0.9166666666666667406815349750104360282 -1-01-30 12:00:00  -1-2-1 0:0:0
    #5  -0.8333333333333334813630699500208720565 -1-03-02 00:00:00  -1-3-1 0:0:0
    #6  -0.7500000000000001110223024625156540424 -1-04-01 12:00:00  -1-4-1 0:0:0
    #7  -0.6666666666666667406815349750104360282 -1-05-02 00:00:00  -1-5-1 0:0:0
    #8  -0.5833333333333334813630699500208720565 -1-06-01 12:00:00  -1-6-1 0:0:0
    #9  -0.5000000000000001110223024625156540424 -1-07-02 00:00:00  -1-7-1 0:0:0
    #10 -0.4166666666666667406815349750104360282 -1-08-01 12:00:00  -1-8-1 0:0:0
    #11 -0.3333333333333334813630699500208720565 -1-09-01 00:00:00  -1-9-1 0:0:0
    #12 -0.2500000000000001110223024625156540424 -1-10-01 12:00:00 -1-10-1 0:0:0
    #13 -0.1666666666666667406815349750104360282 -1-11-01 00:00:00 -1-11-1 0:0:0
    #14 -0.0833333333333334813630699500208720565 -1-12-01 12:00:00 -1-12-1 0:0:0
    #15 -0.0000000000000002220446049250313080847  0-01-01 00:00:00   0-1-1 0:0:0
    #16  0.0833333333333332593184650249895639718  0-01-31 12:00:00   0-2-1 0:0:0
    #17  0.1666666666666665186369300499791279435  0-03-02 00:00:00   0-3-1 0:0:0
    #18  0.2499999999999997779553950749686919153  0-04-01 12:00:00   0-4-1 0:0:0
    #19  0.3333333333333332593184650249895639718  0-05-02 00:00:00   0-5-1 0:0:0
    #20  0.4166666666666665186369300499791279435  0-06-01 12:00:00   0-6-1 0:0:0
    #21  0.4999999999999997779553950749686919153  0-07-02 00:00:00   0-7-1 0:0:0
    #22  0.5833333333333332593184650249895639718  0-08-01 12:00:00   0-8-1 0:0:0
    #23  0.6666666666666665186369300499791279435  0-09-01 00:00:00   0-9-1 0:0:0
    #24  0.7499999999999997779553950749686919153  0-10-01 12:00:00  0-10-1 0:0:0
    #25  0.8333333333333332593184650249895639718  0-11-01 00:00:00  0-11-1 0:0:0
    #26  0.9166666666666662965923251249478198588  0-12-01 12:00:00  0-12-1 0:0:0    
    if (F) { # this does not work
        if (any(yearsf >= -1 & yearsf < 0)) {
            message("there are some `-1 <= yearsdec < 0`. these are not defined since ",
                    "trunc(-0.5) = 0 = trunc(0.5) so decimal years -0.5 and 0.5 would ",
                    "yield the same year (year 0), whereas e.g. trunc(-1.5) = -1 != ",
                    "trunc(1.5) = 1.")
            yearsf[yearsf >= -1 & yearsf < 0] <- yearsf[yearsf >= -1 & yearsf < 0] - 1
            #yearsf[yearsf < 0] <- yearsf[yearsf < 0] - 1
        }
    } else if (T) { # shift negative years by -1 but not e.g. -1.0, -2.0
        if (any(yearsf < 0 & yearsf %% 1 != 0)) { 
            yearsf[yearsf < 0 & yearsf %% 1 != 0] <- yearsf[yearsf < 0 & yearsf %% 1 != 0] - 1
        }
    }
    years <- trunc(yearsf)
    yearsrel <- yearsf - years
    yearsrel[yearsrel < 0] <- 1 - abs(yearsrel[yearsrel < 0])
    yearsrel <- round(yearsrel, round)
    monthsrel_lookup <- round((0:11)/12, round) 
    months <- findInterval(x=yearsrel, vec=monthsrel_lookup)
    monthsrel <- monthsrel_lookup[months]
    dayspm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    dayspm <- dayspm[months]
    inds <- which(((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0)) # leap
    inds <- inds[which(dayspm[inds] == 28)]
    dayspm[inds] <- 29
    dmons <- yearsrel
    dmons <- dmons - monthsrel
    dmons[months < 12] <- dmons[months < 12]/
        (monthsrel_lookup[months[months < 12]+1] - monthsrel_lookup[months[months < 12]])
    dmons[months == 12] <- dmons[months == 12]/(1 - monthsrel_lookup[months[months == 12]])
    daysf <- dmons*dayspm + 1 # + 1 since day must be > 0 for POSIX*
    days <- trunc(daysf); daysrel <- daysf - days
    if (any(daysrel < 0)) stop("should not happen")
    hoursf <- 24*daysrel; hours <- trunc(hoursf); hoursrel <- hoursf - hours
    minsf <- 60*hoursrel; mins <- trunc(minsf); minsrel <- minsf - mins
    secsf <- 60*minsrel; secs <- trunc(secsf)
    if (verbose) {
        print(head(data.frame(yearsdec, yearsf, years, yearsrel, 
                              months, monthsrel, dayspm, dmons,
                              daysf, days, daysrel, hoursf, hours, hoursrel,
                              minsf, mins, minsrel, secsf, secs), n=37))
    }
    text <- paste0(years, "-", months, "-", days, " ", hours, ":", mins, ":", secs)
    if (length(nonNAinds) != length(yearsdec)) {
        years2 <- months2 <- days2 <- hours2 <- mins2 <- secs2 <- text2 <- rep(NA, t=length(yearsdec))
        years2[nonNAinds] <- years
        months2[nonNAinds] <- months
        days2[nonNAinds] <- days
        hours2[nonNAinds] <- hours
        mins2[nonNAinds] <- mins
        secs2[nonNAinds] <- secs
        text2[nonNAinds] <- text
        years <- years2; months <- months2; days <- days2 
        hours <- hours2; mins <- mins2; secs <- secs2
        text <- text2
    }
    return(list(ymdhms=list(year=years, mon=months, day=days, hour=hours, min=mins, sec=secs),
                text=text))
} # yearsdec_to_ymdhms

# make POSIX time with negative years
make_posixlt_origin <- function(years, origin_in=0, origin_out, 
                                allow_sticky=T, verbose=0) {

    # simpler: lubridate::date_decimal() but this function has a bug:
    # as.POSIXlt(lubridate::date_decimal(seq(2000, b=1/12, l=12)))$mon+1 = 
    # 1  1  3  4  5  6  7  8  9 10 10 12

    # input: 
    #   years (numeric; negative for "before `origin_in`")
    #   origin_in (numeric; origin of input years; default: 0 CE)
    #   origin_out (numeric; origin of output years; default: same as `origin_in`)
    # output:
    #   dates (POSIXlt; date values with respect to `origin_out`)
  
    # optional dependency: `sticky::sticky()` keeps the attribute 
    # `origin` also after subset of the returned POSIXlt object

    # check
    if (missing(years)) stop("must provide years (integer, numeric or POSIX*)")
    if (!is.integer(years) && 
        !is.numeric(years) && 
        !any(grepl("posix", class(years), ignore.case=T))) {
        stop("years must be integer, numeric or POSIX*")
    }
    if (!is.numeric(origin_in)) stop("origin_in must be numeric")
    if (missing(origin_out)) origin_out <- origin_in
    if (!is.numeric(verbose)) stop("verbose must be numeric")
    
    nyearsin <- length(years)
    nonNAinds <- which(!is.na(years))
    nnonNA <- length(nonNAinds)
    years <- years[nonNAinds]
    
    if (verbose > 0) {
        message(nnonNA, "/", nyearsin, " non-NA years:")
        print(head(years))
        print(tail(years))
        message("origin_in = ", origin_in, "\n",
                "origin_out = ", origin_out)
    }

    # capture yyyy.f if input is numeric
    if (is.integer(years) || is.numeric(years)) {
        ymdhms <- yearsdec_to_ymdhms(years)
        if (verbose > 0) {
            message("ymdhms:")
            print(head(ymdhms$text))
            print(tail(ymdhms$text))
        }
        years <- ymdhms$ymdhms$year
        months <- ymdhms$ymdhms$mon
        days <- ymdhms$ymdhms$day
    } else if (any(grepl("posix", class(years), ignore.case=T))) {
        if (any(class(years) == "POSIXct")) years <- as.POSIXlt(years)
        months <- years$mon + 1L
        days <- years$mday
        years <- years$year + 1900L
    }

    # capture different cases
    years_ge_zero_lt_10000 <- which(years >= 0 & years < 10000) # case 1
    years_ge_zero_ge_10000 <- which(years >= 0 & years >= 10000) # case 2
    years_lt_zero <- which(years < 0) # case 3
        
    # case 1
    # as.POSIXlt("0-1-1") to as.POSIXlt("9999-12-31") -> ok
    if (length(years_ge_zero_lt_10000) > 0) { 
        if (verbose > 0) {
            message("case 1 years[years_ge_zero_lt_10000]:")
            cat(capture.output(str(years[years_ge_zero_lt_10000])), sep="\n")
        }
        lt_ge_zero_lt_10000 <- as.POSIXlt(paste0(years[years_ge_zero_lt_10000], "-",
                                                 months[years_ge_zero_lt_10000], "-",
                                                 days[years_ge_zero_lt_10000]), tz="UTC") 
        if (verbose > 0) {
            message("case 1 lt_ge_zero_lt_10000:")
            cat(capture.output(str(lt_ge_zero_lt_10000)), sep="\n")
        }
    } # case 1

    # case 2
    # as.POSIXlt("10000-1-1") and above -> error "not in a standard unambiguous format"
    if (length(years_ge_zero_ge_10000) > 0) { 
        lt_ge_zero_ge_10000 <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # placeholder for saving results
        for (yeari in seq_along(years_ge_zero_ge_10000)) {
            by <- paste0(years[years_ge_zero_ge_10000][yeari], " years")
            tmp <- seq.POSIXt(from=as.POSIXlt(paste0("0000-",
                                                     months[years_ge_zero_ge_10000][yeari], "-",
                                                     days[years_ge_zero_ge_10000][yeari]), 
                                              tz="UTC"), l=2, b=by, tz="UTC")[2]
            lt_ge_zero_ge_10000[yeari] <- as.POSIXlt(tmp)
            if (verbose > 1) message("case 2 year ", years[years_ge_zero_ge_10000][yeari], ": from 0 by ", by, " --> ", 
                                     lt_ge_zero_ge_10000[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("case 2 lt_ge_zero_ge_10000:")
            cat(capture.output(str(lt_ge_zero_ge_10000)), sep="\n")
        }
    } # case 2

    # case 3
    # below or equal as.POSIXlt("-1-1-1") --> negative year gives error "not in a standard unambiguous format" 
    if (length(years_lt_zero) > 0) { 
        lt_lt_zero <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # placeholder for saving results
        for (yeari in seq_along(years[years_lt_zero])) {
            by <- paste0(years[years_lt_zero][yeari], " years")
            tmp <- seq.POSIXt(from=as.POSIXlt(paste0("0000-",
                                                     months[years_lt_zero][yeari], "-",
                                                     days[years_lt_zero][yeari]), 
                                              tz="UTC"), l=2, b=by, tz="UTC")[2]
            lt_lt_zero[yeari] <- as.POSIXlt(tmp)
            if (verbose > 1) message("case 3 year ", years[years_lt_zero][yeari], ": from 0 by ", by, " --> ", 
                                     lt_lt_zero[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("case 3 lt_lt_zero:")
            cat(capture.output(str(lt_lt_zero)), sep="\n")
        }

    } # if any negative years since origin_in

    # combine all cases
    posixlt <- as.POSIXlt(seq.POSIXt(as.POSIXlt("0-1-1", tz="UTC"), b="1 day", 
                                     l=nyearsin)) # placeholder
    if (length(years_ge_zero_lt_10000) > 0) {
        posixlt[nonNAinds[years_ge_zero_lt_10000]] <- lt_ge_zero_lt_10000
    }
    if (length(years_ge_zero_ge_10000) > 0) {
        posixlt[nonNAinds[years_ge_zero_ge_10000]] <- lt_ge_zero_ge_10000
    }
    if (length(years_lt_zero) > 0) {
        posixlt[nonNAinds[years_lt_zero]] <- lt_lt_zero
    }
    if (length(nonNAinds) != nyearsin) {
        posixlt[seq_len(nyearsin)[-nonNAinds]] <- NA
    }
    if (verbose > 0) {
        message("posixlt:")
        cat(capture.output(str(posixlt)), sep="\n")
    }

    # shift to new origin
    if (origin_out != origin_in) {
        
        shift_by <- origin_in - origin_out
        if (verbose > 0) message("shift\n   ", min(posixlt), " to ", max(posixlt), " since ", origin_in, "\n", 
                             "by ", shift_by, " years:")
        posixlt$year <- posixlt$year + shift_by
        if (verbose > 0) {
            message("   ", min(posixlt), " to ", max(posixlt), " since ", origin_out)
            message("class(posixlt) = ", class(posixlt))
        }

    } # if origin_out != origin_in or not

    # append origin
    attributes(posixlt)$origin <- origin_out
    if (verbose > 0) message("class(posixlt) = ", paste(class(posixlt), collapse=", "))
    
    # fix time zone
    #posixlt$zone <- rep("UTC", t=length(posixlt)) # todo: this breaks if input `years` is of length 1
    #if (verbose > 0) message("6 class(posixlt) = ", class(posixlt))

    # sort
    #posixlt <- sort(posixlt)
    
    # make own `origin` attribute sticky
    if (allow_sticky) {
        if (verbose > 0) message("load sticky package and make `origin` attribute permanent. ",
                                 "set `allow_sticky=F` if you do not want that.")
        if (any(search() == "package:sticky")) library(sticky)
        posixlt <- sticky::sticky(posixlt)
    } # if allow_sticky

    return(posixlt)

} # make_posixlt_origin

# minute/second degree to decimal degree longitude/latitude
deg2dec <- function(deg=0.0, min=0.0, sec=0.0) {
    if (length(deg) == 1 && length(min) == 1 && length(sec) == 1) {
        message("deg2dec(): dec = deg + min/60 + sec/3600 = ", deg, " + ", min, "/60 + ", sec, "/3600")
    } else {
        message("deg2dec(): dec = deg + min/60 + sec/3600")
    }
    dec <- deg + min/60 + sec/3600
    return(dec)
} # deg2dec

m2lon <- function(dm, alat) {
    # dm = distance in meters
    # alat = average latitude between the two fixes
    # American Practical Navigator, Vol II, 1975 Edition, p 5
    # http://www.movable-type.co.uk/scripts/latlong.html
    rlat <- alat * pi/180 # alat in radian
    p <- 111415.13 * cos(rlat) - 94.55 * cos(3 * rlat)
    dlon <- dm / p
    return(dlon)
} # m2lon

m2lat <- function(dm, alat) {
    # dm = meters
    # alat = average latitude between the two fixes
    # American Practical Navigator, Vol II, 1975 Edition, p 5
    # http://www.movable-type.co.uk/scripts/latlong.html
    rlat <- alat * pi/180 # alat in radian
    m <- 111132.09 - 566.05 * cos(2 * rlat) + 1.2 * cos(4 * rlat)
    dlat <- dm / m 
    return(dlat)
} # m2lat

# convert longitudes from 0,360 to -180,180
convert_lon_360_to_180 <- function(lon360, data360=NULL, data360_lonind=NULL) {
    if (missing(lon360)) stop("provide `lon360`")
    west_of_180_inds <- which(lon360 < 180)
    east_of_180_inds <- which(lon360 >= 180)
    lon180 <- lon360 - 180
    ret <- list(lon180=lon180)
    if (!is.null(data360)) {
        if (is.null(data360_lonind)) stop("provided `data360` but not `data360_lonind`")
        data360 <- as.array(data360)
        cmdeast <- rep(",", t=length(dim(data360))) 
        cmdeast[data360_lonind] <- paste0("east_of_180_inds")
        cmdeast <- paste(cmdeast, collapse="")
        cmdwest <- rep(",", t=length(dim(data360))) 
        cmdwest[data360_lonind] <- paste0("west_of_180_inds")
        cmdwest <- paste(cmdwest, collapse="")
        cmd <- paste0("data180 <- abind(data360[", cmdeast, "], ",
                                       "data360[", cmdwest, "], ",
                                       "along=", data360_lonind, ")")
        message("convert_lon_360_to_180(): run `", cmd, "` ...")
        library(abind)
        eval(parse(text=cmd))
        dimnames(data180) <- NULL
        ret$data180 <- data180
    } # if !is.null(data)
    return(ret)
} # convert_lon_360_to_180

# running mean
myma <- function(x, order, verbose=F, ...) {
    if (verbose) {
        message("yields the same result as\n,
                forecast::ma(x, order=order, centre=ifelse(order %% 2 == 0, F, T))\n
                monthly ts --> order=36 --> 3a ma\n
                daily   ts --> order=\n")
    }
    y <- stats::filter(x, filter=rep(1/order, t=order))
}

# Get the proportion variation explained. See this website for more details: http://goo.gl/jte8X
# http://www.gettinggeneticsdone.com/2011/08/sync-your-rprofile-across-multiple-r.html
get_rsq <- function(predicted, actual) {
    # "goodness of fit" = 
    # "coefficient of determination" = 
    # "fraction of variance explained by the model" = 
    # "Multiple R-squared:"-result of lm()
    1-sum((predicted-actual)^2)/sum((actual-mean(actual))^2)
}

# get p-value of linear model; from 3.2.1 of faraways book
get_pval <- function(model) {
    ## lm calculation:
    # https://madrury.github.io/jekyll/update/statistics/2016/07/20/lm-in-R.html
    # lm() calls lm.fit()
    # from lm.fit(): z <- .Call(C_Cdqrls, x, y, tol, FALSE)
    #                coef <- z$coefficients
    # C_Cdqrls is defined in src/library/stats/src/lm.c:
    #    F77_CALL(dqrls)(REAL(qr), &n, &p, REAL(y), &ny, &rtol,
    # fortrans dqrls is defined src/appl/dqrls.f:
    #    call dqrdc2(x,n,n,p,tol,k,qraux,jpvt,work)
    # fortrans dqrdc2 is defined src/appl/dqrdc2.f:
    #
    if (F) { # run lm example
        if (F) {
            ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
            trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
            group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
            weight <- c(ctl, trt)
            model <- lm(weight ~ group)
            predictors <- as.matrix(cbind(1, group))
        } else if (T) {
            library(faraway)
            data(gala)
            model <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
            predictors <- as.matrix(cbind(1, gala[,-c(1, 2)]))
        }
        rss <- sum(model$residuals^2) # residual standard error
        resvar <- rss/model$df.residual # df.residual =? n - npredictors
        se <- summary(model)$coefficients[,2]
        ## summary of lm:
        # `summary(model)` calls `summary.lm(model)`
        if (F) { # get "std. error", se, of the parameters:
            if (F) { # from summary.lm:
                Qr <- model$qr # actually: stats:::qr.lm(model)
                p1 <- seq_len(model$rank) # 1:rank of the fitted linear model (rank =? number of predictors)
                R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
                se <- sqrt(diag(R) * resvar) # this is the standard error of the estimated coefficients/parameters
            } else if (F) { # or
                var_cov_mat <- vcov(model) # variance-covariance matrix = model$qr
                se <- sqrt(diag(var_cov_mat))
            } else if (F) { # or
                xtxi <- solve(t(predictors) %*% predictors)
                se <- sqrt(diag(xtxi))*sqrt(resvar)
            }
        }
    } # run lm example
    # todo: precision is not as high as `summary(model)$coefficients[2,4]`
    if (class(lm) != "lm") stop("input must be of class \"lm\"")
    # model[[1]] is actual data that was modeled by predictors model[[2..]]
    a <- sum((lm$model[[1]]-mean(lm$model[[1]]))^2) 
    b <- sum(lm$res^2)
    n_predictors <- lm$rank-1
    n_df <- lm$df.residual
    c <- (a-b)/n_predictors/(b/n_df)
    1-pf(c, n_predictors, n_df)
}

# effective sample size after thiebaux and zwiers 1984
# --> formulated as in hannachi et al. 2007: `n_eff = n * (1 + 2 * sum_{k=1}^{n-1} (1-k/n) * \rho(k))^{-1}
neff <- function(ts) {
    if (missing(ts)) stop("ts missing")
    if (!is.null(dim(ts))) {
        if (length(dim(ts)) > 1) stop("ts must be vector")
        ts <- as.vector(ts)
    }
    # autocorrelation rho
    rho <- stats::acf(ts, lag.max=length(ts)-1, plot=F)
    rho <- rho$acf[seq_len(length(ts) - 1)]
    fac <- 1 - seq_len(length(rho))/length(ts)
    neff <- length(ts)*(1 + 2 * sum(fac * rho))^-1
    as.integer(neff)
} # neff function

# eigenvalue and -vector uncertainty after north et al. 1982
# --> formulated as in hannachi et al. 2007
north_etal_1982_rule <- function(eigenval, eigenvec=NULL) {
    if (missing(eigenval)) stop("must provide eigenval")
    neff <- neff(eigenval)
    eigenval.err <- eigenval * sqrt(2/neff)
    # rest from sinkr::northTest
    upper.lim <- eigenval + eigenval.err
    lower.lim <- eigenval - eigenval.err
    sig <- rep(1, length(eigenval))
    for (i in seq(sig)) {
        if ((i - 1) %in% seq(sig)) {
            if (upper.lim[i] > lower.lim[i - 1]) 
                sig[i] <- 0
        }
        if ((i + 1) %in% seq(sig)) {
            if (lower.lim[i] < upper.lim[i + 1]) 
                sig[i] <- 0
        }
    }
    result <- list(eigenval = eigenval, eigenval.err = eigenval.err, 
        upper.lim = upper.lim, lower.lim = lower.lim, sig = sig, 
        n.sig = min(which(sig == 0)) - 1)
    if (!missing(eigenvec)) { # calc uncerstaintied of eigenvectors
        # todo
    }
    result
} # north_etal_1982_rule function

# convert velocities with units package
speeds <- function(x=1, unit="cm/s") {
    library(units) # valid_udunits()
    x <- set_units(x=x, value=unit, mode="standard")
    lengths <- c("0.1mm", "mm", "cm", "10cm", "m", "10m", "100m", "km", "3km", "10km", "100km", "1000km")
    times <- c("s", "min", "h", "day", "month", "year", "kyear", "Myear")
    vec <- as.vector(outer(lengths, times, paste, sep="/"))
    for (i in 1:length(vec)) vec[i] <- set_units(x, value=vec[i], mode="standard")
    mat <- matrix(as.numeric(vec), nrow=length(lengths))
    rownames(mat) <- lengths; colnames(mat) <- times
    oo <- getOption("scipen") # oldoption
    options("scipen"=100); message("scipen=", getOption("scipen"))
    print(mat)
    options("scipen"=oo); message("scipen=", getOption("scipen"))
    invisible(mat)

} # speeds function

## section 2: r-stuff

# return currently running R executable
Rexe <- function() return(paste0(R.home(), "/bin/exec/R"))

tryCatch.W.E <- function(expr) { # from `demo(error.catching)`
    W <- NULL
    w.handler <- function(w) { # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value=withCallingHandlers(tryCatch(expr, error=function(e) e),
                                   warning=w.handler), 
         warning=W)
} # tryCatch.W.E

# will never understand this
checkfun <- function() {
    message("myfunctions.r: sys.parent() = ", sys.parent(), 
            ", current frame sys.nframe() = ", sys.nframe())
    # copypaste/Rscript: pa0,fr0; checkfun(): pa0,fr1; source("file.r") = pa3,fr4
    f1 <- function() message("f1(): sys.parent() = ", sys.parent(), 
                             ", current frame sys.nframe() = ", sys.nframe())
    f2 <- function() f1(); f3 <- function() f2()
    message("run f1"); f1() # copypaste/Rscript: pa0,fr1; checkfun() = pa1,fr2; source("file.r") = pa0,fr5
    message("run f2"); f2() # copypaste/Rscript: pa1,fr2; checkfun() = pa2,fr3; source("file.r") = pa5,fr6
    message("run f3"); f3() # copyüaste/Rscript: pa2,fr3; checkfun() = pa3,fr4; source("file.r") = pa6,fr7
    # --> test if run from file or as function:
    #if (sys.parent() == 3L && sys.nframe() == 4L) { # run `source(file.r")`
    #    me <- "file.r"
    #} else { # run `fun()`
    #    me <- normalizePath(getSrcFilename(fun, full.names=T))
    #}
} # checkfun

# return name of sourced file
# from https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
thisFile <- function() {
        cmdArgs <- commandArgs(trailingOnly = FALSE)
        needle <- "--file="
        match <- grep(needle, cmdArgs)
        if (length(match) > 0) {
                # Rscript
                return(normalizePath(sub(needle, "", cmdArgs[match])))
        } else {
                # 'source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
        }
}

# check if all elements of a list are identical
# https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
identical_list <- function(x) {
    if (length(x) == 1) {
        return(T)
    } else if (length(x) == 0) {
        return(F)
    } else {
        check <- vapply(1:(length(x)-1),
                    function(n) identical(x[[n]], x[[n+1]]),
                    logical(1))
        if (all(check)) T else F
    }
}

# irregular check from graphics::image()'s argument `useRaster` from graphics::image.default 
check_irregular <- function(x, y) {
    dx <- diff(x)
    dy <- diff(y)
    # all.equal(target, current, ...) returns
    #   TRUE            if d <= tolereance with
    #                       tolerance <- sqrt(.Machine$double.eps)
    #                       d <- (sum(abs(target - current))/length(target))
    #   a character     otherwise, giving the mean relative difference, 
    #                   e.g. "Mean relative difference: 0.2"; or other 
    #                   information like "Numeric: lengths (18, 1) differ"
    #                   if the lengths of target and current differ
    # isTRUE(x) returns
    #   TRUE if is.logical(x) && length(x) == 1L && !is.na(x) && x
    (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) ||
    (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))
}

reorder_legend <- function(le) {
    # check input
    if (le$ncol < 1) warning("reorder_legend(): le$ncol=", ncol, ". set to 1")
    le$ncol <- max(1, le$ncol)
    n <- length(le$text)
    nrow <- ceiling(n/le$ncol)
    # https://stackoverflow.com/questions/39552682/base-r-horizontal-legend-with-multiple-rows
    MyOrder <- as.vector(matrix(1:(nrow*le$ncol), nrow=nrow, ncol=le$ncol, byrow=T))
    
    # reorder every list element of length n
    for (i in 1:length(le)) {
        if (length(le[[i]]) == n) {
            le[[i]] <- le[[i]][MyOrder]
        }
    }
    return(le)
} # reorder_legend

# headtail
ht <- function(x, n=15, ...) {
    # from FSA::headtail
    if (!(is.matrix(x) | is.data.frame(x))) 
        x <- as.data.frame(x)
    if ("tbl_df" %in% class(x)) 
        x <- as.data.frame(x)
    N <- nrow(x)
    if (n >= N)
        tmp <- x
    else {
        h <- utils::head(x, n, ...)
        if (is.null(rownames(x))) 
            rownames(h) <- paste0("[", seq_len(n), ",]")
        t <- utils::tail(x, n, keepnums=T, ...)
        tmp <- rbind(h, t)
    }
    print(tmp)
} # ht()

grl_nfigs2nwords <- function(nfigs=1:12, ntabs) {
    # in GRL, 1 paper consists of 12 "publication units" (PU) max.
    pu_max <- 12
    # 1 PU = 500 words = 1 fig = 1 tab
    nwords_per_pu <- 500; nfigs_per_pu <- ntabs_per_pu <- 1
    # -> there is a maximum number of words per paper as a function of the number of figures and tables
    if (missing(ntabs)) ntabs <- rep(0, t=length(nfigs))
    if (length(ntabs) != length(nfigs)) stop("nfigs and ntabs are of different length")
    nwords <- pu_max*nwords_per_pu - (nfigs + ntabs)*nwords_per_pu
    df <- data.frame(nfig=nfigs, ntab=ntabs, nwords=nwords)
    print(df, row.names=F)
    return(df)
}

# color2rgb
col2rgba <- function(x, alpha) {
    if (missing(x)) stop("must provide colors")
    if (missing(alpha)) {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3)))
    } else {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3), alpha=alpha))
    }
}

# my colors
mycols <- function(n) {
    cols <- "black"
    if (n > 1) cols <- c(cols, "#E41A1C") # myred
    if (n > 2) cols <- c(cols, "#377EB8") # myblue
    if (n > 3) {
        library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
        cols <- c(cols, brewer.pal(min(8, max(3, n)), "Dark2"))
        cols <- cols[seq_len(min(length(cols), n))]
    }
    if (n > 3+8) cols <- c(cols, (3+8+1):n) # add default until end
    return(cols)
} # mycols

# get coords of clicked points in active plot
mylocator <- function(...) {
    # open plot with world map if no plot is already open 
    if (is.null(dev.list())) {
        library(maps)
        message("run maps::map(\"world\", interiod=F) ...")
        map("world", interior=F)
        message("par(\"usr\") = ", appendLF=F)
        dput(par("usr"))
    } 

    options(locatorBell=F) # turn off system beep
    message("To exit the locator, hit any mouse button but the first while the mouse is over the plot device ...")
    locator(type="o", ...)
} # mylocator

# get file format
cdo_get_filetype <- function(fin, cdo="cdo", ncdump="ncdump", verbose=T) {
    if (verbose) message("cdo_get_filetype() start with `verbose`=T ...")
    cmd <- paste0(cdo, " showformat ", fin)
    if (verbose) message("run `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) { # `cdo showformat` yields warn/error
        # `cdo showformat` on fesom data yields error:
        # Warning (cdf_read_xcoord): Unsupported array structure, skipped variable tosga!
        # Warning (cdfInqContents): No data arrays found!
        # Unsupported file structure
        message(input_format$warning$message)
        message("-> try to run `", ncdump, " -k` instead ...")
        cmd <- paste0(ncdump, " -k ", fin)
        if (verbose) message("run `", cmd, "`")
        input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
        if (!is.null(input_format$warning)) { # `ncdump -k` yields also warn/error
            stop(input_format$warning$message)
        } else { # no warning on `ncdump -k` 
            if (verbose) {
                for (i in seq_along(input_format$value)) {
                    message("--> \"", input_format$value[i], "\"")
                }
                if (length(input_format$value) > 1) {
                    message("take last entry")
                    input_format$value <- input_format$value[length(input_format$value)]
                }
                message("--> ", appendLF=F)
            }
        }
    } else { # no warning on `cdo showformat`
        if (verbose) {
            for (i in seq_along(input_format$value)) {
                message("--> \"", input_format$value[i], "\"")
            }
            if (length(input_format$value) > 1) {
                message("take last entry")
                input_format$value <- input_format$value[length(input_format$value)]
            }
        }
    }
    if (any(input_format$value == c(# from `cdo showformat`:
                                    "netCDF", "NetCDF", "NetCDF2", 
                                    "NetCDF4 classic", "NetCDF4 classic zip", 
                                    "netCDF-4 classic model",
                                    # from `ncdump -k`:
                                    "64-bit offset"))) {
        if (verbose) message("--> input is of type netcdf")
        file_type <- "nc"
    } else { # e.g. "GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"
        if (verbose) message("--> input is not of type netcdf or not known")
        file_type <- "non-nc"
    }
    
    if (verbose) message("cdo_get_filetype() finished")
    return(list(file_type=file_type, exact_format=input_format$value))
} # cdo_get_filetype

# set my default plot options
myDefaultPlotOptions <- function(plist=list(plot_type="pdf", 
                                            ts_width_in=7, ts_asp=2,
                                            ts_mon_width_in=7, ts_mon_asp=1,
                                            depth_width_in=7, depth_asp=1,
                                            map_width_in=7, map_asp=1,
                                            scatter_width_in=7, scatter_asp=1,
                                            #ts_width=2666, ts_height=1333, 
                                            #ts_width_m=2100, ts_height_m=1600,
                                            #depth_width=2666, depth_weight=1600,
                                            #map_width=2666, map_height=2000,
                                            #scatter_width=2666, scatter_height=2666,
                                            pdf_family="sans",
                                            pdf_embed_fun="grDevices::embedFonts",
                                            png_family="sans" 
                                            ), 
                                 verbose=F, ...) {
    dot_list <- list(...)
    dot_names <- names(dot_list)
    if (length(dot_list) > 0) {
        for (i in seq_along(dot_list)) {
            if (verbose && i == 1) message("*** myDefaultPlotOptions() start ***")
            if (dot_names[i] == "plot_type") {
                if (!any(dot_list[[i]] == c("png", "pdf", "active"))) {
                    stop("myDefaultPlotOptions(): given plot_type ", 
                         dot_list[[i]], " must be one of \"png\", \"pdf\" or \"active\"")
                }
                if (dot_list[[i]] == plist$plot_type) { # if wanted is already default
                    next # argument
                }
            }
            # check if provided pdf family is available
            # -> for png, system falls back to default automatically if wanted font is not available
            # -> for pdf, an error is raised
            if (dot_names[i] == "pdf_family") {
                # check if wanted font is one of system defaults
                fonts <- names(grDevices::pdfFonts())
                if (!any(fonts == dot_list[[i]])) { # if wanted font is not any system default
                    message("wanted pdf_family = \"", dot_list[[i]], "\" is not included in ", 
                            length(fonts), " currently loaded grDevices::pdfFonts()")
                    if (!any(search() == "package:extrafont")) { # try to load extrafont package
                        message("run `require(extrafont)`")
                        suppressMessages(require("extrafont"))
                    }
                    if (any(search() == "package:extrafont")) { # try to load extrafont package
                        fonts <- extrafont::fonts()
                        message("wanted pdf_family = \"", dot_list[[i]], "\" ", appendLF=F)
                        if (!any(fonts == dot_list[[i]])) { # wanted font not avilable
                            message("is also not included in ", length(fonts), 
                                    " currently loaded extrafont::fonts(). use default \"",
                                    plist$pdf_family, "\" ...")
                            dot_list[[i]] <- plist$pdf_family
                        } else { # wanted font available through extrafont package
                            message("is included in extrafont::fonts()")
                        }
                    } else { # extrafont package not available
                        message("use default \"", plist$pdf_family, "\" ...")
                        dot_list[[i]] <- plist$pdf_family
                    }
                }
                # check if embed command needs to be adjusted
                # -> font provided by `extrafont` package must be imbedded with `extrafont::embed_fonts()` 
                #    instead with default grDevices::embedFonts()`
                if (dot_list[[i]] != plist$pdf_family) { # if change is applied
                    if (any(search() == "package:extrafont")) { # check if wanted front comes from grDevices or extrafont
                        extrafont_fonts <- fonts()
                        default_fonts <- names(pdfFonts()) # includes default and extrafont-fonts
                        # strange workaround: once extrafont is loaded, one cannot infer the default fonts anymore 
                        default_fonts <- setdiff(default_fonts, extrafont_fonts) 
                        if (!(dot_list[[i]] %in% default_fonts)) {
                            message("change `pdf_embed_fun` from default \"", plist$pdf_embed_fun, 
                                    "\" to \"extrafont::embed_fonts\" ...")
                            plist$pdf_embed_fun <- "extrafont::embed_fonts"
                        }
                    }
                }
            } # if argument is `pdf_family`
            if (dot_list[[i]] != plist[[dot_names[i]]]) { # if change from default
                message("argument \"", dot_names[i], "\" provided -> overwrite default \"", 
                        plist[[dot_names[i]]], "\" with \"", dot_list[[i]], "\"")
                plist[[dot_names[i]]] <- dot_list[[i]]
            }
            if (verbose && i == length(dot_list)) message("*** myDefaultPlotOptions() finished ***")
            ## Note: 
            # print(str(dot_list[[i]]))
            # returns the value AND NULL
            #str(dot_list[[i]]) 
        } # for all arguments in dots `...`
    } # if any dots arguments given
    return(plist)
} # myDefaultPlotOptions

# determine plot width, height and pointsize based on wanted ppi and asp.
# main source of confusion: pixel is not a length unit since it 
# depends on pixels per inch (ppi), which can be specified for 
# png plots:
# 1 inch = 96 pixel if ppi = 96
# 1 inch = 300 pixel if ppi = 300
# default ppi of `grDevices::png()` = 72
plot_sizes <- function(width_in=NULL, height_in=NULL,
                       width_cm=NULL, height_cm=NULL,
                       png_ppi=300, asp=4/3, verbose=F) {
    
    if (!is.numeric(width_in) && !is.numeric(height_in) &&
        !is.numeric(width_cm) && !is.numeric(height_cm)) {
        stop("at least one of `width_in`, `height_in`, `width_cm`, `height_cm` must be numeric")
    } else if (is.numeric(width_in) && is.numeric(width_cm)) {
        stop("provide either `width_in` or `width_cm`")
    } else if (is.numeric(height_in) && is.numeric(height_cm)) {
        stop("provide either `height_in` or `height_cm`")
    }
    if (is.numeric(width_in) && !is.numeric(height_in)) height_in <- width_in/asp
    if (!is.numeric(width_in) && is.numeric(height_in)) width_in <- height_in*asp
    if (is.numeric(width_cm) && !is.numeric(height_cm)) height_cm <- width_cm/asp
    if (!is.numeric(width_cm) && is.numeric(height_cm)) width_cm <- height_cm*asp
    if (!is.numeric(width_in)) width_in <- width_cm/2.54
    if (!is.numeric(height_in)) height_in <- height_cm/2.54
    if (!is.numeric(width_cm)) width_cm <- width_in*2.54
    if (!is.numeric(height_cm)) height_cm <- height_in*2.54
    asp <- width_in/height_in # same for width_cm/height_cm

    # for png
    width_px <- width_in*png_ppi
    height_px <- height_in*png_ppi
    pointsize <- 12
    # increase png pointsize (default = 12) if asp < 2
    # png pointsize: the default pointsize of plotted text, interpreted as big
    #      points (1/72 inch) at ‘res’ ppi.
    # 1 pt = 1/72 inch = 0.01388889 inch = 0.35277780599999997 mm
    if (asp < 2) {
        asp_interp <- approx(x=c(1, 2), y=c(2, 1), n=100) # linearly interp asp 1 to 2 --> fac 2 to 1
        asp_fac_ind <- which.min(abs(asp_interp$x - asp))
        asp_fac <- asp_interp$y[asp_fac_ind]
        message("asp = ", round(asp, 3), 
                " --> multiply pointsize ", pointsize, " * asp_interp[", asp_fac_ind, "] = ", 
                pointsize, " * ", round(asp_fac, 3), " = ", round(pointsize*asp_fac, 3))
        pointsize <- pointsize*asp_fac
    }
    
    # for pdf
    # sizes in inch with respect to default pdf width 7 inch used as maximum pdf width
    pdf_width_in <- 7
    pdf_height_in <- pdf_width_in/asp
    #pdf_pointsize <- 12
    pdf_pointsize <- pointsize
    # pdf pointsize: the default point size to be used.  Strictly speaking, in
    #      bp, that is 1/72 of an inch, but approximately in points.
    #      Defaults to ‘12’.
    
    if (verbose) {
        message("png_ppi=", png_ppi, ",\n",
                "png_width_in=", width_in, ", png_height_in=", height_in, ",\n",
                "png_width_cm=", width_cm, ", png_height_cm=", height_cm, ",\n",
                "png_width_px=", width_px, ", png_height_px=", height_px, ",\n",
                "png_pointsize=", round(pointsize, 3), ", asp=", round(asp, 3), ",\n",
                "pdf_width_in=", pdf_width_in, ", pdf_height_in=", pdf_height_in, ", ",
                "pdf_pointsize=", round(pdf_pointsize, 3))
    }
    return(list(png_ppi=png_ppi,
                png_width_in=width_in, png_height_in=height_in,
                png_width_px=width_px, png_height_px=height_px,
                png_pointsize=pointsize, asp=asp,
                pdf_width_in=pdf_width_in, pdf_height_in=pdf_height_in,
                pdf_pointsize=pdf_pointsize))
} # plot_sizes

# nicer default pars
# attention: this overwrites the default par()
#par <- function(las=1, ...) {
#    graphics::par(las=las, ..., no.readonly = FALSE)
#} # usage: par(); plot(...)

# paste my relevant plot options
mypar <- function() {
    if (is.null(dev.list())) {
        y <- askYesNo("mypar(): no plot open. would you like to run 'dev.new()'?", 
                      default=F, prompts=c("Yes", "No", "Cancel"))
        if (y) {    
            dev.new()
        }
    }
    if (!is.null(dev.list())) {
        op <- par()
        message("current device no. ", dev.cur(), " is of type ", 
                names(dev.cur()), " with family='", op$family, "'")
        message("character width, height")
        message("   par(\"cin\") = c(", paste0(op$cin, collapse=", "), ") # inch")
        message("   par(\"cra\") = c(", paste0(op$cra, collapse=", "), ") # px; default = 1/72 = 0.01388889 inch") 
        message("   --> par(\"cra\")/par(\"cin\") = c(", paste0(op$cra/op$cin, collapse=", "), ") # ppi if device is png")
        message("device width, height (whole plot):")
        message("   par(\"fin\") = c(", paste0(par("fin"), collapse=", "), ") # inch")
        message("   dev.size(\"in\") = c(", paste0(dev.size("in"), collapse=", "), ")")
        message("   dev.size(\"px\") = c(", paste0(dev.size("px"), collapse=", "), ")")
        message("   dev.size(\"cm\") = c(", paste0(dev.size("cm"), collapse=", "), ")")
        message("plot width, height (where data is shown):")
        message("   par(\"pin\") = c(", paste0(op$pin, collapse=", "), ") # inch")
        message("   --> par(\"pin\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$pin*op$cra/op$cin, collapse=", "), ") # px")
        message("   --> par(\"pin\")*2.54 = c(", 
                paste0(op$pin*2.54, collapse=", "), ") # cm")
        message("plot coordinates:")
        message("   par(\"usr\") = c(", paste0(op$usr, collapse=", "), ") # l r b t")
        message("plot coordinates relative:")
        message("   par(\"plt\") = c(", paste0(op$plt, collapse=", "), ") # l r b t")
        message("margin size:")
        message("   par(\"omi\") = c(", paste0(op$omi, collapse=", "), ") # inch; b l t r")
        message("   --> par(\"omi\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$omi*op$cra/op$cin, collapse=", "), ") # px; b l t r")
        message("   --> par(\"omi\")*2.54 = c(", 
                paste0(op$omi*2.54, collapse=", "), ") # cm; b l t r")
        message("   par(\"mai\") = c(", paste0(op$mai, collapse=", "), ") # inch; b l t r (default: c(1.02, 0.82, 0.82, 0.42))")
        message("   --> par(\"mai\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$mai*op$cra/op$cin, collapse=", "), ") # px; b l t r")
        message("   --> par(\"mai\")*2.54 = c(", 
                paste0(op$mai*2.54, collapse=", "), ") # cm; b l t r")
        if (!all(op$oma == 0)) {
            message("sum outer and inner margin sizes")
            message("   c(", paste0(op$omi + op$mai, collapse=", "), ") # inch; b l t r")
            message("   c(", paste0(op$omi*op$cra/op$cin + op$mai*op$cra/op$cin, collapse=", "), ") # px; b l t r")
            message("   c(", paste0((op$omi + op$mai)*2.54, collapse=", "), ") # cm; b l t r")
        }
        message("margin rows:")
        message("   par(\"oma\") = c(", paste0(op$oma, collapse=", "), ") # b l t r")
        message("   par(\"mar\") = c(", paste0(op$mar, collapse=", "), ") # b l t r (default: c(5.1, 4.1, 4.1, 2.1) + 0.1)")
        if (!all(op$oma == 0)) {
            message("sum outer and inner margins rows")
            message("   c(", paste0(op$oma + op$mar, collapse=", "), ") # b l t r")
            message("   c(", paste0(op$oma*op$cra/op$cin + op$mar*op$cra/op$cin, collapse=", "), ") # b l t r")
            message("   c(", paste0((op$oma + op$mar)*2.54, collapse=", "), ") # b l t r")
        }
    }
} # mypar()

par_px2in <- function(px) {
    if (is.null(dev.list())) {
        stop("par_px2in(): no plot open.")
    } else { 
        op <- par()
        inch <- px*op$cin/op$cra
        message("in the current device ", dev.cur(), " (", names(dev.cur()), ")")
        message("   1 px*", op$cin[1], "/", op$cra[1], " = ", op$cin[1]/op$cra[1], " inch wide")
        message("   1 px*", op$cin[2], "/", op$cra[2], " = ", op$cin[2]/op$cra[2], " inch high")
        inch
    }
} # par_px2in 

# load function for packages
load_packages <- function(pkgs) {
    status <- sapply(pkgs, function(pkg) {
                         if (!any(search() == paste0("package:", pkg))) {
                             library(pkg, character.only=T, logical.return=T)
                         } else {
                             message("package '", pkg, "' already loaded.")
                         }})
    if (length(unlist(status)) > 0) {
        stop()
    }
} # load_packages

# Get month names in specific locale
mymonth.name <- function(inds, locales=Sys.getlocale("LC_TIME")) {
 
    ## https://stat.ethz.ch/pipermail/r-help/2004-May/051503.html

    if (any(!(inds %in% 1:12))) {
        stop("mymonth.name(): 'inds' must be in 1:12")
    }

    months <- vector("list", l=length(locales))
    names(months) <- locales

    # save system locale for later
    locale.bak <- Sys.getlocale("LC_TIME")

    for (i in 1:length(locales)) {

        # change locale is needed
        if (Sys.getlocale("LC_TIME") != locales[i]) {
            
            # check if locale can be changed; returns the locale (success) or "" (no success)
            status <- tryCatch(suppressWarnings(Sys.setlocale("LC_TIME", locales[i])),
                               error=function(e) e,
                               warning=function(w) w)
            if (status != "") Sys.setlocale("LC_TIME", locales[i])
        } # if locale change is necessary

        # only evaulate result if system has correct locale
        if (Sys.getlocale("LC_TIME") == locales[i]) {
            months[[i]] <- format(ISOdate(2004, inds, 1), "%B")
        }

    } # for i locales

    if (any(sapply(months, is.null))) {
        locale_avail <- system("localedef --list-archive", intern=T)
        if (length(locale_avail) > 0) {
            months[[i+1]] <- locale_avail
        }
        names(months)[i+1] <- "locale_avail"
    } # if any locales not successful

    # restore original locale if necessary
    if (Sys.getlocale("LC_TIME") != locale.bak) Sys.setlocale("LC_TIME", locale.bak)

    return(months)

} # mymonth.name function

# convert human readable size to bytes
# https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r/49380514
size2byte <- function(x, unit) {
    if (any(!is.finite(x)) || any(!is.character(unit))) stop("x must be finite and unit must be character")
    if (any(is.na(match(unit, c("B", "K", "M", "G", "T", "P"))))) {
        stop("`unit` must be one of \"B\", \"K\", \"M\", \"G\", \"T\" or \"P\"")
    }
    mult <- c("B"=1,  "K"=1024,  "M"=1024^2,  "G"=1024^3,  "T"=1024^4,  "P"=1024^5)
    x * unname(mult[unit])
} # size2byte function

get_encoding <- function(test_symbol="ä", test_ctype="de", verbose=F) {
    
    encoding <- iconvlist()
    n <- length(encoding)
    encoding <- sapply(encoding, function(x) iconv(test_symbol, from=x))
    if (any(is.na(encoding))) encoding <- encoding[!is.na(encoding)]
    if (any(encoding == test_symbol)) {
        encoding <- encoding[which(encoding == test_symbol)]
        if (verbose) {
            message(length(encoding), "/", n, 
                    " encodings can print test_symbol \"", test_symbol, "\":")
            message(paste(names(encoding), collapse=" "))
        }
        encoding <- names(encoding[1])
        message("get_encoding(): first found encoding that matches \"", 
                test_symbol, "\": ", encoding)
    } else {
        message("could not find an encoding to evaluate test_symbol = \"", 
                test_symbol, "\"")
    }
	ctype <- Sys.getlocale("LC_CTYPE")

    # repeat for ctype 
	if (regexpr(test_ctype, ctype, ignore.case=T) == -1) {
		
		# get available langs
		locs <- system("locale -a", intern=T)
		# check if german character type is possible
		if (!any(regexpr(test_ctype, locs, ignore.case=T) != -1)) {
			message("could not find a locale contaning the pattern \"", 
                    test_ctype, "\" (case is ignored)")
		} else {
			locs <- locs[which(regexpr(test_ctype, locs, ignore.case=T) != -1)]
			message("Sys.getlocalte(\"LC_CTYPE\"): ", ctype)
			message("Run Sys.setlocale(\"LC_CTYPE\", \"", locs[1], "\")")
			Sys.setlocale("LC_CTYPE", locs[1])
			message("Sys.getlocale(\"LC_CTYPE\"): ", Sys.getlocale("LC_CTYPE"))
		
			# get available encodings
			encoding <- iconvlist()
            n <- length(encoding)
			encoding <- sapply(encoding, function(x) iconv(test_symbol, from=x))
			if (any(encoding == test_symbol)) {
                if (verbose) {
                    message(length(encoding), "/", n, 
                            " encodings can print test_symbol \"", test_symbol, "\":")
                    message(paste(names(encoding), collapse=" "))
                }
                encoding <- names(encoding[1])
                message("get_encoding(): first found encoding that matches \"", 
                        test_symbol, "\": ", encoding)
			} else {
				message("could not find an encoding to evaluate test_symbol = \"", 
                        test_symbol, "\"")
			}
		} # if pattern test_ctype is contained in locale -a
	} else {
        if (verbose) message("test_ctype = \"", test_ctype, 
                             "\" in `Sys.getlocale(\"LC_CTYPE\")` = ", ctype) 
    } # if current LC_CTYPE does not contain test_ctype
    return(encoding)
} # get_encoding

# check gs command from grDevices::embedFonts()
myembedFonts <- function (file, format, outfile = file, fontpaths = character(), 
    options = character()) 
{
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) 
        stop("'file' must be a non-empty character string")
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) 
        stop("GhostScript was not found")
    if (.Platform$OS.type == "windows") 
        gsexe <- shortPathName(gsexe)
    suffix <- gsub(".+[.]", "", file)
    if (missing(format)) 
        format <- switch(suffix, ps = , eps = "ps2write", pdf = "pdfwrite")
    if (!is.character(format)) 
        stop("invalid output format")
    grDevices:::check_gs_type(gsexe, format)
    tmpfile <- tempfile("Rembed")
    if (length(fontpaths)) 
        fontpaths <- paste0("-sFONTPATH=", shQuote(paste(fontpaths, 
            collapse = .Platform$path.sep)))
    args <- c(paste0("-dNOPAUSE -dBATCH -q -dAutoRotatePages=/None -sDEVICE=", 
        format), paste0(" -sOutputFile=", shQuote(tmpfile)), 
        fontpaths, options, shQuote(file))
    message("run `", gsexe, " ", args, "` ...")
    ret <- system2(gsexe, args)
    if (ret != 0) 
        stop(gettextf("status %d in running command '%s'", ret, 
            cmd), domain = NA)
    if (outfile != file) 
        args[2] <- paste0(" -sOutputFile=", shQuote(outfile))
    cmd <- paste(c(shQuote(gsexe), args), collapse = " ")
    file.copy(tmpfile, outfile, overwrite = TRUE)
    invisible(cmd)
} # from grDevices::embedFonts()

myErrorFun <- function() {
    # default: getOption("error") = NULL
    # weird bug: When running a script from the command line you will have to skip one or two traceback() calls
    # https://stackoverflow.com/questions/1445964/r-script-line-numbers-at-error
    # https://stackoverflow.com/questions/13116099/traceback-for-interactive-and-non-interactive-r-sessions
    # #traceback(2) #recover() #rlang::last_trace(); rlang::last_error(); # library(rlang)
    tmp <- base::.traceback(2)
    #tmp <- base::traceback(2)
    #message("str(tmp)")
    #print(str(tmp))
    if (!is.null(tmp) && # if e.g. not ctrl+c
        #regexpr("stop\\(", tmp[[1]]) != -1 && # if stop() was called last
        !is.null(srcref <- attr(tmp[[1]], "srcref"))) { # if attribute exists
        message("in line ", srcref[1], " in ", basename(attr(srcref, "srcfile")$filename))
    } else {
        #message("str(tmp)")
        #print(str(tmp))
    }
}

mynews <- function() {
    current_version <- paste0(R.version$major, ".", R.version$minor)
    db <- news(Version==current_version)$Text
    db <- gsub("\\\n", " ", db)
    message("R ", current_version, " news:")
    for (i in seq_along(db)) message(i, "/", length(db), ": ", db[i])
}

# paste stuff that I always forget
myhelp <- function() {
	tmp <- c("   Built-in constants ...",
             "      LETTERS, letters, month.abb, month.name, pi",
             "   default fonts/encodings ...",
             "      pdfFonts(); encodings <- list.files(system.file(\"enc\", package=\"grDevices\"))",
             "      embed: grDevices::embedFonts(plotname, outfile=plotname)",
             "   non-default fonts: package \"extrafont\"",
             "      font_import() (loadfonts()), fonts(), font_install(\"fontcm\"), \"CM *\"",
             "      embed: embed_fonts(plotname, outfile=plotname)",
             "      evince -> File -> Properties -> Fonts -> \"Embedded subset\"",
             "      library(extrafontdb) (reset)",
             "   available locales ...",
             "      locs <- system(\"locale -a\", intern=T)",
             "   read file with specific encoding ...",
             "      iconvlist <- iconvlist()",
             "   POSIX ...",
             "      d <- as.POSIXct(\"2000-01-30\", format=\"%Y-%m-%d\", tz=\"CET\") # = \"2000-01-30 CET\"",
             "      n <- as.numeric(d) # = 949186800",
             "      dd <- as.POSIXct(n, origin=\"1970-01-01\",tz=\"CET\") # = \"2000-01-30 CET\"",
             "          %d	Day of the month (decimal number)",
             "          %m	Month (decimal number)",
             "          %b	Month (abbreviated)",
             "          %B	Month (full name)",
             "          %y	Year (2 digit)",
             "          %Y	Year (4 digit)",
             "   Package options ...",
             "      install: install.packages(\"pkg\", lib=\"/path/\", configure.args=\"--help\")",
             "               install.packages(\"pkg\", lib=\"/path/\", configure.args=\"--with-pkg-lib=/path/ --with-pkg-include=\")",
             "               rgeos:",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/geos-3.6.1-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  install.packages(\"rgeos\", configure.args=\"--with-geos-config=/sw/rhel6-x64/geos-3.4.2-gcc48/bin/geos-config\")",
             "               rgdal:",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/gdal-2.1.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  install.packages(\"rgdal\", configure.args=\"--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config --with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include --with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib\")",
             "               sf:",
             "                  install.packages(\"sf\", configure.args=\"--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config --with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include --with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib --with-geos-config=/sw/rhel6-x64/geos-3.6.1-gcc48/bin/geos-config\")",
             "               rJava:",
             "                  install default-jre on system, then `R CMD javareconf`",
             "                  better use a package without java-dependence: openxlsx or readxl",
             "               devtools::install_github(\"user/package\", args=\"--with-keep.source\")",
             "               withr::with_libpaths(new=\"libpath\", install_github(\"user/package\"))",
             "               https://cran.r-project.org/web/checks/check_summary_by_package.html",
             "      compile: R CMD build \"package directory\"",
             "               R CMD INSTALL -l \"lib\" \"pkg.tar.gz\"",
             "               install.packages(\"pkg.tar.gz\", repos=NULL)",
             "      load:    library(pkg, lib=\"lib\")",
             "      unload:  detach(package:pkg, unload=T)",
             "      update:  update.packages(lib=\"lib/to/update\", instlib=\"lib/to/put/updates\", ask=F, checkBuilt=T)",
             "               update.packages(instlib=.libPaths()[1], ask=F, checkBuilt=T)",
             "               old <- old.packages(lib=.libPaths()[1])",
             "               dtupdate::github_update(auto.install=T, ask=T, dependencies=T)",
             "      remove:  remove.packages(\"pkg\", lib=\"lib\")",
             "      which:   packageDescription(\"pkg\"); packageVersion(\"pkg\"); citation(\"pkg\"); find.package(\"pkg\"); maintainer(\"pkg\"); library(help=\"pkg\")",
             "      archive: https://cran.r-project.org/src/contrib/Archive",
             "      help:    https://cran.r-project.org/doc/FAQ/R-FAQ.html",
             "      data:    data(package=\"pkg\")",
             "   Functions ...",
             "      C-object infos: e.g. \"graphics:::C_image\"", 
             "      S3: getAnywhere(fun or \"fun\"); methods(fun or \"fun\")",
             "      S4: showMethods(fun or \"fun\"); getMethods(fun or \"fun\")",
             "      .Internal/.Primitive: pryr::show_c_source, e.g. `show_c_source(.Internal(mean(x)))`",
             "      R source code: https://github.com/wch/r-source.git",
             "   Find R ...",
             "      executable: R.home()/bin/exec/R",
             "   Run R ...",
             "      in background:            $ Rscript script.r > test.log 2>&1 &",
             "      as script:                #!/usr/bin/env Rscript",
             "      without this ~/.Rprofile: $ R --no-init-file (or --vanilla)")
    message(paste0(tmp, collapse="\n"))
}

# works in combi with
#export LD_LIBRARY_PATH=/sw/rhel6-x64/gdal-2.1.3-gcc48/lib/:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib/:$LD_LIBRARY_PATH
#install.packages("rgdal", configure.args=c("--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config" 
#                                           ,"--with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib"
#                                           ,"--with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include"
#                                           ))

# works:
#install.packages("proj4", configure.args=c("PKG_LIBS=-L/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib"
#                                           ,"PKG_CPPFLAGS=-I/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include"
#                                           ))

# works in combi with export LD_LIBRARY_PATH=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib:$LD_LIBRARY_PATH: 
#install.packages("units", configure.args=c("--with-udunits2-lib=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib"
#                                           ,"--with-udunits2-include=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/include"
#                                           ))


