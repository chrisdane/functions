# r

## my collection of  R functions that I need again and again

## section 1/2: geo-physical stuff

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
    round <- 13 # round necessary; todo: somehow possible without round?
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
    text <- paste0(years, "-", sprintf("%02i", months), "-", sprintf("%02i", days), " ", 
                   sprintf("%02i", hours), ":", sprintf("%02i", mins), ":", sprintf("%02i", secs))
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
        if (!any(search() == "package:sticky")) library(sticky)
        posixlt <- sticky::sticky(posixlt)
    } # if allow_sticky

    return(posixlt)

} # make_posixlt_origin

difftime_yr <- function(from, to) {
    # the first- and current-year-problem:
    # calc age from e.g. 1992-05-02 (leap year) to 2022-05-02 (non-leap year)
    # wrong method:
    # age_a <- 1 - from[datei]$yday/366 # rest of first year = 0.6666667
    # age_a <- age_a + to[datei]$yday/365 # first part of current year = 0.3342466
    # --> age_a = 0.6666667 + 0.3342466 = 1.000913 != 1 --> wrong
    # --> this method only works if first and current year are both either non-leap or leap years
    # correct method:
    # age_a <- 1 - (from[datei]$mon/12 + from[datei]$mday/31/12) # rest of first year = 0.6612903
    # age_a <- age_a + to[datei]$mon/12 + to[datei]$mday/31/12 # first part of current year = 0.3387097
    # --> age_a = 0.6612903 + 0.3387097 = 1 --> correct
    if (missing(from)) from <- as.POSIXct("1970-1-1", tz="UTC")
    if (missing(to)) to <- as.POSIXct(Sys.time(), tz="UTC")
    for (i in seq_len(2)) {
        if (i == 1) class <- class(from)
        if (i == 2) class <- class(to)
        if (length(class) == 2 && class[2] == "POSIXt") {
        } else if (length(class) == 1 && class == "Date") { # add further if needed
        } else {
            message("class of `", ifelse(i == 1, "from", "to"), "` (\"",  
                    paste(class, collapse="\", \""), "\") must be \"Date\" or \"POSIX*t\" --> run `base::as.POSIXct()` ...")
            if (i == 1) from <- as.POSIXct(from)
            if (i == 2) to <- as.POSIXct(to)
        }
    }
    if (length(from) != length(to)) {
        if (length(from) == 1 && length(to) != 1) { # repeat from
            from <- rep(from, t=length(to))
        } else if (length(from) != 1 && length(to) == 1) { # repeat to
            to <- rep(to, t=length(from))
        } else {
            stop("`from` and `to` must either be of same length or, if not, one of both must be of length 1")
        }
    }
    # ignore hour, min, sec
    fromlt <- as.POSIXlt(from, tz="UTC"); tolt <- as.POSIXlt(to, tz="UTC")
    fromlt$hour <- fromlt$min <- fromlt$sec <- tolt$hour <- tolt$min <- tolt$sec <- 0
    from <- as.POSIXct(fromlt); to <- as.POSIXct(tolt)
    dpms <- c("Jan"=31, "Feb"=28, "Mar"=31, "Apr"=30, "May"=31, "Jun"=30, # days per month of non-leap year
              "Jul"=31, "Aug"=31, "Sep"=30, "Oct"=31, "Nov"=30, "Dec"=31)
    mpy <- 12 # 12 months per year
    ages_yr <- rep(NA, t=length(from))
    for (datei in seq_along(ages_yr)) {
        dpm_start <- unname(dpms[fromlt[datei]$mon + 1]) # days per month of start month
        if (fromlt[datei]$mon + 1 == 2 && # if start mon is Feb and start year is leap year
            ((fromlt[datei]$year+1900 %% 4 == 0) & (fromlt[datei]$year+1900 %% 100 != 0)) | (fromlt[datei]$year+1900 %% 400 == 0)) { 
            dpm_start <- 29
        }
        # decimal of start date:
        age_a <- fromlt[datei]$mon/mpy + fromlt[datei]$mday/dpm_start/mpy # 0.3387097
        if (to[datei] > from[datei]) { # current date is after start date (hour, min, sec ignored)
            if (tolt[datei]$year > fromlt[datei]$year) { # current date is already next year
                age_a <- 1 - age_a # rest of first year: 1 - 0.3387097 = 0.6612903 yrs
                dpm_current <- unname(dpms[tolt[datei]$mon + 1]) # days per month of current month
                if (tolt[datei]$mon + 1 == 2 && # if current mon is Feb and current year is leap year
                    ((tolt[datei]$year+1900 %% 4 == 0) & (tolt[datei]$year+1900 %% 100 != 0)) | (tolt[datei]$year+1900 %% 400 == 0)) { 
                    dpm_current <- 29
                }
                # rest of first year + decimal of current date:
                age_a <- age_a + tolt[datei]$mon/mpy + tolt[datei]$mday/dpm_current/mpy # 0.6612903 + (0 < x <= 1)
                # add 1 year for every year between start year and current year:
                nyears <- tolt[datei]$year - fromlt[datei]$year
                age_a <- age_a + nyears - 1
            } else { # current date is in the same year as first date
                age_a <- (tolt[datei]$mon/mpy + tolt[datei]$mday/dpm_start/mpy) - age_a # (0.3413978 <= x <= 1) - 0.3387097 
                # -> age_a = 0.3413978 - 0.3387097 = 0.0026881 yrs if current date only one day later than start date
            }
        } else if (to[datei] == from[datei]) { # current date and start date are identical
            if (((fromlt[datei]$year+1900 %% 4 == 0) & (fromlt[datei]$year+1900 %% 100 != 0)) | (fromlt[datei]$year+1900 %% 400 == 0)) { 
                age_a <- 1/366 # 0.00273224 yrs 
            } else {
                age_a <- 1/365 # 0.002739726 yrs
            }
        } else { # current date is before start date
            message("datei ", datei, ": ", events[datei], " from ", from[datei], " to ", to[datei], " --> start date is in future")
        }
        ages_yr[datei] <- age_a
    } # for datei
    return(data.frame(from=from, to=to, dyr=ages_yr, dmon=ages_yr*12))
} # difftime_yr()

if (F) { # todo: seq(from, to, b=non-integer-month)
    origin <- as.POSIXlt("2015-1-1 00:00:00") # from nc1$dim$time$units = "months since 2015-1-1 00:00:00"
    dts <- nc1$dim$time$vals # 0.5 12 24 ... 984 996 1007 months since 2015-1-1
    # integer months can be understood by `seq`, non-integer months not:
    time <- rep(origin, t=length(dts)) # placeholder
    month_of_origin <- origin$mon+1 # e.g. 1 for January
    for (ti in seq_along(time)) {
        if (dts[ti] %% 1 == 0) { # dt in months is integer, e.g. 12 --> `seq` can be used
            time[ti] <- seq(origin, by=paste0(dts[ti], " months"), l=2)[2]
        } else if (F) { # dt in months is not integer, e.g. 0.5 --> `seq` can not be used
            full_month <- floor(dts[i]) # e.g. 0 or 12
            rest <- dts[ti] %% 1 # e.g. 0.5
            if (full_month == 0) { # use only origin + rest 
                time[ti] <- a
            } else { # full_month is not 0
               a <- seq(origin, by=paste0(dts[ti], " months"), l=2)[2]
            }
        }
    }
}

# minute/second degree to decimal degree longitude/latitude
deg2dec <- function(deg=0.0, min=0.0, sec=0.0, verbose=F) {
    if (verbose) { 
        if (length(deg) == 1 && length(min) == 1 && length(sec) == 1) {
            message("deg2dec(): dec = deg + min/60 + sec/3600 = ", deg, " + ", min, "/60 + ", sec, "/3600")
        } else {
            message("deg2dec(): dec = deg + min/60 + sec/3600")
        }
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
convert_lon_360_to_180 <- function(nc_file, nc_out, nc_varname, lon360, data360, londimind) {
    # input:
    #   if `nc_file` and `nc_out` (and optionally nc_varname`):
    #       load lon360 from nc dims (and optionally data360 form `nc_varname`) from nc file
    #   else if `lon360`:
    #       work on provided numeric lon360 (and optionally data360) data directly
    if (missing(nc_file) && missing(lon360)) {
        stop("provide either `nc_file` or `lon360`")
    }
    if (!missing(nc_file)) { # case 1: user provided nc file and not data
        if (missing(nc_out) || is.null(nc_out)) stop("`nc_file` was provided but not `nc_out`")
        dir.create(dirname(nc_out), recursive=T, showWarnings=F)
        if (file.access(dirname(nc_out), mode=2) == -1) { # not writeable
            stop("directory of `nc_out` = \"", nc_out, "\" not writable")
        }
        if (file.exists(nc_file) && file.access(nc_file, mode=4) != -1) { # file exists and is readable
            message("load ncdf4 package and run ncdf4::nc_open() on `nc_file` = \"", nc_file, "\" ...")
            library(ncdf4)
            nc <- ncdf4::nc_open(nc_file)
            known_lon_dimnames <- c("lon", "lons", "longitude")
            message("check nc dims for \"lon\", \"lons\", \"longitude\" ...")
            # find and load lons
            if (any(!is.na(match(known_lon_dimnames, names(nc$dim))))) {
                lonind <- which(!is.na(match(known_lon_dimnames, names(nc$dim))))
                if (length(lonind) != 1) {
                    stop("found ", length(lonind), " matching dims: ",
                         "\"", paste(known_lon_dimnames[lonind], collapse="\", \""), "\"",
                         " --> only one allowed")
                }
                lonind <- which(names(nc$dim) == c(known_lon_dimnames[lonind]))
                message("--> use \"", names(nc$dim)[lonind], "\" dim with ", nc$dim[[lonind]]$len, " values from ",
                        paste(head(nc$dim[[lonind]]$vals), collapse=", "), " to ",
                        paste(tail(nc$dim[[lonind]]$vals), collapse=", "), " as lon360 dim ...")
                lon360 <- nc$dim[[lonind]]$vals
            } else {
                stop("found not any dimension having one of those names")
            }

            # check nc_varname
            if (missing(nc_varname) || is.null(nc_varname)) {
                message("`nc_varname` not provided --> use all variables from nc file")
                nc_varname <- names(nc$var)
            } else {
                if (typeof(nc_varname) != "character") stop("provided `nc_varname` must be of type \"character\"")
                nc_varname <- strsplit(nc_varname, ",")[[1]] # if provided as "var1,var2"
            }

            # load nc_varname from nc_file
            data360 <- vector("list", l=length(nc_varname))
            for (vi in seq_along(nc_varname)) {
                message("check if `nc_varname[", vi, "]` \"", nc_varname[vi], "\" has lon dim ...")
                varind <- which(nc_varname[vi] == names(nc$var))
                if (length(varind) != 1) {
                    stop("this varname was not identified once from available nc varnames \"", 
                         paste(names(nc$var), collapse="\", \""), "\"")
                }
                lonid <- nc$dim[[lonind]]$id
                dims_of_var <- nc$var[[varind]]$dimids
                londimind <- which(dims_of_var == lonid)
                if (length(londimind) == 0) { # not any dim of current var is the lon dim
                    message("--> did not find lon id ", lonid, " in ", nc_varname[vi], 
                            " dimids ", paste(dims_of_var, collapse=", "), " --> skip this var")
                } else if (length(londimind) == 1) { # current var has a dim that is the lon dim
                    message("--> dims(", nc_varname[vi], ") = ", paste(dim(data360), collapse=", "), 
                            " --> londimind = ", londimind)
                    message("--> load ", nc_varname[vi], " from nc file via ncdf4::ncvar_get() ...")
                    data360[[vi]] <- ncdf4::ncvar_get(nc, nc_varname[vi])
                    names(data360)[vi] <- nc_varname[vi]
                    attributes(data360[[vi]]) <- list(varind=varind, dim=dim(data360[[vi]]))
                } else {
                    stop("--> found ", length(londimind), " lon dims with id ", lonid, " in ", nc_varname[vi], 
                         " dimids ", paste(dims_of_var, collapse=", "), ". should only be one lon dim.")
                }
            } # for vi in nc_varname
            
            # remove variables without lon dim
            inds <- which(sapply(data360, is.null))
            if (length(inds) > 0) {
                if (length(inds) == length(data360)) {
                    stop("non of the variables ", paste(nc_varname, collapse=", "), 
                         " has a lon dim")
                } else {
                    data360 <- data360[-inds]
                }
            }

        } else { # provided nc file not readable
            stop("provided `nc_file` = \"", nc_file, "\" does not exist as file or is not readable")
        }
    
    } else { # case 2: user provided lon360 (and data360) data and not nc file
        # check input data
        if (!is.numeric(lon360)) stop("`lon360` must be numeric")
        if (!missing(data360)) { # if data360 was provided
            if (!is.list(data360)) data360 <- list(data360)
            for (vi in seq_along(data360)) {
                if (!is.numeric(data360[[vi]])) stop("`data360[[", vi, "]]` is not numeric")
                if (missing(londimind)) { # londimind not provided
                    message("`londimind` not provided --> try to determine from ",
                            "length(lon360) = ", length(lon360), "; dims(data360) = (", 
                            paste(dim(data360[[vi]]), collapse=","), ") ... ", appendLF=F)
                    londimind <- which(dim(data360[[vi]]) == length(lon360))
                    if (length(londimind) != 1) {
                        stop("number of longitudes was found ", length(ind), " times in data dims")
                    } else {
                        message("--> londimind = ", londimind)
                    }
                } else { # if londimind was provided
                    if (!is.numeric(londimind)) stop("`londimind` must be numeric")
                }
            } # for vi variables
        } else { # if data360 was not provided
            data360 <- NULL
        }
    } # if nc_file or data360 was provided
                   
    # start converting from 0,360 --> -180,180
    ge0_and_lt180_inds <- which(lon360 >= 0 & lon360 < 180)
    ge180_and_lt360_inds <- which(lon360 >= 180 & lon360 < 360)
    ge360_inds <- which(lon360 >= 360)
    lon180 <- c()
    if (length(ge180_and_lt360_inds) > 0) lon180 <- c(lon180, lon360[ge180_and_lt360_inds] - 360)
    if (length(ge360_inds) > 0) lon180 <- c(lon180, lon360[ge360_inds] - 360)
    if (length(ge0_and_lt180_inds) > 0) lon180 <- c(lon180, lon360[ge0_and_lt180_inds])
    if (length(lon180) != length(lon360)) stop("length(lon180) != length(lon360). sth wrong")
    if (all(lon180 == lon360)) message("--> lon180 = lon360. no conversion necessary")
    ret <- list(lon180=lon180)
    if (!is.null(data360)) {
        if (all(lon180 == lon360)) { # no conversion necessary
            ret$data180 <- data360 
        } else { # conversion necessary
            data180 <- vector("list", l=length(data360))
            names(data180) <- names(data360)
            for (vi in seq_along(data180)) {
                arr360 <- as.array(data360[[vi]]) # any dims
                cmd <- paste0("data180[[", vi, "]] <- abind::abind(")
                cmd1 <- cmd2 <- cmd3 <- NA
                verbose <- list()
                if (length(ge180_and_lt360_inds) > 0) {
                    tmp <- rep(",", t=length(dim(arr360))) 
                    tmp[londimind] <- "ge180_and_lt360_inds"
                    tmp <- paste(tmp, collapse="")
                    cmd1 <- paste0("arr360[", tmp, "]")
                    verbose[[length(verbose)+1]] <- ge180_and_lt360_inds
                }
                if (length(ge360_inds) > 0) {
                    tmp <- rep(",", t=length(dim(arr360))) 
                    tmp[londimind] <- "ge360_inds"
                    tmp <- paste(tmp, collapse="")
                    cmd2 <- paste0("arr360[", tmp, "]")
                    verbose[[length(verbose)+1]] <- ge360_inds
                }
                if (length(ge0_and_lt180_inds) > 0) {
                    tmp <- rep(",", t=length(dim(arr360))) 
                    tmp[londimind] <- "ge0_and_lt180_inds"
                    tmp <- paste(tmp, collapse="")
                    cmd3 <- paste0("arr360[", tmp, "]")
                    verbose[[length(verbose)+1]] <- ge0_and_lt180_inds
                }
                if (!is.na(cmd1)) cmd <- paste0(cmd, cmd1, ", ")
                if (!is.na(cmd2)) cmd <- paste0(cmd, cmd2, ", ")
                if (!is.na(cmd3)) cmd <- paste0(cmd, cmd3, ", ")
                cmd <- paste0(cmd, "along=", londimind, ")")
                message("run `", cmd, "`")
                if (vi == 1) {
                    library(abind)
                    for (i in seq_along(verbose)) {
                        if (!is.null(verbose[[i]])) {
                            message(length(lon360[verbose[[i]]]), " lons ", 
                                    paste(head(lon360[verbose[[i]]]), collapse=","), ",...,",
                                    paste(tail(lon360[verbose[[i]]]), collapse=","),
                                    " (inds ", paste(head(verbose[[i]]), collapse=","), ",...,",
                                    paste(tail(verbose[[i]]), collapse=","), ")")
                        }
                    } # for i lon chunks
                } # if vi == 1
                eval(parse(text=cmd))
                dimnames(data180[[vi]]) <- NULL
            } # for vi nc_varname
            ret$data180 <- data180
        } # if lon180 == lon360 or not
    } # if !is.null(data)
    
    # save converted data180 as nc file
    if (!missing(nc_file)) { 
        message("save `nc_out` = \"", nc_out, "\" ...")
        ncdims <- nc$dim
        ncdims[[lonind]]$vals <- lon180
        ncvars <- nc$var # save at least all input variables 
        if (!is.null(data360)) { 
            for (vi in seq_along(data360)) { # replace old 360 with new 180 lon vals
                varind <- attributes(data360[[vi]])$varind
                ncvars[[varind]]$dim[[londimind]]$vals <- lon180
            }
        }

        # put variables to nc
        outnc <- ncdf4::nc_create(filename=nc_out, force_v4=T, vars=ncvars)
        for (vi in seq_along(ncvars)) { # for all vars of input file
            if (!is.null(data360) && any(names(ncvars)[vi] == names(data360))) { # save converted data 
                message("variable ", names(ncvars)[vi], ": save converted values")
                ind <- which(names(ncvars)[vi] == names(data360))
                ncdf4::ncvar_put(nc=outnc, varid=ncvars[[vi]], vals=data180[[ind]])
            } else { # keep all other untouched
                message("variable ", names(ncvars)[vi], ": save original values")
                ncdf4::ncvar_put(nc=outnc, varid=ncvars[[vi]], vals=ncdf4::ncvar_get(nc, names(ncvars)[vi]))
            }
        }

        # copy all variable/global attributes from input
        for (vi in seq_along(ncvars)) {
            atts <- ncdf4::ncatt_get(nc, names(ncvars)[vi])
            if (length(atts) > 0) {
                for (i in seq_along(atts)) {
                    #message(names(atts)[i])
                    if (names(atts)[i] == "_FillValue") {
                        # Error in ncatt_put, while writing attribute _FillValue with value 1.00000002004088e+20
                        # was already set earlier
                    } else {
                        ncdf4::ncatt_put(outnc, names(ncvars)[vi], names(atts)[i], atts[[i]])
                    }
                }
            }
        }
        global_atts <- ncdf4::ncatt_get(nc, 0)
        if (length(global_atts) > 0) {
            for (i in seq_along(global_atts)) {
                ncdf4::ncatt_put(outnc, 0, names(global_atts)[i], global_atts[[i]])
            }
        }
        ncdf4::nc_close(outnc)
   
        # select correct grid
        cmd <- paste0("cdo selgrid,2 ", nc_out, " ", 
                      dirname(nc_out), "/", basename(nc_out), ".tmp && ",
                      "mv ", dirname(nc_out), "/", basename(nc_out), ".tmp ", nc_out)
        message("run `", cmd, "` ...")
        system(cmd)

    # return converted data180 
    } else { 
        return(ret)
    }
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
    lm_summary <- summary(lm)
    # from ?lm.summary:
    # fstatistic: (for models including non-intercept terms) a 3-vector with
    #             the value of the F-statistic with its numerator and
    #             denominator degrees of freedom.
    r_lm.summary <- stats::pf(lm_summary$fstatistic[1], lm_summary$fstatistic[2], lm_summary$fstatistic[3], lower.tail=F)
    # model[[1]] is actual data that was modeled by predictors model[[2..]]
    a <- sum((lm$model[[1]]-mean(lm$model[[1]]))^2) 
    b <- sum(lm$res^2)
    n_predictors <- lm$rank-1
    n_df <- lm$df.residual
    c <- (a-b)/n_predictors/(b/n_df)
    1-pf(c, n_predictors, n_df)
} # get_pval

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

# model resolutions
res_km_from_nx_ny <- function(nx, ny) {
    # boucher et al. 2020 https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019MS002010
    Rearth <- 6371 # km 
    sqrt(4*pi*Rearth^2/(nx*ny))
}

# convert velocities with units package
speeds <- function(x=1, unit="cm/s") {
    library(units) # valid_udunits()
    x <- set_units(x=x, value=unit, mode="standard")
    lengths <- c("0.1mm", "mm", "cm", "10cm", "m", "10m", "100m", "km", "3km", "10km", "100km", "1000km")
    times <- c("s", "min", "h", "day", "month", "year", "kyear", "Myear")
    vec <- as.vector(outer(lengths, times, paste, sep="/"))
    for (i in seq_along(vec)) vec[i] <- set_units(x, value=vec[i], mode="standard")
    mat <- matrix(as.numeric(vec), nrow=length(lengths))
    rownames(mat) <- lengths; colnames(mat) <- times
    oo <- getOption("scipen") # oldoption
    options("scipen"=100); message("scipen=", getOption("scipen"))
    print(mat)
    options("scipen"=oo); message("scipen=", getOption("scipen"))
    invisible(mat)
} # speeds function

# convert masses with units package
masses <- function(x=1, unit="kg") {
    library(units) # valid_udunits()
    x <- set_units(x=x, value=unit, mode="standard")
    units <- data.frame(unit=c("g", "kg", "t", "Mt",               "Gt",                                    "Tg",                                     "Pg",      "Tt"),
                        note=c("",  "",   "",  "mega 6 (million)", "giga 9 (billion short, milliard long)", "tera 12 (trillion short, billion long)", "peta 15", "tera 12"))  
    vals <- units$unit
    for (i in seq_along(vals)) vals[i] <- set_units(x, value=units$unit[i], mode="standard")
    df <- data.frame(value=vals, unit=units$unit, note=units$note, stringsAsFactors=F)
    print(df)
    invisible(df)
} # masses function

# convert carbon units
kgC_m2_s1_to_gC_m2_yr1 <- function(kgC_m2_s1) {
    kgC_m2_s1 * 365.25*86400 * 1e3 # s-1 -> yr-1; kg -> g 
}
kgC_m2_s1_to_PgC_yr1 <- function(kgC_m2_s1) {
    Aearth <- 5.100656e14 # m2
    kgC_m2_s1 * Aearth * 365.25*86400 / 1e12 # m2 -> fldint; s-1 -> yr-1; kg -> Pg 
}
kgC_s1_to_PgC_yr1 <- function(kgC_s1) {
    kgC_s1 * 365.25*86400 / 1e12 # s-1 -> yr-1; kg -> Pg 
}
kgCO2_m2_s1_to_PgC_yr1 <- function(kgCO2_m2_s1) {
    Aearth <- 5.100656e14 # m2
    kgCO2_m2_s1 * Aearth * 0.272912 * 365.25*86400 / 1e12 # m2 -> fldint; kgCO2 -> kgC; s-1 -> yr-1; kg -> Pg 
}
kgCO2_m2_to_ppm <- function(kgCO2_m2) {
    Aearth <- 5.100656e14 # m2
    kgCO2_m2 * Aearth * 0.272912 / 1e12 / 2.124 # m2 -> fldint; kgCO2 -> kgC; kg -> Pg ; PgC --> ppm
}
kgCO2_m2_to_PgC <- function(kgCO2_m2) {
    Aearth <- 5.100656e14 # m2
    kgCO2_m2 * Aearth * 0.272912 / 1e12 # m2 -> fldint; kgCO2 -> kgC; kg -> Pg 
}
kgCO2_s1_to_PgC_yr1 <- function(kgCO2_s1) {
    kgCO2_s1 * 0.272912 * 365.25*86400 / 1e12 # kgCO2 -> kgC; s-1 -> yr-1; kg -> Pg 
}
molC_m2_yr1_to_gC_m2_yr1 <- function(molC_m2_yr1) {
    molC_m2_yr1 * 12.0107 # molC --> gC
}
molC_to_kgCO2 <- function(molC) {
    molC * 12.0107 * 3.664191 / 1e3 # molC --> gC; gC --> gCO2; gCO2 --> kgCO2
}
molC_to_PgC <- function(molC) {
    molC * 12.0107 / 1e15 # molC --> gC; g -> Pg
}
molC_s1_to_PgC_yr1 <- function(molC_s1) {
    molC_s1 * 12.0107 * 365.25*86400 / 1e15 # molC --> gC; s-1 -> yr-1; g -> Pg
}
molCO2_m2_s1_to_PgC_yr1 <- function(molCO2_m2_s1) {
    Aearth <- 5.100656e14 # m2
    molCO2_m2_s1 * Aearth * 44.0095 * 0.272912 * 365.25*86400 / 1e15 # m2 -> fldint; molCO2 -> gCO2; gCO2 -> gC; s-1 -> yr-1; g -> Pg 
}
molCO2_s1_to_kgC_s1 <- function(molCO2_s1) {
    molCO2_s1 * 44.0095 * 0.272912 / 1e3 # molCO2 -> gCO2; gCO2 -> gC; gC -> kgC
}
molCO2_s1_to_PgC_yr1 <- function(molCO2_s1) {
    molCO2_s1 * 44.0095 * 0.272912 * 365.25*86400 / 1e15 # molCO2 -> gCO2; gCO2 -> gC; s-1 -> yr-1; g -> Pg 
}

## section 2/2: r-stuff

# return currently running R executable
Rexe <- function() return(paste0(R.home(), "/bin/exec/R"))

tryCatch.W.E <- function(expr) { # from `demo(error.catching)`
    # use e.g. like this: `type <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))`
    W <- NULL
    w.handler <- function(w) { # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value=withCallingHandlers(tryCatch(expr, error=function(e) e),
                                   warning=w.handler), 
         warning=W)
} # tryCatch.W.E

# find unbalanced (), [], {}-code blocks with a missing open or closed part
# from https://stat.ethz.ch/pipermail/r-help/2014-May/374864.html
check_unbalanced_braces <- function(file) {
    if (!file.exists(file)) {
        stop("`file` = \"", file, "\" does not exist")
    }
    message("check `file` = \"", file, "\" ... ", appendLF=F)
    text <- readLines(file)
    src <- srcfile(file)
    status <- tryCatch(parse(text=text, srcfile=src),
                       error=function(e) e,
                       warning=function(w) w)
    if (typeof(status) != "expression") { # some warning/error 
        d <- getParseData(src)
        tokens_to_check <- c("'{'", "'}'", "'['", "']'", "'('", "')'")
        message("found some warning/error --> check ", length(tokens_to_check), 
                " tokens for missing parents ...")
        for (i in seq_along(tokens_to_check)) {
            message("*****************************************************\n",
                    "token ", i, "/", length(tokens_to_check), ": \"", 
                    tokens_to_check[i], "\" ... ", appendLF=F)
            inds <- which(d$token == tokens_to_check[i] & d$parent == 0)
            if (length(inds) > 0) {
                message("--> found ", length(inds), " missing parents:")
                print(d[inds,])
            } else {
                message("--> found zero missing parents")
            }
        }
    } else {
        message("no warning/error found")
    }
} # check_unbalanced_braces()

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
    if (F) {
        message()
        message("sys.call() = ")
        print(sys.call())
        message("sys.calls() = ")
        print(sys.calls())
        message("sys.frame() = ")
        print(sys.frame())
        message("sys.nframe() = ")
        print(sys.nframe())
        message("sys.frames() = ")
        print(sys.frames())
        message("sys.function() = ")
        print(sys.function())
        message("sys.parent() = ")
        print(sys.parent())
        message("sys.parents() = ")
        print(sys.parents())
        message("parent.frame() = ")
        print(parent.frame())
        message("sys.status() = ")
        print(sys.status())
        message()
    } # F
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
} # thisFile()

# get memory of current r session
get_memory <- function(method="ps", verbose=F) {
    known_methods <- c("ps")
    if (is.na(match(method, known_methods))) {
        stop("`method` must be one of \"", paste(known_methods, collapse="\", \""), "\"")
    }
    pid <- Sys.getpid()
    if (method == "ps") {
        if (Sys.which("ps") == "") stop("program `ps` not found")
        cmd <- paste0("ps -o rss,vsz ", pid)
        if (verbose) message("run `", cmd, "` ...")
        mem <- system(cmd, intern = T)
        mem <- read.table(text=mem, header=T)
        mem <- unlist(mem) * 1024
        prettyMem <- sapply(mem, function(x) {
            class(x) <- "object_size"
            format(x, units = "auto")
        })
        mem <- data.frame(c("RSS physical", "VSZ virtual"), mem, 
            prettyMem)
        names(mem) <- c(paste0("`", cmd, "`"), "Mem [B]", "PrettyMem")
        message("pid ", pid, " ", paste(paste0(mem[,1], ": ", mem[,3]), collapse=", "))
    }
    #return(mem)
} # get_memory()

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
    # small changes based on FSA::headtail
    if (!(is.matrix(x) | is.data.frame(x))) 
        x <- as.data.frame(x)
    if ("tbl_df" %in% class(x)) 
        x <- as.data.frame(x)
    N <- nrow(x)
    if (n >= N)
        tmp <- x
    else {
        h <- utils::head(x, n=min(N/2, n), ...)
        if (is.null(rownames(x))) 
            rownames(h) <- paste0("[", seq_len(n), ",]")
        t <- utils::tail(x, n=min(N/2, n), keepnums=T, ...)
        tmp <- rbind(h, t)
    }
    print(t(tmp))
} # ht function

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
ncdump_get_filetype <- function(fin, ncdump=Sys.which("ncdump"), verbose=T) {
    if (verbose) message("ncdump_get_filetype() start with `verbose`=T ...")
    #fin <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/echam/historical2_201412.01_co2"
    #fin <- "/pool/data/JSBACH/input/r0010/T63/spitfire/population_density_ssp2_T63.nc"
    if (file.access(fin, mode=0) == -1) stop("fin = ", fin, " does not exist")
    if (file.access(fin, mode=4) == -1) stop("fin = ", fin, " not readable")
    if (!is.character(ncdump)) stop("ncdump = ", ncdump, " not of type character")
    if (Sys.which(ncdump) == "") stop("ncdump = ", ncdump, " not found")
    cmd <- paste0(ncdump, " -k ", fin)
    if (verbose) message("ncdump_get_filetype() run `", cmd, "` ...")
    type <- suppressWarnings(system(cmd, intern=T, ignore.stderr=T))
    if (!is.null(attributes(type))) { # no success
        # e.g.: "GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"
        file_type <- "non-nc"
    } else { # success
        # e.g: "classic"
        #      "netCDF", "NetCDF"
        #      "NetCDF2"
        #      "netCDF-4", "NetCDF4", "NetCDF4 zip"
        #      "NetCDF4 classic", "NetCDF4 classic zip", "netCDF-4 classic model"
        if (verbose) message("ncdump_get_filetype() --> \"", type, "\"")
        file_type <- "nc"
    }
    if (verbose) {
        message("ncdump_get_filetype() type = ", file_type)
        message("ncdump_get_filetype() finished")
    }
    return(file_type)
} # ncdump_get_filetype

# set my default plot options
myDefaultPlotOptions <- function(plist=list(plot_type="png", 
                                            ts_width_in=7, ts_asp=2,
                                            ts_mon_width_in=7, ts_mon_asp=1,
                                            depth_width_in=7, depth_asp=0.5,
                                            map_width_in=7, map_asp=4/3,
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
        if (verbose) message("*** myDefaultPlotOptions() start ***")
        for (i in seq_along(dot_list)) {
            if (!is.null(dot_list[[i]])) {
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
            } # if !is.null(dot_list[[i]])
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
    if (is.logical(asp)) {
        if (asp) {
            asp <- 4/3 # default
        } else if (!asp) {
            asp <- 1 # no asp; height=width if one is missing
        }
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
    pointsize <- 12 # default
    # png pointsize: the default pointsize of plotted text, interpreted as big
    #                points (1/72 inch) at ‘res’ ppi.
    # pdf pointsize: the default point size to be used.  Strictly speaking, in
    #                bp, that is 1/72 of an inch, but approximately in points.
    #                Defaults to ‘12’.
    # 1 pb = 1/72 inch = 0.01388889 inch = 0.35277780599999997 mm (1 inch = 2.54 cm)
    # 12/72 inch = 0.1666667 inch = 4.233333 mm (1 inch = 2.54 cm)
    #                  300 pixel   1  inch   
    # --> 1 point uses --------- x ------- = 4.166667 pixel at 300 ppi resolution
    #                  1 inch      72
    
    # increase png pointsize (default = 12) if 1 < asp < 2
    # ragg package: res_mod(scaling * res / 72.0),
    #               lwd_mod(scaling * res / 96.0),
    if (asp > 1 && asp < 2) {
        if (T) {
            asp_interp <- approx(x=c(1, 2), y=c(1, 1.7), n=100) # linearly interp asp 1 to 2 --> fac 1 to 1.5
            asp_fac_ind <- which.min(abs(asp_interp$x - asp))
            asp_fac <- asp_interp$y[asp_fac_ind]
        } else if (F) {
            asp_fac <- asp
        }
        message("asp = ", round(asp, 3), " --> multiply pointsize ", 
                pointsize, " * asp_fac ", round(asp_fac, 3), " = ", 
                round(pointsize*asp_fac, 3))
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

# show font info
font_info <- function() {

    # from ?png:
    # type: character string, one of '"Xlib"' or '"quartz"' (some macOS
    #       builds) or '"cairo"'.  The latter will only be available if
    #       the system was compiled with support for cairo - otherwise
    #       '"Xlib"' will be used.  The default is set by
    #       'getOption("bitmapType")' - the 'out of the box' default is
    #       '"quartz"' or '"cairo"' where available, otherwise '"Xlib"'.
    type <- getOption("bitmapType")
    message("\ngetOption(\"bitmapType\"): ", type)

    # from ?png:
    # For types '"cairo"' and '"quartz"', the 'family' argument can
    # be supplied.  See the 'Cairo fonts' section in the help for
    # 'X11'.
    # For type '"cairo"', the 'symbolfamily' argument can be
    # supplied.  See 'X11.options'.

    # from grDecives::X11.options():
    x11_opts <- grDevices::X11.options()
    message("\nX11.options:")
    cat(capture.output(str(x11_opts)), sep="\n")

    # from grDevices::X11():
    fonts <- get(".X11.Fonts", envir=grDevices:::.X11env)
    message("\n.X11.Fonts:")
    cat(capture.output(str(fonts)), sep="\n")

    # plot x11 fonts
    message("\nplot ", length(fonts), " x11 fonts ...")
    plotnames <- rep(NA, t=length(fonts))
    for (fi in seq_along(fonts)) {
        plotnames[fi] <- paste0("x11_font_", names(fonts)[fi], "_", gsub(" ", "_", gsub("[[:punct:]]", "_", R.version.string)), "_", Sys.info()["nodename"], ".png")
        message("plot ", plotnames[fi], " ...")
        png(plotnames[fi], width=1000, height=1000, res=300, family=names(fonts)[fi])
        plot(1:10, 1:10, t="n", main=names(fonts)[fi])
        text(1:10, 1:10, paste0(R.version.string, " ", Sys.info()["nodename"]))
        dev.off()
    } # for fi

    nm <- grDevices::n2mfrow(length(fonts))
    plotname <- paste0(gsub(" ", "_", gsub("[[:punct:]]", "_", R.version.string)), "_", Sys.info()["nodename"], "_", length(fonts), "_x11_fonts.png")
    cmd <- paste0("magick montage -label %f ", paste(plotnames, collapse=" "),
                  " -geometry 1500x1500 -frame ", nm[1], " -tile 1x", nm[2],
                  " miff:- | magick montage - -geometry +0+0 -tile ", nm[1], "x1 ", plotname)
    message("\nrun `", cmd, "` ...")
    system(cmd)
    message("--> check combined plot ", plotname)
    message("\nremove ", length(plotnames), " single plots ...")
    invisible(file.remove(plotnames))

    # get all available fonts via fc-list
    message("\nget all available fc-list fonts ...")
    # figure out which font lib is used: default or non-default (R compiled against specific systemfonts.so):
    # # default:
    # fc-list | grep "Sans" | grep ":style=Regular"
    # # non-default:
    # ```
    # unalias R
    # which R: /sw/spack-levante/r-4.1.2-eprwea/bin/R
    # ldd $(dirname $(dirname $(which R)))/rlib/R/library/systemfonts/libs/systemfonts.so | grep font
    # libfontconfig.so.1 => /sw/spack-levante/fontconfig-2.13.94-vzwyua/lib/libfontconfig.so.1 (0x00007f69ec088000)
    #                       /sw/spack-levante/fontconfig-2.13.94-vzwyua/bin/fc-list | grep "Sans" | grep ":style=Regular"
    # ```
    # todo: https://github.com/r-lib/systemfonts/issues/99: unique(dirname(systemfonts::system_fonts()$path))
    systemfonts_so <- paste0(R.home(), "/library/systemfonts/libs/systemfonts.so")
    if (!file.exists(systemfonts_so)) { # default
        message("did not find systemfonts.so in `R.home()` --> use default fc-list ...")
        fc_list <- Sys.which("fc-list")
    } else { # non-default: R was compiled against specific systemfonts.so
        message("found systemfonts.so in `R.home()`: ", systemfonts_so, " --> find specific fc-list ...")
        cmd <- paste0("ldd ", systemfonts_so, " | grep font")
        ldd <- system(cmd, intern=T) # e.g. "\tlibfontconfig.so.1 => /sw/spack-levante/fontconfig-2.13.94-vzwyua/lib/libfontconfig.so.1 (0x00007f1182051000)"
        if (length(ldd) != 1) stop("ldd result must be of length 1")
        fontconfig <- strsplit(ldd, " => ")[[1]][2] # e.g. "/sw/spack-levante/fontconfig-2.13.94-vzwyua/lib/libfontconfig.so.1 (0x00007f1182051000)"
        fontconfig <- strsplit(fontconfig, " ")[[1]][1]
        fc_list <- paste0(dirname(dirname(fontconfig)), "/bin/fc-list")
    }
    if (!file.exists(fc_list)) stop("could not find ", fc_list)
    fonts <- system(fc_list, intern=T) # e.g. "/sw/spack-levante/font-util-1.3.2-m5qnf5/share/fonts/X11/100dpi/timB18-ISO8859-1.pcf.gz: Times:style=Bold"
    fonts <- fonts[which(!duplicated(sapply(basename(fonts), "[", 1)))] # throw out duplicated .ttf/.gz entries
    fonts <- strsplit(fonts, ":") # e.g. "/sw/spack-levante/font-util-1.3.2-m5qnf5/share/fonts/X11/100dpi/timB18-ISO8859-1.pcf.gz" " Times" "style=Bold"
    fontnames <- trimws(sapply(fonts, "[", 2))
    fontstyles <- gsub("style=", "", trimws(sapply(fonts, "[", 3)))
    
    # plot all "*Sans*" "Regular" fonts
    message("\ncheck fc-list fonts for \"*Sans*\" and style=\"Regular\" fonts from `fc-list` ...")
    inds1 <- grep("Sans", fontnames)
    inds2 <- which(fontstyles == "Regular")
    inds <- intersect(inds1, inds2)
    if (length(inds) == 0) {
        message("\nfound zero \"*Sans*\" and \"Regular\" fonts from `fc-list`. cannot plot any")
    } else { # found some "*Sans*" and "Regular" fonts
        fonts <- fonts[inds]; fontnames <- fontnames[inds]; fontstyles <- fontstyles[inds]
        if (any(duplicated(fontnames))) {
            inds <- which(duplicated(fontnames))
            message("found ", length(inds), " duplicated \"*Sans*\" and \"Regular\" fonts from fc-list:")
            for (fi in seq_along(inds)) {
                cat(capture.output(str(fonts[which(fontnames == fontnames[inds[fi]])])), sep="\n")
                message("--> take 1st entry")
                rminds <- which(fontnames == fontnames[inds[fi]])
                rminds <- rminds[2:length(rminds)]
                fonts[rminds] <- NA
            } # for fi
            fontnames <- fontnames[which(!is.na(fonts))]
            fontstyles <- fontstyles[which(!is.na(fonts))]
            fonts <- fonts[which(!is.na(fonts))]
        } # if (any(duplicated(fontnames))) {

        message("\nplot ", length(fonts), " \"*Sans*\" and \"Regular\" fonts from `fc-list` ...")
        plotnames <- rep(NA, t=length(fonts))
        for (fi in seq_along(fonts)) {
            fontname <- gsub(" ", "_", fontnames[fi])
            plotnames[fi] <- paste0("fc_list_font_", fontname, "_", gsub(" ", "_", gsub("[[:punct:]]", "_", R.version.string)), "_", Sys.info()["nodename"], ".png")
            message("plot ", plotnames[fi], " ...")
            png(plotnames[fi], width=1000, height=1000, res=300, family=fontnames[fi])
            plot(1:10, 1:10, t="n", main=fontnames[fi])
            text(1:10, 1:10, paste0(R.version.string, " ", Sys.info()["nodename"]))
            dev.off()
        } # for fi

        nm <- grDevices::n2mfrow(length(fonts))
        plotname <- paste0(gsub(" ", "_", gsub("[[:punct:]]", "_", R.version.string)), "_", Sys.info()["nodename"], "_", length(fonts), "_fc_list_fonts.png")
        cmd <- paste0("magick montage -label %f ", paste(plotnames, collapse=" "),
                      " -geometry 1500x1500 -frame ", nm[1], " -tile 1x", nm[2],
                      " miff:- | magick montage - -geometry +0+0 -tile ", nm[1], "x1 ", plotname)
        message("\nrun `", cmd, "` ...")
        system(cmd)
        message("--> check combined plot ", plotname)
        message("\nremove ", length(plotnames), " single plots ...")
        invisible(file.remove(plotnames))
    } # if (length(inds) > 0) {

    message("\nfinished")

} # font_info

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

my_install.packages <- function(pkg="asdasd", dry=T) {
    
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
        
        # R36 mistral
        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "3.6") {
            message("units package defined on mistral for r 3.6")
            udunits2_path <- "/sw/spack-rhel6/udunits2-2.2.24-74lq3k"
            #LD_LIBRARY_PATH <- paste0(udunits2_path, "/lib") # not recommended
            configure.args <- paste0("--with-udunits2-include=", udunits2_path, "/include ",
                                     "--with-udunits2-lib=", udunits2_path, "/lib")
            configure.vars <- paste0("LIBS=-Wl,-rpath,", udunits2_path, "/lib")
        }

    } else if (pkg == "sf") {
        
        # R36 mistral
        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "3.6") {
            gdal_path <- "/sw/spack-rhel6/gdal-3.1.3-f7koyc"
            proj_path <- "/sw/spack-rhel6/proj-7.1.0-w57onb"
            geos_path <- "/sw/spack-rhel6/geos-3.8.1-ru2zkr"
            #LD_LIBRARY_PATH <- paste0(gdal_path, "/lib") # not recommended
            configure.args <- paste0("--with-gdal-config=", gdal_path, "/bin/gdal-config ",
                                     "--with-proj-include=", proj_path, "/include ",
                                     "--with-proj-lib=", proj_path, "/lib ",
                                     "--with-geos-config=", geos_path, "/bin/geos-config")
            stop("todo")
            #configure.vars <- paste0("LIBS=-Wl,-rpath,", udunits2_path, "/lib")
        }
   
    } else if (pkg == "XML") {

        # R41 mistral
        if (any(sapply(c("mlogin", "mistralpp", "m[0-9][0-9][0-9][0-9][0-9]"), grepl, hostname)) &&
            substr(rversion, 1, 3) == "4.1") {
            libxml2_path <- "/sw/spack-rhel6/miniforge3-4.9.2-3-Linux-x86_64-pwdbqi"
            configure.vars <- paste0("LIBS=-Wl,-rpath,", libxml2_path, "/lib")
        }

    } # which package
    
    # checks
    if (!is.character(lib)) stop("`lib` must be of type character")
    if (!is.null(configure.args)) if (!is.character(configure.args)) stop("`configure.args` must be of type character")
    if (!is.null(configure.vars)) if (!is.character(configure.vars)) stop("`configure.vars` must be of type character")

    # add user LD_LIBRARY_PATH; not recommended
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

    # remove LD_LIBRARY_PATH duplicates
    LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH")
    LD_LIBRARY_PATH <- sort(unique(strsplit(LD_LIBRARY_PATH, ":")[[1]]))
    LD_LIBRARY_PATH <- paste(LD_LIBRARY_PATH, collapse=":")
    Sys.setenv(LD_LIBRARY_PATH=LD_LIBRARY_PATH)
    
    # get LD_LIBRARY_PATH with user paths if provided in alphabetical order and without duplicates
    LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH") # usage not recommended
    
    # verbose 
    msg <- paste0("install package \"", pkg, "\" on ", hostname, "\n",
                  "to path \"", lib, "\"\n",
                  "with ", R.version.string, "\n",
                  "R binary = ", rbin, "\n",
                  "shared lib paths of this binary =\n",
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
    ret <- list(pkg=pkg, lib=lib,
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
    message("\nfinished")
    return(ret)
    
} # my_install.packages function

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

# draw secret christmas present lists
xmas_secret_present_list <- function(persons, verbose=F) {
    
    if (interactive()) {
        persons <- list("wakenvillagers"=c("LeV", "Meck", "Malle", "Mörl", "Mattis"))
    } else {
        if (missing(persons)) stop("must provide persons")
        if (!is.list(persons)) stop("persons must be a named list")
        if (is.null(names(persons))) stop("persons must be named list")
    }

    # do for all provided person lists
    for (li in seq_along(persons)) {
        if (!is.character(persons[[li]])) stop("persons[[", li, "]] not of type character")
        
        # draw the samples
        senders <- sample(persons[[li]])
        recipients <- sample(persons[[li]])
        while (any(senders == recipients)) {
            recipients <- sample(persons[[li]])
        }

        # sort as in beginning
        inds <- rep(NA, t=length(persons[[li]]))
        for (i in seq_along(persons[[li]])) {
            inds[i] <- which(recipients == persons[[li]][i])
        }
        recipients <- recipients[inds]
        senders <- senders[inds]

        # show result
        res <- paste0(recipients, ": ", senders) 
        if (verbose) message(paste(res, collapse="\n"))

        # save result
        fout <- paste0(names(persons)[li], ".txt")
        message("save ", fout)
        write(res, fout, sep="\n")
        cmd <- paste0("chmod 600 ", fout)
        message("run `", cmd, "` ...")
        system(cmd)
    } # for li

} # xmas_secret_present_list

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
             "               rgdal:",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/gdal-2.1.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  install.packages(\"rgdal\", configure.args=\"--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config --with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include --with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib\")",
             "               rgeos:",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/geos-3.6.1-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  install.packages(\"rgeos\", configure.args=\"--with-geos-config=/sw/rhel6-x64/geos-3.4.2-gcc48/bin/geos-config\")",
             "               proj4:",
             "                  install.packages(\"proj4\", configure.args=\"--with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include\")",
             "               s2 (intel compiler):",
             "                  module load R(intel) intel.compiler gcc",
             "                  withr::with_makevars(list(CXX11STD=\"-std=c++11\"), install.packages(\"s2\"))",
             "               units:",
             "                  Sys.setenv(LD_LIBRARY_PATH=paste0(\"/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib:\", Sys.getenv(\"LD_LIBRARY_PATH\")))",
             "                  install.packages(\"units\", configure.args=\"--with-udunits2-include=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/include --with-udunits2-lib=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib\")",
             "                  maybe reinstall Rcpp",
             "               sf:",
             "                  install.packages(\"sf\", configure.args=\"--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config --with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include --with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib --with-geos-config=/sw/rhel6-x64/geos-3.6.1-gcc48/bin/geos-config\")",
             "                  maybe module load gdal",
             "               rJava:",
             "                  install default-jre on system, then `R CMD javareconf`",
             "                  better use a package without java-dependence: openxlsx or readxl",
             "               colorout:",
             "                  https://github.com/jalvesaq/colorout/releases or",
             "                  devtools::install_github(\"jalvesaq/colorout\")",
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


