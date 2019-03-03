myccf <- function (x, y, lag.max = NULL, type = c("correlation", "covariance"), 
    plot = TRUE, na.action = na.fail, ...) 
{
    #print("myccf")
    type <- match.arg(type)
    if (is.matrix(x) || is.matrix(y)) 
        stop("univariate time series only")
    X <- ts.intersect(as.ts(x), as.ts(y))
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    acf.out <- acf2(X, lag.max = lag.max, plot = FALSE, type = type, 
        na.action = na.action)
    #print(acf.out$acf[,1,1])
    #print(acf.out$acf[,2,2])
    #print("own ccf.r(): take serial correlation into account")
    #acf.out$x.ar <- arima(X[,1], order=c(p=1, d=0, q=0), method="ML")$coef[1]
    #acf.out$y.ar <- arima(X[,2], order=c(p=1, d=0, q=0), method="ML")$coef[1]
    #print(paste0("AR1(x)=", round(acf.out$x.ar, 3), ", AR1(y)=", round(acf.out$y.ar, 3)))
    acf.out$x.ar <- acf.out$acf[2,1,1]
    acf.out$y.ar <- acf.out$acf[2,2,2]
    #print(str(acf.out))
    lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
    y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
    acf.out$acf <- array(y, dim = c(length(y), 1L, 1L))
    acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot(acf.out, ...) # cannot tell plot() to use plot.acf2 :/
        return(invisible(acf.out))
    }
    else return(acf.out)
}

acf2 <- function (x, lag.max = NULL, type = c("correlation", "covariance",
    "partial"), plot = TRUE, na.action = na.fail, demean = TRUE,
    ...)
{
    #print("acf2")
    type <- match.arg(type)
    if (type == "partial") {
        m <- match.call()
        m[[1L]] <- quote(stats::pacf)
        m$type <- NULL
        return(eval(m, parent.frame()))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if (!is.numeric(x))
        stop("'x' must be numeric")
    sampleT <- as.integer(nrow(x))
    nser <- as.integer(ncol(x))
    if (is.na(sampleT) || is.na(nser))
        stop("'sampleT' and 'nser' must be integer")
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- as.integer(min(lag.max, sampleT - 1L))
    if (is.na(lag.max) || lag.max < 0)
        stop("'lag.max' must be at least 0")
    if (demean)
        x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin = FALSE)
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    # change here:
    #acf <- .Call(C_acf, x, lag.max, type == "correlation")
    acf <- .Call(stats:::C_acf, x, lag.max, type == "correlation")
    #
    lag <- outer(0:lag.max, lag/x.freq)
    acf.out <- structure(list(acf = acf, type = type, n.used = sampleT,
        lag = lag, series = series, snames = colnames(x)), class = "acf")
    #print(str(acf.out))
    if (plot) {
        plot.acf(acf.out, ...)
        invisible(acf.out)
    }
    else acf.out
}

# this overrides the deafult plot.acf()
plot.acf <- function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
    ylim = NULL, main = NULL, ci.col = "blue", ci.type = c("white",
        "ma"), max.mfrow = 6, ask = Npgs > 1 && dev.interactive(),
    mar = if (nser > 2) c(3, 2, 2, 0.8) else par("mar"), oma = if (nser >
        2) c(1, 1.2, 1, 1) else par("oma"), mgp = if (nser >
        2) c(1.5, 0.6, 0) else par("mgp"), xpd = par("xpd"),
    cex.main = if (nser > 2) 1 else par("cex.main"), verbose = getOption("verbose"),
    ...)
{
    #print("plot.acf")
    ci.type <- match.arg(ci.type)
    if ((nser <- ncol(x$lag)) < 1L)
        stop("x$lag must have at least 1 column")
    if (is.null(ylab))
        ylab <- switch(x$type, correlation = "ACF", covariance = "ACF (cov)",
            partial = "Partial ACF")
    if (is.null(snames <- x$snames))
        snames <- paste("Series ", if (nser == 1L)
            x$series
        else 1L:nser)
    with.ci <- ci > 0 && x$type != "covariance"
    with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
    if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
        warning("can use ci.type=\"ma\" only if first lag is 0")
        with.ci.ma <- FALSE
    }
    clim0 <- if (with.ci)
        qnorm((1 + ci)/2)/sqrt(x$n.used)
    else c(0, 0)
    ## change here
    ci_adjusted <- clim0 * sqrt((1+x$x.ar*x$y.ar)/(1-x$x.ar*x$y.ar))
    ##
    Npgs <- 1L
    nr <- nser
    if (nser > 1L) {
        sn.abbr <- if (nser > 2L)
            abbreviate(snames)
        else snames
        if (nser > max.mfrow) {
            Npgs <- ceiling(nser/max.mfrow)
            nr <- ceiling(nser/Npgs)
        }
        opar <- par(mfrow = rep(nr, 2L), mar = mar, oma = oma,
            mgp = mgp, ask = ask, xpd = xpd, cex.main = cex.main)
        on.exit(par(opar))
        if (verbose) {
            message("par(*) : ", appendLF = FALSE, domain = NA)
            str(par("mfrow", "cex", "cex.main", "cex.axis", "cex.lab",
                "cex.sub"))
        }
    }
    if (is.null(ylim)) {
        ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
        if (with.ci)
            ylim <- range(c(-clim0, clim0, ylim))
            # 
            #print(paste0("own plot.acf.r(): serial ci=qnorm(q=", 
            #             (1+ci)/2,
            #             ",mean=0,sd=1)/sqrt(n)*sqrt((1+AR1(x)*AR1(y))/(1-AR1(x)*AR1(y)))"))
            ylim <- range(ylim, -ci_adjusted, ci_adjusted)
            #
        if (with.ci.ma) {
            for (i in 1L:nser) {
                clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1,
                  i, i]^2)))
                ylim <- range(c(-clim, clim, ylim))
            }
        }
    }
    for (I in 1L:Npgs) for (J in 1L:Npgs) {
        dev.hold()
        iind <- (I - 1) * nr + 1L:nr
        jind <- (J - 1) * nr + 1L:nr
        if (verbose)
            message(gettextf("Page [%d,%d]: i =%s; j =%s", I,
                J, paste(iind, collapse = ","), paste(jind, collapse = ",")),
                domain = NA)
        for (i in iind) for (j in jind) if (max(i, j) > nser) {
            frame()
            box(col = "light gray")
        }
        else {
            clim <- if (with.ci.ma && i == j)
                clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j]^2)))
            else clim0
            plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
                ylab = if (j == 1)
                  ylab
                else "", ylim = ylim, ...)
            abline(h = 0)
            if (with.ci && ci.type == "white")
                #
                #abline(h = c(clim, -clim), col = ci.col, lty = 2)
                {
                    abline(h = c(clim, -clim), col = ci.col, lty = 2)
                    abline(h=c(ci_adjusted, -ci_adjusted), col=2)
                    #mtext(paste0("AR1(x)=", round(x$x.ar, 3), ", AR1(y)=", round(x$y.ar, 3)), line=0.5)
                    mtext(paste0("ACF1(x)=", round(x$x.ar, 3), ", ACF1(y)=", round(x$y.ar, 3)), line=0.5, cex=0.8)
                }
                #
            else if (with.ci.ma && i == j) {
                clim <- clim[-length(clim)]
                lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
                lines(x$lag[-1, i, j], -clim, col = ci.col, lty = 2)
            }
            title(if (!is.null(main))
                main
            else if (i == j)
                snames[i]
            else paste(sn.abbr[i], "&", sn.abbr[j]), line = if (nser >
                2)
                1
            else 2)
        }
        if (Npgs > 1) {
            mtext(paste("[", I, ",", J, "]"), side = 1, line = -0.2,
                adj = 1, col = "dark gray", cex = 1, outer = TRUE)
        }
        dev.flush()
    }
    invisible()
}


