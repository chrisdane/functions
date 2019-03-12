## R

## my collection of small R functions

# http://www.gettinggeneticsdone.com/2011/08/sync-your-rprofile-across-multiple-r.html
ht <- function(d) rbind(head(d, 10), tail(d, 10))
hh <- function(d) d[1:5, 1:5]
# Get the proportion variation explained. See this website for more details: http://goo.gl/jte8X
rsq <- function(predicted, actual) 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)

# Get month names in specific locale
mymonth.abb <- function(inds, locales=Sys.getlocale("LC_TIME")) {
  
    if (any(!(inds %in% 1:12))) {
        stop("mymonth.abb(): 'inds' must be in 1:12")
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

} # mymonth.abb function
