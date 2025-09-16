# https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
ls.objects <- function (pos=1, pattern, order.by,
                        decreasing=F, head=F, n=5, ndim=5, 
                        show, show_mem) {
    
    # print session PID and memory usage
    # https://github.com/tdhock/dotfiles/blob/master/.Rprofile
    if (show_mem) {
        pscmd <- paste0("ps -o rss,vsz ", Sys.getpid())
        mem <- system(pscmd, intern=T)
        mem <- read.table(text=mem, header=T)
        mem <- unlist(mem)*1024 # kilobytes --> bytes
        prettyMem <- sapply(mem, function(x) {
                            class(x) <- "object_size"
                            format(x, units="auto") })
        mem <- data.frame(c("RSS physical", "VSZ virtual"), 
                          mem, prettyMem)
        names(mem) <- c(paste0("`", pscmd, "`"), "Mem [B]", "PrettyMem") 
    }

    # sizes of objects loaded in current work-space
    snapply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    lnapply <- function(names, fn) lapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- snapply(names, function(x) as.character(class(x))[1])
    if (length(obj.class) != 0) {
        obj.mode <- snapply(names, base::mode)
        obj.class <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.prettysize <- snapply(names, function(x) {
                               format(utils::object.size(x), units = "auto") })
        obj.size <- snapply(names, object.size)
        obj.relsize <- round(obj.size/max(obj.size), 3)
        obj.dim <- lnapply(names, function(x) dim(x) )
        maxdim <- max(sapply(obj.dim, length))
        if (maxdim == 0) { # all vectors or functions
            maxdim <- 1
        }
        tmp <- array(NA, c(length(obj.dim), maxdim))
        for (i in 1:length(obj.dim)) {
            if (is.null(obj.dim[[i]])) {
                tmp[i,] <- NA 
            } else {
                tmp[i,1:length(obj.dim[[i]])] <- obj.dim[[i]]
            }
        }
        obj.dim <- tmp
        vec <- apply(obj.dim, 1, function(x) all(is.na(x))) & (obj.class != "function")
        obj.dim[vec, 1] <- snapply(names, length)[vec]
        out <- data.frame(obj.class, obj.size, obj.prettysize, obj.relsize, obj.dim)
        names(out) <- c("Class", "Size [B]", "PrettySize", "RelSize", paste0("dim", 1:maxdim))
        ndim <- min(ndim, maxdim)
        if (!missing(order.by))
            out <- out[order(out[[order.by]], decreasing=decreasing), 1:(4+ndim)]
        if (head)
            out <- head(out[,1:(4+ndim)], n)
    } # if there are objects
    
    if (show) {
        if (show_mem) print(mem, row.names=F, width=300)
        if (exists("out")) print(out, width=300)
    } else {
        ret <- list()
        if (show_mem) ret <- c(ret, list(mem=mem))
        if (exists("out")) ret <- c(ret, list(ls=out))
        if (length(ret) > 0) return(ret)
    }


} # ls.objects

# shorthand
myls <- function(..., n=10, show=T, show_mem=T) {
    ls.objects(..., order.by="RelSize", decreasing=T, head=T, n=n, show=show, show_mem=show_mem)
}

