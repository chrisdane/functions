get_memory_of_workspace_gb <- function(obj_list, unit="Mb") {

    ws <- sapply(obj_list, function(x) object.size(get(x)))
    ws <- sort(ws, decreasing=T)

    if (unit == "Mb") {
        ws <- ws/1024^2
    } else {
        stop(paste0("Unit '", unit, "' not defined."))
    }

    return(ws)

}
