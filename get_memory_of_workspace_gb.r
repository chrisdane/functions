get_memory_of_workspace_gb <- function(obj_list) {

    ws <- sapply(obj_list, function(x) object.size(get(x)))
    ws <- sort(ws, decreasing=T)/1024^2 # Mb

    return(ws)

}
