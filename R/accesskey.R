create_key <- function(user, ...) {
    query <- list(Action = "CreateAccessKey")
    if(!missing(user))
        query$UserName <- user
    iamHTTP(query = query, ...)
}

update_key <- function(key, user, status, ...) {
    query <- list(Action = "UpdateAccessKey")
    vstatus <- c("Active", "Inactive")
    if(!status %in% vstatus)
        stop("'status' must be one of: ", paste0(vstatus, collapse = ", "))
    if(!missing(user))
        query$UserName <- user
    iamHTTP(query = query, ...)
}

delete_key <- function(key, user, ...) {
    query <- list(Action = "DeleteAccessKey", AccessKeyId = key)
    if(!missing(user))
        query$UserName <- user
    iamHTTP(query = query, ...)
}

list_keys <- function(n, user, marker, ...) {
    query <- list(Action = "ListAccessKey")
    if(!missing(user))
        query$UserName <- user
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    iamHTTP(query = query, ...)
}
