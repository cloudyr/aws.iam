create_user <- function(user, path, ...){
    query <- list(Action = "CreateUser")
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    if(!missing(path)) {
        if(nchar(path) > 512 | nchar(path) < 1)
            stop("'path' must be between 1 and 512 characters")
        query$Path <- path
    }
    iamHTTP(query = query, ...)
}

update_user <- function(user, name, path, ...) {
    query <- list(Action = "UpdateUser")
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    if(!missing(name)){
        if(nchar(name) < 1 | nchar(name) > 128)
            stop("'name' must be between 1 and 128 characters")
        query$NewUserName <- name
    }
    if(!missing(path)) {
        if(nchar(path) > 512 | nchar(path) < 1)
            stop("'path' must be between 1 and 512 characters")
        query$NewPath <- path
    }
    iamHTTP(query = query, ...)    
}

get_user <- function(user, ...){
    query <- list(Action = "GetUser")
    if(nchar(user) < 1 | nchar(user) > 128)
        stop("'user' must be between 1 and 128 characters")
    query$UserName <- user
    iamHTTP(query = query, ...)    
}

delete_user <- function(user, ...){
    query <- list(Action = "DeleteUser")
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)
}

list_users <- function(n, marker, prefix, ...) {
    query <- list(Action = "ListUsers")
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(prefix))
        query$Prefix <- prefix
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    iamHTTP(query = query, ...)
}
