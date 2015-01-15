create_group <- function(group, path, ...){
    query <- list(Action = "CreateGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    if(!missing(path)) {
        if(nchar(path) > 512 | nchar(path) < 1)
            stop("'path' must be between 1 and 512 characters")
        query$Path <- path
    }
    iamHTTP(query = query, ...)
}

update_group <- function(group, name, path, ...) {
    query <- list(Action = "UpdateGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    if(!missing(name)){
        if(nchar(name) < 1 | nchar(name) > 128)
            stop("'name' must be between 1 and 128 characters")
        query$NewGroupName <- name
    }
    if(!missing(path)) {
        if(nchar(path) > 512 | nchar(path) < 1)
            stop("'path' must be between 1 and 512 characters")
        query$NewPath <- newpath
    }
    iamHTTP(query = query, ...)    
}

delete_group <- function(group, ...){
    query <- list(Action = "DeleteGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    iamHTTP(query = query, ...)
}


get_group <- function(group, n, marker, ...) {
    query <- list(Action = "GetGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    r <- iamHTTP(query = query, ...)
    return(r)    
}

list_groups <- function(user, n, marker, prefix, ...) {
    if(!missing(user)) {
        query <- list(Action = "ListGroupsForUsers", UserName = user)
    } else {
        query <- list(Action = "ListGroups")
        if(!missing(prefix))
            query$Prefix <- prefix
    }
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    r <- iamHTTP(query = query, ...)
    return(r)
}


add_user <- function(user, group, ...){
    query <- list(Action = "AddUserToGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)
}

remove_user <- function(user, group, ...){
    query <- list(Action = "RemoveUserFromGroup")
    if(!missing(group)){
        if(nchar(group) < 1 | nchar(group) > 128)
            stop("'group' must be between 1 and 128 characters")
        query$GroupName <- group
    }
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)    
}
