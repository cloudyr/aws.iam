create_profile <- function(profile, path, ...){
    query <- list(Action = "CreateInstanceProfile", InstanceProfileName = profile)
    if(!missing(path))
        query$Path <- path
    iamHTTP(query = query, ...)
}

delete_profile <- function(profile, ...){
    query <- list(Action = "DeleteInstanceProfile", InstanceProfileName = profile)
    iamHTTP(query = query, ...)
}

get_profile <- function(profile, ...){
    query <- list(Action = "GetInstanceProfile", InstanceProfileName = profile)
    iamHTTP(query = query, ...)
}

list_profiles <- function(role, n, marker, prefix, ...) {
    if(!missing(role)) {
        query <- list(Action = "ListInstanceProfilesForRole", RoleName = role)
        if(!missing(marker))
            query$Marker <- marker
        if(!missing(n)) {
            if(!n %in% 1:1e3)
                stop("'n' must be in 1:1000")
            query$MaxItems <- n
        }
    } else {
        query <- list(Action = "ListInstanceProfiles")
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
