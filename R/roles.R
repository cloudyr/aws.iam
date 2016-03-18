#' @export
create_role <- function(role, policy, path, ...){
    query <- list(Action = "CreateRole")
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    query$AssumeRolePolicyDocument <- policy
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query$Path <- path
    }
    iamHTTP(query = query, ...)
}

#' @export
delete_role <- function(role, ...){
    query <- list(Action = "DeleteRole")
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    iamHTTP(query = query, ...)
}

#' @export
add_profile_role <- function(role, profile, ...) {
    query <- list(Action = "AddRoleToInstanceProfile", 
                  InstanceProfileName = profile)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    iamHTTP(query = query, ...)
}

#' @export
remove_profile_role <- function(role, profile, ...) {
    query <- list(Action = "RemoveRoleFromInstanceProfile", 
                  InstanceProfileName = profile)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    iamHTTP(query = query, ...)
}

#' @export
list_roles <- function(n, marker, prefix, ...) {
    query <- list(Action = "ListRoles")
    if (!missing(marker)) {
        query$Marker <- marker
    }
    if (!missing(prefix)) {
        query$PathPrefix <- prefix
    }
    if (!missing(n)) {
        if (!n %in% 1:1e3) {
            stop("'n' must be in 1:1000")
        }
        query$MaxItems <- n
    }
    r <- iamHTTP(query = query, ...)
    return(r)
}
