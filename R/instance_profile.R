#' @rdname instance_profiles
#' @title Instance Profiles
#' @description Coming soon\dots
#' @references \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2_instance-profiles.html}{About Instance Profiles}
# @export
create_profile <- function(profile, path, ...){
    query <- list(Action = "CreateInstanceProfile", InstanceProfileName = profile)
    if (!missing(path)) {
        query$Path <- path
    }
    iamHTTP(query = query, ...)
}

#' @rdname instance_profiles
# @export
delete_profile <- function(profile, ...){
    query <- list(Action = "DeleteInstanceProfile", InstanceProfileName = profile)
    iamHTTP(query = query, ...)
}

#' @rdname instance_profiles
# @export
get_profile <- function(profile, ...){
    query <- list(Action = "GetInstanceProfile", InstanceProfileName = profile)
    iamHTTP(query = query, ...)
}

#' @rdname instance_profiles
# @export
list_profiles <- function(role, n, marker, prefix, ...) {
    if (!missing(role)) {
        query <- list(Action = "ListInstanceProfilesForRole", RoleName = role)
        if (!missing(marker)) {
            query$Marker <- marker
        }
        if (!missing(n)) {
            if(!n %in% 1:1e3)
                stop("'n' must be in 1:1000")
            query$MaxItems <- n
        }
    } else {
        query <- list(Action = "ListInstanceProfiles")
        if (!missing(prefix)) {
            query$Prefix <- prefix
        }
    }
    if (!missing(marker)) {
        query$Marker <- marker
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
