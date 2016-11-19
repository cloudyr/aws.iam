#' @rdname roles
#' @title Manage IAM Roles
#' @description Retrieve, create, update, and delete IAM Roles
#' @param role A character string containing a role name or an object of class \dQuote{iam_role}.
#' @param policy \dots
#' @template path
#' @template n
#' @template marker
#' @template dots
#' @return \code{create_role} and \code{get_role} return objects of class \dQuote{iam_role}. \code{update_role} and \code{delete_role} return a logical \code{TRUE} (if successful) or an error. \code{list_roles} returns a list of IAM role objects.
#' @export
create_role <- function(role, policy, path, ...){
    query <- list(Action = "CreateRole")
    role <- get_rolename(role)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query[["RoleName"]] <- role
    query[["AssumeRolePolicyDocument"]] <- policy
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query[["Path"]] <- path
    }
    out <- iamHTTP(query = query, ...)
    out
}

#' @rdname roles
#' @export
delete_role <- function(role, ...){
    query <- list(Action = "DeleteRole")
    role <- get_rolename(role)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query[["RoleName"]] <- role
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname roles
#' @export
add_profile_role <- function(role, profile, ...) {
    query <- list(Action = "AddRoleToInstanceProfile", 
                  InstanceProfileName = profile)
    role <- get_rolename(role)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query[["RoleName"]] <- role
    out <- iamHTTP(query = query, ...)
    out
}

#' @rdname roles
#' @export
remove_profile_role <- function(role, profile, ...) {
    query <- list(Action = "RemoveRoleFromInstanceProfile", 
                  InstanceProfileName = profile)
    role <- get_rolename(role)
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    out <- iamHTTP(query = query, ...)
    out
}

#' @rdname roles
#' @importFrom utils URLdecode
#' @export
list_roles <- function(n, marker, path, ...) {
    query <- list(Action = "ListRoles")
    if (!missing(marker)) {
        query$Marker <- marker
    }
    if (!missing(path)) {
        query$PathPrefix <- path
    }
    if (!missing(n)) {
        if (!n %in% 1:1e3) {
            stop("'n' must be in 1:1000")
        }
        query$MaxItems <- n
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        check_truncation(out[["ListRolesResponse"]][["ListRolesResult"]][["IsTruncated"]])
        roles <- out[["ListRolesResponse"]][["ListRolesResult"]][["Roles"]]
        out <- structure(lapply(roles, function(x) {
                         x[["AssumeRolePolicyDocument"]] <- URLdecode(x[["AssumeRolePolicyDocument"]])
                         class(x) <- "iam_role"
                         x
                         }),
                         marker = out[["ListRolesResponse"]][["ListRolesResult"]][["Marker"]])
    }
    out
}
