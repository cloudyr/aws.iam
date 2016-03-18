#' @export
add_policy <- function(user, group, role, policy, doc, ...) {
    if (!missing(user)) {
        query <- list(Action = "PutUserPolicy")
        if (!missing(user)){
            if (nchar(user) < 1 | nchar(user) > 128) {
                stop("'user' must be between 1 and 128 characters")
            }
            query$UserName <- user
        }
    } else if (!missing(group)) {
        query <- list(Action = "PutGroupPolicy")
        if (!missing(group)) {
            if (nchar(group) < 1 | nchar(group) > 128) {
                stop("'group' must be between 1 and 128 characters")
            }
            query$GroupName <- group
        }    
    } else if (!missing(role)) {
        query <- list(Action = "PutRolePolicy")
        if (!missing(role)) {
            if (nchar(role) < 1 | nchar(role) > 64) {
                stop("'role' must be between 1 and 64 characters")
            }
            query$RoleName <- role
        }
    }
    query$PolicyName <- policy
    if (nchar(doc) > 10240) {
        stop("'doc' must be < 10240 characters")
    }
    query$PolicyDocument <- doc
    r <- iamHTTP(query = query, ...)
    return(r)    
}

#' @export
update_policy <- function(role, policy, ...) {
    query <- list(Action = "UpdateRolePolicy")
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query$RoleName <- role
    if (nchar(policy) > 10240) {
        stop("'policy' must be < 10240 characters")
    }
    query$PolicyDocument <- policy
    r <- iamHTTP(query = query, ...)
    return(r)
}

#' @export
get_policy <- function(user, group, role, policy, ...) {
    if (!missing(user)) {
        query <- list(Action = "GetUserPolicy")
        if (!missing(user)) {
            if (nchar(user) < 1 | nchar(user) > 128) {
                stop("'user' must be between 1 and 128 characters")
            }
            query$UserName <- user
        }
    } else if (!missing(group)) {
        query <- list(Action = "GetGroupPolicy")
        if (!missing(group)) {
            if (nchar(group) < 1 | nchar(group) > 128) {
                stop("'group' must be between 1 and 128 characters")
            }
            query$GroupName <- group
        }    
    } else if (!missing(role)) {
        query <- list(Action = "GetRolePolicy")
        if (!missing(role)) {
            if (nchar(role) < 1 | nchar(role) > 64) {
                stop("'role' must be between 1 and 64 characters")
            }
            query$RoleName <- role
        }
    }
    query$PolicyName <- policy
    r <- iamHTTP(query = query, ...)
    return(r)    
}

#' @export
delete_policy <- function(user, group, role, policy, ...){
    if (!missing(user)) {
        query <- list(Action = "DeleteUserPolicy")
        if (!missing(user)) {
            if (nchar(user) < 1 | nchar(user) > 128) {
                stop("'user' must be between 1 and 128 characters")
            }
            query$UserName <- user
        }
    } else if (!missing(group)) {
        query <- list(Action = "DeleteGroupPolicy")
        if (!missing(group)) {
            if (nchar(group) < 1 | nchar(group) > 128) {
                stop("'group' must be between 1 and 128 characters")
            }
            query$GroupName <- group
        }
    } else if (!missing(role)) {
        query <- list(Action = "DeleteRolePolicy")
        if (!missing(role)) {
            if (nchar(role) < 1 | nchar(role) > 64) {
                stop("'role' must be between 1 and 64 characters")
            }
            query$RoleName <- role
        }
    }
    query$PolicyName <- policy
    r <- iamHTTP(query = query, ...)
    return(r)
}

#' @export
list_policies <- function(user, group, role, n, marker, ...){
    if (!missing(user)) {
        query <- list(Action = "ListUserPolicies")
        if (!missing(user)) {
            if (nchar(user) < 1 | nchar(user) > 128) {
                stop("'user' must be between 1 and 128 characters")
            }
            query$UserName <- user
        }
    } else if (!missing(group)) {
        query <- list(Action = "ListGroupPolicies")
        if (!missing(group)) {
            if (nchar(group) < 1 | nchar(group) > 128) {
                stop("'group' must be between 1 and 128 characters")
            }
            query$GroupName <- group
        }
    } else if (!missing(role)) {
        query <- list(Action = "ListRolePolicies")
        if (!missing(role)) {
            if (nchar(role) < 1 | nchar(role) > 64) {
                stop("'role' must be between 1 and 64 characters")
            }
            query$RoleName <- role
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
