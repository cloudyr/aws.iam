#' @rdname groups
#' @title Manage IAM User Groups
#' @description Retrieve, create, update, and delete IAM user groups
#' @param group A character string containing a group name or an object of class \dQuote{iam_group}.
#' @param name A character string specifying the new name for the group.
#' @template path
#' @template n
#' @template marker
#' @template dots
#' @return \code{create_group} and \code{get_group} return objects of class \dQuote{iam_group}. \code{update_group} and \code{delete_group}, \code{add_user}, and \code{remove_user} return a logical \code{TRUE} (if successful) or an error. \code{list_groups} returns a list of IAM group objects. \code{get_group_users} returns a list of objects of class \dQuote{iam_user}, with a \dQuote{iam_group} attribute.
#' @export
create_group <- function(group, path, ...){
    query <- list(Action = "CreateGroup")
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- get_groupname(group)
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query[["Path"]] <- path
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- structure(out[["CreateGroupResponse"]][["CreateGroupResult"]][["Group"]],
                         class = "iam_group")
    }
    out
}

#' @rdname groups
#' @export
update_group <- function(group, name, path, ...) {
    query <- list(Action = "UpdateGroup")
    group <- get_groupname(group)
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- group
    if (!missing(name)) {
        if (nchar(name) < 1 | nchar(name) > 128) {
            stop("'name' must be between 1 and 128 characters")
        }
        query[["NewGroupName"]] <- name
    }
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query[["NewPath"]] <- newpath
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname groups
#' @export
delete_group <- function(group, ...){
    query <- list(Action = "DeleteGroup")
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- get_groupname(group)
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}


#' @rdname groups
#' @export
get_group_users <- function(group, n, marker, ...) {
    query <- list(Action = "GetGroup")
    group <- get_groupname(group)
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- group
    if (!missing(marker)) {
        query[["Marker"]] <- marker
    }
    if (!missing(n)) {
        if (!n %in% 1:1e3) {
            stop("'n' must be in 1:1000")
        }
        query[["MaxItems"]] <- n
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        check_truncation(out[["GetGroupResponse"]][["GetGroupResult"]][["IsTruncated"]])
        users <- out[["GetGroupResponse"]][["GetGroupResult"]][["Users"]]
        out <- structure(lapply(users, `class<-`, "iam_user"),
                         group = structure(out[["GetGroupResponse"]][["GetGroupResult"]][["Group"]], class = "iam_group"),
                         marker = out[["GetGroupResponse"]][["GetGroupResult"]][["Marker"]])
    }
    return(out)
}

#' @rdname groups
#' @export
list_groups <- function(user, n, marker, prefix, ...) {
    if (!missing(user)) {
        query <- list(Action = "ListGroupsForUsers", UserName = user)
    } else {
        user <- NULL
        query <- list(Action = "ListGroups")
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
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        check_truncation(out[["ListGroupsResponse"]][["ListGroupsResult"]][["IsTruncated"]])
        groups <- out[["ListGroupsResponse"]][["ListGroupsResult"]][["Groups"]]
        out <- structure(lapply(groups, function(x) {
                         x[["UserName"]] <- user
                         class(x) <- "iam_group"
                         x
                         }),
                         marker = out[["ListGroupsResponse"]][["ListGroupsResult"]][["Marker"]])
    }
    out
}


#' @rdname groups
#' @export
add_user <- function(user, group, ...){
    query <- list(Action = "AddUserToGroup")
    group <- get_groupname(group)
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- group
    user <- get_username(user)
    if (nchar(user) < 1 | nchar(user) > 128) {
        stop("'user' must be between 1 and 128 characters")
    }
    query[["UserName"]] <- user
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname groups
#' @export
remove_user <- function(user, group, ...){
    query <- list(Action = "RemoveUserFromGroup")
    group <- get_groupname(group)
    if (nchar(group) < 1 | nchar(group) > 128) {
        stop("'group' must be between 1 and 128 characters")
    }
    query[["GroupName"]] <- group
    user <- get_username(user)
    if (nchar(user) < 1 | nchar(user) > 128) {
        stop("'user' must be between 1 and 128 characters")
    }
    query[["UserName"]] <- user
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}
