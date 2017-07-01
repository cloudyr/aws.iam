#' @rdname users
#' @title Manage IAM Users
#' @description Retrieve, create, update, and delete IAM Users
#' @param user A character string specifying a user name or an object of class \dQuote{iam_user}.
#' @template path
#' @param name A character string specifying the new name for the user.
#' @template n
#' @template marker
#' @template dots
#' @return \code{create_user} and \code{get_user} return objects of class \dQuote{iam_user}. \code{update_user} and \code{delete_user} return a logical \code{TRUE} (if successful) or an error. \code{list_users} returns a list of IAM user objects.
#' @examples
#' \dontrun{
#' list_users()
#' 
#' # create example user
#' u <- create_user("example-user")
#' 
#' # cleanup
#' delete_user(u)
#' }
#' @export
create_user <- function(user, path, ...) {
    query <- list(Action = "CreateUser")
    user <- get_username(user)
    if (nchar(user) < 1 | nchar(user) > 128) {
        stop("'user' must be between 1 and 128 characters")
    }
    query[["UserName"]] <- user
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query[["Path"]] <- path
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- structure(out[["CreateUserResponse"]][["CreateUserResult"]][["User"]],
                         class = "iam_user")
    }
    out
}

#' @rdname users
#' @export
update_user <- function(user, name, path, ...) {
    query <- list(Action = "UpdateUser")
    user <- get_username(user)
    if (nchar(user) < 1 | nchar(user) > 128) {
        stop("'user' must be between 1 and 128 characters")
    }
    query[["UserName"]] <- user
    if (!missing(name)) {
        if (nchar(name) < 1 | nchar(name) > 128) {
            stop("'name' must be between 1 and 128 characters")
        }
        query[["NewUserName"]] <- name
    }
    if (!missing(path)) {
        if (nchar(path) > 512 | nchar(path) < 1) {
            stop("'path' must be between 1 and 512 characters")
        }
        query[["NewPath"]] <- path
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out  
}

#' @rdname users
#' @export
get_user <- function(user, ...) {
    query <- list(Action = "GetUser")
    user <- get_username(user)
    if (nchar(user) < 1 | nchar(user) > 128) {
        stop("'user' must be between 1 and 128 characters")
    }
    query[["UserName"]] <- user
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- structure(out[["GetUserResponse"]][["GetUserResult"]][["User"]],
                         class = "iam_user")
    }
    out
}

#' @rdname users
#' @export
delete_user <- function(user, ...) {
    query <- list(Action = "DeleteUser")
    user <- get_username(user)
    if (!missing(user)) {
        if (nchar(user) < 1 | nchar(user) > 128) {
            stop("'user' must be between 1 and 128 characters")
        }
        query[["UserName"]] <- user
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname users
#' @export
list_users <- function(n, marker, path, ...) {
    query <- list(Action = "ListUsers")
    if (!missing(marker)) {
        query[["Marker"]] <- marker
    }
    if (!missing(path)) {
        query[["PathPrefix"]] <- path
    }
    if (!missing(n)) {
        if (!n %in% 1:1e3) {
            stop("'n' must be in 1:1000")
        }
        query[["MaxItems"]] <- n
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        check_truncation(out[["ListUsersResponse"]][["ListUsersResult"]][["IsTruncated"]])
        users <- out[["ListUsersResponse"]][["ListUsersResult"]][["Users"]]
        out <- structure(lapply(users, `class<-`, "iam_user"),
                         marker = out[["ListUsersResponse"]][["ListUsersResult"]][["Marker"]])
    }
    out
}
