#' @rdname keys
#' @title Manage Access Keys/Credentials
#' @description Retrieve, create, update, and delete IAM access keys
#' @param key A character string specifying an access key or an object of class \dQuote{iam_key}.
#' @param user Optionally, a character string specifying a user name or an object of class \dQuote{iam_user}. This will be retrieved by default from the \dQuote{UserName} list entry in \code{key}, if available; otherwise the user is assumed to be the user whose credentials are being used to execute the request.
#' @param status A character string specifying either \dQuote{Active} or \dQuote{Inactive} to status the key status to.
#' @template n
#' @template marker
#' @template dots
#' @return \code{create_user} and \code{get_user} return objects of class \dQuote{iam_user}. \code{update_user} and \code{delete_user} return a logical \code{TRUE} (if successful) or an error. \code{list_users} returns a list of IAM user objects.
#' @export
create_key <- function(user, ...) {
    query <- list(Action = "CreateAccessKey")
    user <-get_username(user)
    query$UserName <- user
    out <- iamHTTP(query = query, ...)
    out <- out[["CreateAccessKeyResponse"]][["CreateAccessKeyResult"]][["AccessKey"]]
    out[["UserName"]] <- user
    class(out) <- "iam_key"
    out
}

#' @rdname keys
#' @export
update_key <- function(key, user, status, ...) {
    query <- list(Action = "UpdateAccessKey", AccessKeyId = get_keyid(key))
    vstatus <- c("Active", "Inactive")
    if (!status %in% vstatus) {
        stop("'status' must be one of: ", paste0(vstatus, collapse = ", "))
    }
    query$Status <- status
    if (!missing(user)) {
        query[["UserName"]] <- get_username(user)
    } else if (inherits(key, "iam_key") && !is.null(key[["UserName"]])) {
        query[["UserName"]] <- key[["UserName"]]
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname keys
#' @export
delete_key <- function(key, user, ...) {
    query <- list(Action = "DeleteAccessKey", AccessKeyId = get_keyid(key))
    if (!missing(user)) {
        query[["UserName"]] <- get_username(user)
    } else if (inherits(key, "iam_key") && !is.null(key[["UserName"]])) {
        query[["UserName"]] <- key[["UserName"]]
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname keys
#' @export
list_keys <- function(user, n, marker, ...) {
    query <- list(Action = "ListAccessKeys")
    if  (!missing(user)) {
        user <- get_username(user)
        query[["UserName"]] <- user
    } else {
        user <- NULL
    }
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
        check_truncation(out[["ListAccessKeysResponse"]][["ListAccessKeysResult"]][["IsTruncated"]])
        out <- lapply(out[["ListAccessKeysResponse"]][["ListAccessKeysResult"]][["AccessKeyMetadata"]], function(x) {
            x[["UserName"]] <- user
            class(x) <- "iam_key"
            x
        })
    }
    out
}
