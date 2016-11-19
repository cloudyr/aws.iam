#' @rdname aliases
#' @title Manage IAM Account Aliases
#' @description Retrieve, create, update, and delete IAM Account Aliases
#' @param alias A character string specifying an alias, or an object of class \dQuote{iam_alias}.
#' @template n
#' @template marker
#' @template dots
#' @return \code{create_alias} and \code{delete_alias} return a logical \code{TRUE} (if successful). \code{list_aliases} returns a list of objects of class \dQuote{iam_alias}.
#' @references \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/console_account-alias.html}{AWS Account Aliases}
#' @export
create_alias <- function(alias, ...) {
    query <- list(Action = "CreateAccountAlias")
    query[["AccountAlias"]] <- alias
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname aliases
#' @export
delete_alias <- function(alias, ...) {
    query <- list(Action = "DeleteAccountAlias", AccountAlias = get_aliasname(alias))
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname aliases
#' @export
list_aliases <- function(n, marker, ...) {
    query <- list(Action = "ListAccountAliases")
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
        check_truncation(out[["ListAccountAliasesResponse"]][["ListAccountAliasesResult"]][["IsTruncated"]])
        out <- lapply(out[["ListAccountAliasesResponse"]][["ListAccountAliasesResult"]][["AccountAliases"]], 
                function(x) {
                    structure(list(AliasName = x), class = "iam_alias")
                })
        attr(out, "marker") <- out[["ListAccountAliasesResponse"]][["ListAccountAliasesResult"]][["Marker"]]
    }
    out
}
