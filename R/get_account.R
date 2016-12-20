#' @rdname get_account
#' @title Get Account Details
#' @description Retrieve IAM Account Details. This is useful as a \dQuote{hello world!} test.
#' @template dots
#' @return A list containing various account details.
#' @details \code{get_account} returns a list of account details. \code{credential_report} generates and/or retrieves a credential report. \code{auth_details} returns a list of group, user, role, and policy details.
#' @export
get_account <- function(...) {
    query <- list(Action = "GetAccountSummary")
    out <- iamHTTP(query = query, ...)
    out[["GetAccountSummaryResponse"]][["GetAccountSummaryResult"]][["SummaryMap"]]
}

#' @rdname get_account
#' @export
credential_report <- function(...) {
    query <- list(Action = "GenerateCredentialReport")
    out <- iamHTTP(query = query, ...)
    state <- out[["GenerateCredentialReportResponse"]][["GenerateCredentialReportResult"]][["State"]]
    desc <- out[["GenerateCredentialReportResponse"]][["GenerateCredentialReportResult"]][["Description"]]
    if (state == "STARTED") {
        message(desc)
    }
    desc
}

#' @rdname get_account
#' @param type An optional character string specifying one or more types of reports to return.
#' @template n
#' @template marker
#' @export
auth_details <- function(type, n, marker, ...) {
    query <- list(Action = "GetAccountAuthorizationDetails")
    if (!missing(type)) {
        type <- match.arg(type, several.ok = TRUE)
        a <- as.list(type)
        names(a) <- paste0("Filter.member.",1:length(a))
        query <- c(query, a)
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
    out[["GetAccountAuthorizationDetailsResponse"]][["GetAccountAuthorizationDetailsResult"]]
}
