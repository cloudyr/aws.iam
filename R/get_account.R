#' @rdname get_account
#' @title Get Account Details
#' @description Retrieve IAM Account Details. This is useful as a \dQuote{hello world!} test.
#' @template dots
#' @return A list containing various account details.
#' @details \code{get_account} returns a list of account details. \code{credential_report} generates and/or retrieves a credential report.
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
