#' @title Get Account Details
#' @description Retrieve IAM Account Details. This is useful as a \dQuote{hello world!} test.
#' @template dots
#' @return A list containing various account details.
#' @export
get_account <- function(...) {
    query <- list(Action = "GetAccountSummary")
    out <- iamHTTP(query = query, ...)
    out[["GetAccountSummaryResponse"]][["GetAccountSummaryResult"]][["SummaryMap"]]
}
