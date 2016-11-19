check_truncation <- function(truncation) {
    if (isTRUE(truncation)) {
        message("Response is truncated")
    }
}

get_username <- function(x) {
    if (inherits(x, "iam_user")) {
        x[["UserName"]]
    } else {
        x
    }
}

print.iam_user <- function(x, ...) {
    if (!is.null(x[["UserName"]])) {
        cat("UserName:  ", paste0(x[["Path"]], x[["UserName"]]), "\n")
    }
    if (!is.null(x[["UserId"]])) {
        cat("UserId:    ", x[["UserId"]], "\n")
    }
    if (!is.null(x[["Arn"]])) {
        cat("Arn:       ", x[["Arn"]], "\n")
    }
    if (!is.null(x[["CreateDate"]])) {
        cat("CreateDate:", x[["CreateDate"]], "\n")
    }
    invisible(x)
}
