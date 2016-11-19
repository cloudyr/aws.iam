check_truncation <- function(truncation) {
    if (isTRUE(truncation)) {
        message("Response is truncated")
    }
}

get_keyid <- function(x) {
    if (inherits(x, "iam_key")) {
        x[["AccessKeyId"]]
    } else {
        x
    }
}

get_username <- function(x) {
    if (inherits(x, "iam_user")) {
        x[["UserName"]]
    } else {
        x
    }
}

get_rolename <- function(x) {
    if (inherits(x, "iam_role")) {
        x[["RoleName"]]
    } else {
        x
    }
}

print.iam_key <- function(x, ...) {
    if (!is.null(x[["AccessKeyId"]])) {
        cat("AccessKeyId:", x[["AccessKeyId"]], "\n")
    }
    if (!is.null(x[["CreateDate"]])) {
        cat("CreateDate: ", x[["CreateDate"]], "\n")
    }
    if (!is.null(x[["Status"]])) {
        cat("Status:     ", x[["Status"]], "\n")
    }
    if (!is.null(x[["UserName"]])) {
        cat("UserName:   ", x[["UserName"]], "\n")
    }
    invisible(x)
}

print.iam_role <- function(x, ...) {
    if (!is.null(x[["RoleName"]])) {
        cat("RoleName:  ", paste0(x[["Path"]], x[["RoleName"]]), "\n")
    }
    if (!is.null(x[["RoleId"]])) {
        cat("RoleId:    ", x[["RoleId"]], "\n")
    }
    if (!is.null(x[["Arn"]])) {
        cat("Arn:       ", x[["Arn"]], "\n")
    }
    if (!is.null(x[["CreateDate"]])) {
        cat("CreateDate:", x[["CreateDate"]], "\n")
    }
    if (!is.null(x[["AssumeRolePolicyDocument"]])) {
        cat("policy:    ", x[["AssumeRolePolicyDocument"]], "\n")
    }
    invisible(x)
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
