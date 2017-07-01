check_truncation <- function(truncation) {
    if (isTRUE(truncation)) {
        message("Response is truncated")
    }
}

get_aliasname <- function(x) {
    if (inherits(x, "iam_alias")) {
        x[["AliasName"]]
    } else {
        x
    }
}

get_groupname <- function(x) {
    if (inherits(x, "iam_group") | inherits(x, "iam_policy")) {
        x[["GroupName"]]
    } else {
        x
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
    if (inherits(x, "iam_user") | inherits(x, "iam_policy")) {
        x[["UserName"]]
    } else {
        x
    }
}

get_policyname <- function(x) {
    if (inherits(x, "iam_policy")) {
        x[["PolicyName"]]
    } else {
        x
    }
}

get_profilename <- function(x) {
    if (inherits(x, "iam_instance_profile")) {
        x[["InstanceProfileName"]]
    } else {
        x
    }
}

get_rolearn <- function(x) {
    if (inherits(x, "iam_role")) {
        x[["Arn"]]
    } else {
        x
    }
}

get_rolename <- function(x) {
    if (inherits(x, "iam_role") | inherits(x, "iam_policy")) {
        x[["RoleName"]]
    } else {
        x
    }
}
