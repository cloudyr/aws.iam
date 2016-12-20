#' @rdname policies
#' @title Manage IAM Polices
#' @description Retrieve, create, update, and delete IAM Role, User, and Group Polices
#' @param user A character string specifying a user name or an object of class \dQuote{iam_user}.
#' @param group A character string containing a group name or an object of class \dQuote{iam_group}.
#' @param role A character string containing a role name or an object of class \dQuote{iam_role}.
#' @param policy A character string specifying the policy name.
#' @param doc The contents of the policy document as a character string.
#' @template n
#' @template marker
#' @template dots
#' @return \code{add_policy} and \code{get_policy} return objects of class \dQuote{iam_policy}. \code{update_policy} and \code{delete_policy} return a logical \code{TRUE} (if successful) or an error. \code{list_policies} returns a list of IAM role objects.
#' @export
add_policy <- function(user, group, role, policy, doc, ...) {
    query <- list(PolicyName = get_policyname(policy))
    if (nchar(doc) > 10240) {
        stop("'doc' must be < 10240 characters")
    }
    query[["PolicyDocument"]] <- doc
    if (!missing(user)) {
        query[["Action"]] <- "PutUserPolicy"
        if (nchar(user) < 1 | nchar(user) > 128) {
            stop("'user' must be between 1 and 128 characters")
        }
        query[["UserName"]] <- user
    } else if (!missing(group)) {
        query[["Action"]] <- "PutGroupPolicy"
        if (nchar(group) < 1 | nchar(group) > 128) {
            stop("'group' must be between 1 and 128 characters")
        }
        query[["GroupName"]] <- group
    } else if (!missing(role)) {
        query[["Action"]] <- "PutRolePolicy"
        if (nchar(role) < 1 | nchar(role) > 64) {
            stop("'role' must be between 1 and 64 characters")
        }
        query[["RoleName"]] <- role
    } else {
        stop("Must specify one of 'user', 'group', or 'role'")
    }
    out <- iamHTTP(query = query, verb = "POST", ...)
    out
}

#' @rdname policies
#' @export
update_policy <- function(role, doc, ...) {
    query <- list(Action = "UpdateRolePolicy")
    if (nchar(role) < 1 | nchar(role) > 64) {
        stop("'role' must be between 1 and 64 characters")
    }
    query[["RoleName"]] <- role
    if (nchar(doc) > 10240) {
        stop("'doc' must be < 10240 characters")
    }
    query[["PolicyDocument"]] <- doc
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname policies
#' @importFrom utils URLdecode
#' @export
get_policy <- function(policy, user, group, role, ...) {
    query <- list(PolicyName = get_policyname(policy))
    if (!missing(user) || ("UserName" %in% names(policy))) {
        query[["Action"]] <- "GetUserPolicy"
        if (!missing(user)) {
            user <- get_username(user)
        } else {
            user <- get_username(policy)
        }
        if (nchar(user) < 1 | nchar(user) > 128) {
            stop("'user' must be between 1 and 128 characters")
        }
        query[["UserName"]] <- user
        out <- iamHTTP(query = query, verb = "POST", ...)
        out <- out[["GetGroupPolicyResponse"]][["GetGroupPolicyResult"]]
    } else if (!missing(group) || ("GroupName" %in% names(policy))) {
        query[["Action"]] <- "GetGroupPolicy"
        if (!missing(group)) {
            group <- get_groupname(group)
        } else {
            group <- get_groupname(policy)
        }
        if (nchar(group) < 1 | nchar(group) > 128) {
            stop("'group' must be between 1 and 128 characters")
        }
        query[["GroupName"]] <- group
        out <- iamHTTP(query = query, verb = "POST", ...)
        out <- out[["GetGroupPolicyResponse"]][["GetGroupPolicyResult"]]
    } else if (!missing(role) || ("RoleName" %in% names(policy))) {
        query[["Action"]] <- "GetRolePolicy"
        if (!missing(role)) {
            role <- get_rolename(role)
        } else {
            role <- get_rolename(policy)
        }
        if (nchar(role) < 1 | nchar(role) > 64) {
            stop("'role' must be between 1 and 64 characters")
        }
        query[["RoleName"]] <- role
        out <- iamHTTP(query = query, verb = "POST", ...)
        out <- out[["GetRolePolicyResponse"]][["GetRolePolicyResult"]]
    } else {
        stop("Must specify one of 'user', 'group', or 'role'")
    }
    out[["PolicyDocument"]] <- URLdecode(out[["PolicyDocument"]])
    structure(out, class = "iam_policy")
}

#' @rdname policies
#' @export
delete_policy <- function(user, group, role, policy, ...){
    query <- list(PolicyName = policy)
    if (!missing(user)) {
        query[["Action"]] <- "DeleteUserPolicy"
        user <- get_username(user)
        if (nchar(user) < 1 | nchar(user) > 128) {
            stop("'user' must be between 1 and 128 characters")
        }
        query[["UserName"]] <- user
    } else if (!missing(group)) {
        query[["Action"]] <- "DeleteGroupPolicy"
        group <- get_groupname(group)
        if (nchar(group) < 1 | nchar(group) > 128) {
            stop("'group' must be between 1 and 128 characters")
        }
        query[["GroupName"]] <- group
    } else if (!missing(role)) {
        query[["Action"]] <- "DeleteRolePolicy"
        role <- get_rolename(role)
        if (nchar(role) < 1 | nchar(role) > 64) {
            stop("'role' must be between 1 and 64 characters")
        }
        query[["RoleName"]] <- role
    } else {
        stop("Must specify one of 'user', 'group', or 'role'")
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname policies
#' @export
list_policies <- function(user, group, role, n, marker, ...){
    query <- list()
    if (!missing(marker)) {
        query[["Marker"]] <- marker
    }
    if (!missing(n)) {
        if (!n %in% 1:1e3) {
            stop("'n' must be in 1:1000")
        }
        query[["MaxItems"]] <- n
    }
    if (!missing(user)) {
        query[["Action"]] <- "ListUserPolicies"
        user <- get_username(user)
        if (nchar(user) < 1 | nchar(user) > 128) {
            stop("'user' must be between 1 and 128 characters")
        }
        query[["UserName"]] <- user
        out <- iamHTTP(query = query, ...)
        check_truncation(out[["ListUserPoliciesResponse"]][["ListUserPoliciesResult"]][["IsTruncated"]])
        policies <- out[["ListUserPoliciesResponse"]][["ListUserPoliciesResult"]][["PolicyNames"]]
        policies <- lapply(policies, function(x) {
            structure(list(PolicyName = x, UserName = user), class = "iam_policy")
        })
        attr(policies, "marker") <- out[["ListUserPoliciesResponse"]][["ListUserPoliciesResult"]][["Marker"]]
    } else if (!missing(group)) {
        query[["Action"]] <- "ListGroupPolicies"
        group <- get_groupname(group)
        if (nchar(group) < 1 | nchar(group) > 128) {
            stop("'group' must be between 1 and 128 characters")
        }
        query[["GroupName"]] <- group
        out <- iamHTTP(query = query, ...)
        check_truncation(out[["ListGroupPoliciesResponse"]][["ListGroupPoliciesResult"]][["IsTruncated"]])
        policies <- out[["ListGroupPoliciesResponse"]][["ListGroupPoliciesResult"]][["PolicyNames"]]
        policies <- lapply(policies, function(x) {
            structure(list(PolicyName = x, GroupName = group), class = "iam_policy")
        })
        attr(policies, "marker") <- out[["ListGroupPoliciesResponse"]][["ListGroupPoliciesResult"]][["Marker"]]
    } else if (!missing(role)) {
        query[["Action"]] <- "ListRolePolicies"
        role <- get_rolename(role)
        if (nchar(role) < 1 | nchar(role) > 64) {
            stop("'role' must be between 1 and 64 characters")
        }
        query[["RoleName"]] <- role
        out <- iamHTTP(query = query, ...)
        check_truncation(out[["ListRolePoliciesResponse"]][["ListRolePoliciesResult"]][["IsTruncated"]])
        policies <- out[["ListRolePoliciesResponse"]][["ListRolePoliciesResult"]][["PolicyNames"]]
        policies <- lapply(policies, function(x) {
            structure(list(PolicyName = x, RoleName = role), class = "iam_policy")
        })
        attr(policies, "marker") <- out[["ListRolePoliciesResponse"]][["ListRolePoliciesResult"]][["Marker"]]
    } else {
        stop("Must specify one of 'user', 'group', or 'role'")
    }
    policies
}
