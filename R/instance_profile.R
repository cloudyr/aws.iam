#' @rdname instance_profiles
#' @title Instance Profiles
#' @description Create, retrieve, list, and delete EC2 Instance Profiles
#' @template profile 
#' @param role A character string containing a role name or an object of class \dQuote{iam_role}.
#' @template n
#' @template marker
#' @template path
#' @template dots
#' @return An object of class \dQuote{iam_instance_profile}.
#' @references
#'   \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2_instance-profiles.html}{About Instance Profiles}
#'   \href{http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfile.html}{API Documentation: CreateInstanceProfile}
#'   \href{http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteInstanceProfile.html}{API Documentation: DeleteInstanceProfile}
#'   \href{http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetInstanceProfile.html}{API Documentation: GetInstanceProfile}
#'   \href{http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfiles.html}{API Documentation: ListInstanceProfiles}
#' @export
create_profile <- function(profile, path, ...){
    query <- list(Action = "CreateInstanceProfile", InstanceProfileName = get_profilename(profile))
    if (!missing(path)) {
        query$Path <- path
    }
    out <- iamHTTP(query = query, ...)
    if (inherits(out, "aws_error")) {
        return(out)
    }
    out <- out[["CreateInstanceProfileResponse"]][["CreateInstanceProfileResult"]][["InstanceProfile"]]
    structure(out,
              class = "iam_instance_profile",
              RequestId = out[["CreateInstanceProfileResponse"]][["ResponseMetadata"]][["RequestId"]])
}

#' @rdname instance_profiles
#' @export
delete_profile <- function(profile, ...){
    query <- list(Action = "DeleteInstanceProfile", InstanceProfileName = get_profilename(profile))
    out <- iamHTTP(query = query, ...)
    if (inherits(out, "aws_error")) {
        return(out)
    }
    return(TRUE)
}

#' @rdname instance_profiles
#' @export
get_profile <- function(profile, ...){
    query <- list(Action = "GetInstanceProfile", InstanceProfileName = get_profilename(profile))
    out <- iamHTTP(query = query, ...)
    if (inherits(out, "aws_error")) {
        return(out)
    }
    out <- out[["GetInstanceProfileResponse"]][["GetInstanceProfileResult"]][["InstanceProfile"]]
    if (length(out[["Roles"]])) {
        out[["Roles"]] <- lapply(out[["Roles"]], `class<-`, "iam_role")
    }
    structure(out,
              class = "iam_instance_profile",
              RequestId = out[["GetInstanceProfileResponse"]][["ResponseMetadata"]][["RequestId"]])
}

#' @rdname instance_profiles
#' @export
list_profiles <- function(role, n, marker, path, ...) {
    if (!missing(role)) {
        role <- get_rolename(role)
        query <- list(Action = "ListInstanceProfilesForRole", RoleName = role)
        if (!missing(marker)) {
            query$Marker <- marker
        }
        if (!missing(n)) {
            if(!n %in% 1:1e3)
                stop("'n' must be in 1:1000")
            query$MaxItems <- n
        }
    } else {
        query <- list(Action = "ListInstanceProfiles")
        if (!missing(path)) {
            query$Prefix <- path
        }
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
    if (inherits(out, "aws_error")) {
        return(out)
    }
    profiles <- out[["ListInstanceProfilesResponse"]][["ListInstanceProfilesResult"]][["InstanceProfiles"]]
    profiles <- lapply(profiles, function(x) {
        if (length(x[["Roles"]])) {
            x[["Roles"]] <- lapply(x[["Roles"]], `class<-`, "iam_role")
        }
        class(x) <- "iam_instance_profile"
        x
    })
    structure(profiles,
              IsTruncated = out[["ListInstanceProfilesResponse"]][["ListInstanceProfilesResult"]][["IsTruncated"]],
              Marker = out[["ListInstanceProfilesResponse"]][["ListInstanceProfilesResult"]][["Marker"]],
              RequestId = out[["ListInstanceProfilesResponse"]][["ResponseMetadata"]][["RequestId"]])
}
