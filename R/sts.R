#' @rdname STS
#' @title Temporary Session Tokens
#' @description Get a temporary credentials (i.e., a Session Token)
#' @param role A character string containing a role ARN or an object of class \dQuote{iam_role}.
#' @param session A character string specifying the name of the temporary session.
#' @param duration A numeric value specifying a duration that the credentials should be valid, in seconds, between 900 and 129600.
#' @param id Optionally, the serial number or Amazon Resource Number for a multi-factor authentication (MFA) device.
#' @param code If \code{id} is specified, the value provided by the MFA device.
#' @param policy A character string specifying a JSON-formatted role policy. For \code{assume_role}, if \code{role} is an object of class \dQuote{iam_role}, this will be inferred automatically.
#' @param use A logical (default \code{FALSE}), specifying whether to use these credentials for subsequent requests. If \code{TRUE}, any currently used credentials are stored in a package environment (if no credentials are already stored; in that case, the request will fail) and the requested tokens overwrite them in the relevant environment variables. \code{restore_credentials()} can then be used to reset environment variables based on those from the saved environment; \code{delete_saved_credentials()} deletes the credentials without restoring them.
#' @template stsdots
#' @return A list.
#' @details \code{get_caller_identity} returns the account ID and ARN for the currently credentialled user; this can be used to confirm that an assumed role has indeed been assumed. \code{get_session_token} and \code{get_federation_token} generate and return temporary credentials. Details about the underlying behavior of the various API endpoints can be found at \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html}{Requesting Temporary Security Credentials}.
#' @references
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_GetCallerIdentity.html}{API Reference: GetCallerIdentity}
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_GetSessionToken.html}{API Reference: GetSessionToken}
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_GetFederationToken.html}{API Reference: GetFederationToken}
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html}{API Reference: AssumeRole}
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithSAML.html}{API Reference: AssumeRoleWithSAML}
#'  \href{http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html}{API Reference: AssumeRoleWithWebIdentity}
#' @examples
#' \dontrun{
#' get_caller_identity() # check current identity
#' get_session_token() # get token but do not use
#' 
#' x <- get_session_token(use = TRUE) # use temp token
#' get_caller_identity() # check that token is in use
#' 
#' restore_credentials() # return to original credentials
#' get_caller_identity() # check identity, again
#' }
#' @export
get_session_token <- function(duration = 900, id, code, use = FALSE, ...) {
    check_saved_credentials(use)
    query <- list(Action = "GetSessionToken")
    if (duration < 900 | duration > 129600) {
        stop("'duration' must be a value in seconds between 900 and 129600")
    }
    query[["DurationSeconds"]] <- duration
    if (!missing(id)) {
        query[["SerialNumber"]] <- id
        query[["TokenCode"]] <- code
    }
    out <- stsHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- out[["GetSessionTokenResponse"]][["GetSessionTokenResult"]][["Credentials"]]
    }
    if (isTRUE(use)) {
        set_credentials(out)
    }
    out
}

#' @rdname STS
#' @param name The name of the federated user.
#' @param policy An IAM policy document in JSON format.
#' @export
get_federation_token <- function(duration = 900, name, policy, use = FALSE, ...) {
    check_saved_credentials(use)
    query <- list(Action = "GetFederationToken")
    if (duration < 900 | duration > 129600) {
        stop("'duration' must be a value in seconds between 900 and 129600")
    }
    query[["DurationSeconds"]] <- duration
    query[["Name"]] <- name
    if (!missing(policy)) {
        query[["Policy"]] <- policy
    }
    out <- stsHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- out[["GetFederationTokenResponse"]][["GetFederationTokenResult"]]
        out <- c(out[["Credentials"]], out[["FederatedUser"]])
    }
    if (isTRUE(use)) {
        set_credentials(out)
    }
    out
}

#' @rdname STS
#' @export
get_caller_identity <- function(...) {
    query <- list(Action = "GetCallerIdentity")
    out <- stsHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- out[["GetCallerIdentityResponse"]][["GetCallerIdentityResult"]]
    }
    out
}

#' @rdname STS
#' @export
assume_role <- function(role, session, duration = 900, id, code, externalid, policy, use = FALSE, ...) {
    check_saved_credentials(use)
    query <- list(Action = "AssumeRole", RoleArn = get_rolearn(role))
    if (duration < 900 | duration > 129600) {
        stop("'duration' must be a value in seconds between 900 and 129600")
    }
    query[["DurationSeconds"]] <- duration
    query[["RoleSessionName"]] <- session
    if (!missing(id)) {
        query[["SerialNumber"]] <- id
        query[["TokenCode"]] <- code
    }
    if (!missing(externalid)) {
        query[["ExternalId"]] <- externalid
    }
    if (inherits(role, "iam_role") | !missing(policy)) {
        if (inherits(role, "iam_role")) {
            query[["Policy"]] <- role[["AssumeRolePolicyDocument"]]
        } else {
            query[["Policy"]] <- policy
        }
    }
    out <- stsHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- out[["AssumeRoleResponse"]][["AssumeRoleResult"]]
        out <- c(out[["Credentials"]], out[["AssumedRoleUser"]])
    }
    if (isTRUE(use)) {
        set_credentials(out)
    }
    out
}

saved_env_vars <- list2env(list(SAVED = FALSE))

check_saved_credentials <- function(use = TRUE) {
    if (isTRUE(use)) {
        if (isTRUE(saved_env_vars[["SAVED"]])) {
            stop("Saved credentials have not been restored.\nUse 'restore_credentials()' to restore them, or 'delete_saved_credentials()' to clear them.")
        }
    }
    return(TRUE)
}

save_credentials <- function() {
    saved_env_vars[["SAVED"]] <- TRUE
    saved_env_vars[["AWS_ACCESS_KEY_ID"]] <- Sys.getenv("AWS_ACCESS_KEY_ID")
    saved_env_vars[["AWS_SECRET_ACCESS_KEY"]] <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    saved_env_vars[["AWS_SESSION_TOKEN"]] <- Sys.getenv("AWS_SESSION_TOKEN")
    return(TRUE)
}

set_credentials <- function(credentials) {
    save_credentials()
    if ("AccessKeyId" %in% names(credentials)) {
        Sys.setenv("AWS_ACCESS_KEY_ID" = credentials[["AccessKeyId"]])
    }
    if ("SecretAccessKey" %in% names(credentials)) {
        Sys.setenv("AWS_SECRET_ACCESS_KEY" = credentials[["SecretAccessKey"]])
    }
    if ("SessionToken" %in% names(credentials)) {
        Sys.setenv("AWS_SESSION_TOKEN" = credentials[["SessionToken"]])
    }
    return(TRUE)
}

#' @rdname STS
#' @export
delete_saved_credentials <- function() {
    saved_env_vars[["SAVED"]] <- FALSE
    saved_env_vars[["AWS_ACCESS_KEY_ID"]] <- NA_character_
    saved_env_vars[["AWS_SECRET_ACCESS_KEY"]] <- NA_character_
    saved_env_vars[["AWS_SESSION_TOKEN"]] <- NA_character_
    return(TRUE)
}

#' @rdname STS
#' @export
restore_credentials <- function() {
    reset_from_env <- function(x) {
        if (!is.na(saved_env_vars[[x]])) {
            do.call("Sys.setenv", `names<-`(list(saved_env_vars[[x]]), x))
        }
    }
    reset_from_env("AWS_ACCESS_KEY_ID")
    reset_from_env("AWS_SECRET_ACCESS_KEY")
    reset_from_env("AWS_SESSION_TOKEN")
    delete_saved_credentials()
    return(TRUE)
}
