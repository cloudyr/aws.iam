#' @rdname STS
#' @title Temporary Session Tokens
#' @description Get a temporary credentials (i.e., a Session Token)
#' @param role string, role ARN or an object of class \dQuote{iam_role}.
#' @param session string, name of the temporary session, can be
#'     arbitrary and is mainly used to disambiguate multiple sessions
#'     using the same role.
#' @param duration numeric, optional, duration for which the
#'     credentials should be valid, in seconds, between 900 and
#'     129600. If not set, the back-end can decided.
#' @param id string, optional, the serial number or Amazon Resource
#'     Number for a multi-factor authentication (MFA) device.
#' @param code If \code{id} is specified, the value provided by the MFA device.
#' @param policy string, optional, specifying a JSON-formatted inline
#'     session policy. Note that for \code{get_federation_token} this
#'     entry is practically mandatory (unless \code{policy_arns} is
#'     used), since not specifying a policy results in a session
#'     without any permissions. Conversely, for \code{assume_role} the
#'     policy can only restrict what is already granted by the role's
#'     policy thus is rarely needed.
#' @param policy_arns character vector, optional, list of ARNs of IAM
#'     managed policies to use as session policy.
#' @param tags named character vector or named list of scalars,
#'     optional, if specified then the supplied key/value pairs (names
#'     are keys) are passed as session tags.
#' @param transitive.tags character vector, optional, specifies names
#'     of the session tags which will be passed to subsequent sessions
#'     in the role chain.
#' @param use logical (default \code{FALSE}), specifying whether to
#'     use these credentials for subsequent requests. If \code{TRUE},
#'     any currently used credentials are stored in a package
#'     environment (see \code{\link{save_credentials}}) and the
#'     requested tokens overwrite them in the relevant environment
#'     variables. \code{\link{restore_credentials}()} can then be used
#'     to restore environment variables based on those from the saved
#'     environment and \code{\link{delete_saved_credentials}()} deletes
#'     the credentials without restoring them.
#' @template stsdots
#' @return A list.
#' @details \code{get_caller_identity} returns the account ID and ARN
#'     for the currently credentialled user. This can be used to
#'     confirm that an assumed role has indeed been assumed.
#'
#'     \code{get_session_token} and \code{get_federation_token}
#'     generate and return temporary credentials.
#'
#'     Details about the underlying behavior of the various API
#'     endpoints can be found at
#'     \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html}{Requesting Temporary Security Credentials}.
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
#'
#' x <- get_session_token() # get token (T1) but do not use
#' set_credentials(x)       # now use those credentials
#' 
#' # assume a role
#' r <- assume_role("arn:aws:iam::111111111111:role/my-role", "test", use=TRUE)
#' get_caller_identity() # check that the role has been assumed
#' 
#' restore_credentials() # return to credentials of T1
#' restore_credentials() # return to root credentials
#' get_caller_identity() # check identity, again
#'
#' get_federation_token(name="Bob",
#'     policy_arns="arn:aws:iam::aws:policy/AmazonS3ReadOnlyAccess",
#'     use=TRUE)
#' aws.s3::bucketlist()
#' restore_credentials() # back to root
#' }
#' @export
get_session_token <- function(duration = 900, id, code, tags, use = FALSE, ...) {
    query <- list(Action = "GetSessionToken")
    if (duration < 900 | duration > 129600) {
        stop("'duration' must be a value in seconds between 900 and 129600")
    }
    query[["DurationSeconds"]] <- duration
    if (!missing(id)) {
        query[["SerialNumber"]] <- id
        query[["TokenCode"]] <- code
    }
    if (!missing(tags))
        query <- c(query, .mk.tags.query("Tags", tags))
    out <- stsHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- out[["GetSessionTokenResponse"]][["GetSessionTokenResult"]][["Credentials"]]
    }
    if (isTRUE(use)) {
        set_credentials(out)
    }
    invisible(out)
}

#' @rdname STS
#' @param name The name of the federated user.
#' @export
get_federation_token <- function(duration = 900, name, policy, policy_arns, use = FALSE, ...) {
    query <- list(Action = "GetFederationToken")
    if (duration < 900 | duration > 129600) {
        stop("'duration' must be a value in seconds between 900 and 129600")
    }
    query[["DurationSeconds"]] <- duration
    query[["Name"]] <- name
    if (!missing(policy)) {
        query[["Policy"]] <- policy
    }
    if (!missing(policy_arns)) {
        query <- c(query, .mk.tags.query("PolicyArns", array=policy_arns, suffix=".arn"))
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

## Creates query string entries for tags (<prefix>.member.<n>.Key=/.Value=)
## and arrays (<prefix>.member.<n><suffix>=)
## only one of v or array must be populated
.mk.tags.query <- function(prefix, v, array, suffix='') {
    if (missing(v) && !missing(array)) {
        array <- as.character(array)
        names(array) <- paste0(prefix, ".member.", seq_along(array), suffix)
        as.list(array)
    } else {
        keys <- names(v)
        vals <- as.character(v)
        names(keys) <- paste0(prefix, ".member.", seq_along(keys), ".Key")
        names(vals) <- paste0(prefix, ".member.", seq_along(vals), ".Value")
        as.list(c(keys, vals))
    }
}

#' @rdname STS
#' @param externalid A unique identifier that is used by third parties when assuming roles in their customers' accounts.
#' @export
assume_role <- function(role, session, duration, id, code, externalid,
                        policy, tags, transitive.tags, use = FALSE, ...) {
    query <- list(Action = "AssumeRole", RoleArn = get_rolearn(role))
    if (!missing(duration)) {
        if (duration < 900 || duration > 129600)
            stop("'duration' must be a value in seconds between 900 and 129600")
        query[["DurationSeconds"]] <- duration
    }
    query[["RoleSessionName"]] <- session
    if (!missing(id)) {
        query[["SerialNumber"]] <- id
        query[["TokenCode"]] <- code
    }
    if (!missing(tags))
        query <- c(query, .mk.tags.query("Tags", tags))
    if (!missing(transitive.tags))
        query <- c(query, .mk.tags.query("TransitiveTagKeys", array=transitive.tags))
    if (!missing(externalid)) {
        query[["ExternalId"]] <- externalid
    }
    if (!missing(policy)) {
        query[["Policy"]] <- policy
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

.saved <- new.env(parent=emptyenv())


## which variables to save/restore, names correspond to the AWS API names, values to env var names
.cred.vars <- c(AccessKeyId="AWS_ACCESS_KEY_ID", SecretAccessKey="AWS_SECRET_ACCESS_KEY", SessionToken="AWS_SESSION_TOKEN")

#' @rdname credentials
#' @title Save/restore/manage session credentials
#' @description The following functions manage the environment
#'     variables \code{AWS_ACCESS_KEY_ID},
#'     \code{AWS_SECRET_ACCESS_KEY} and \code{AWS_SESSION_TOKEN} used
#'     for credentials for all AWS API calls.
#'
#'     \code{save_credentials} saves the current credentials to a
#'     stack of credentials kept in the session. Always returns
#'     \code{TRUE}.
#' 
#'     \code{restore_credentials} restores the last saved credentials
#'     and pops them off the stack.
#' 
#'     \code{delete_saved_credentials} removes the last saved
#'     credentials without using them.
#'
#'     \code{set_credentials} uses credentials list as supplied by the
#'     REST API and makes them current by assigning their values to
#'     the corresponding \code{AWS_*} environment variables. If
#'     \code{save.previous} is \code{TRUE} then the currently used
#'     credentials are first saved on the stack ebfore being replaced
#'     with the new ones.
#'
#'     Most functions in the \code{STS} section call
#'     \code{set_credentials()} automatically if \code{use = TRUE} is
#'     set.
#' @param credentials list, credentials as received from the REST API
#'     call, they should contain to following elements:
#'     \code{AccessKeyId}, \code{SecretAccessKey} and
#'     \code{SessionToken})
#' @param save.previous logical, if \code{TRUE} the current
#'     credentials are saved first using \code{save_credentials}
#'     before the new credentials are applied.
#' @param all logical, if \code{TRUE} then removes all credentials
#'     from the stack, otherwise only the last ones.
#' @param pop logical, if \code{TRUE} then the credentials are
#'     restored and then removed from the stack.
#' @param root logical, if \code{FALSE} then last saved credentials
#'     are used. if \code{TRUE} then goes down the stack to the first
#'     saved credentials. If both \code{root} and \code{pop} are
#'     \code{TRUE} then all credentials are removed from the stack.
#' @details Since \code{aws.iam} version 0.1.8 the credentials are
#'     kept on a stack, so it is possible to use
#'     \code{save_credentials()} several times without restoring
#'     them. This allows role chaining. At the end of a chained
#'     session it is possible to get back to the main credentials using
#'     \code{restore_credentials(pop=TRUE, root=TRUE)}.
#' @export
save_credentials <- function() {
    .cred <- sapply(.cred.vars, Sys.getenv)
    if (is.null(.saved$cred)) .saved$cred <- list(.cred) else .saved$cred <- list(.cred, .next=.saved$cred)
    TRUE
}

#' @rdname credentials
#' @export
set_credentials <- function(credentials, save.previous=TRUE) {
    if (!is.null(credentials) && !is.list(credentials))
        stop("invalid credentials, must be a list")
    if (save.previous) save_credentials()
    v <- sapply(names(.cred.vars), function(e) {
        v <- credentials[[e]]
        if (is.null(v)) "" else v
    })
    names(v) <- .cred.vars
    do.call(Sys.setenv, as.list(v))
    TRUE
}

#' @rdname credentials
#' @export
delete_saved_credentials <- function(all=FALSE) {
    if (all) {
        .saved$cred <- NULL
        TRUE
    } else {
        .cred <- .saved$cred
        if (is.null(.cred)) {
            warning("There are no saved credentials")
            FALSE
        } else {
            .saved$cred <- .cred$.next
            TRUE
        }
    }
}

#' @rdname credentials
#' @export
restore_credentials <- function(pop=TRUE, root=FALSE) {
    .cred <- .saved$cred
    if (is.null(.cred)) {
        warning("There are no saved credentials, no change")
        FALSE
    } else {
        if (root) {
            ## go down to the root
            while (!is.null(.cred$.next))
                .cred <- .cred$.next

            ## if popping then remove everything except the root
            if (pop)
                .saved$cred <- .cred
        }
        vars <- .cred[[1]]
        names(vars) <- .cred.vars
        do.call(Sys.setenv, as.list(vars))
        if (pop) delete_saved_credentials() else TRUE
    }
}
