#' @rdname passwords
#' @title Change Password
#' @description Change password for currently authenticated user
#' @param old A character string specifying the current password
#' @param new A character string specifying the new password
#' @param allowchange Optionally, a logical indicating whether to allow users to change their own passwords (default is \code{FALSE}).
#' @param hardexpire Optionally, a logical indicating whether to prevent users from changing their passwords after they expire (default is \code{FALSE}).
#' @param age Optionally, a number of days (between 1 and 1095) specifying maximum valid age of an IAM user password.
#' @param length Optionally, a minimum password length between 6 and 128 (default is 6).
#' @param previous Optionally, a number specifying the number (between 1 and 24) of previous passwords that users are prevented from reusing. Default is 0.
#' @param requirements A character vector specifying whether to require specific password features, including: \dQuote{upper} (upper case character), \dQuote{lower} (lower case character), \dQuote{number} (a digit), and \dQuote{symbol} (a symbol). Multiple can be specified.
#' @template dots
#' @return \code{get_pwd_policy} returns a list. \code{change_pwd} and \code{set_pwd_policy} return a logical \code{TRUE} (if successful).
#' @references \href{http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_passwords_account-policy.html}{IAM Password Policies}
#' @export
change_pwd <- function(old, new, ...) {
    query <- list(Action = "ChangePassword", NewPassword = new, OldPassword = old)
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}

#' @rdname passwords
#' @export
get_pwd_policy <- function(...) {
    query <- list(Action = "GetAccountPasswordPolicy")
    out <- iamHTTP(query = query, ...)
    out[["GetAccountPasswordPolicyResponse"]][["GetAccountPasswordPolicyResult"]][["PasswordPolicy"]]
}

#' @rdname passwords
#' @export
set_pwd_policy <- 
function(allowchange, 
         hardexpire,
         age,
         length,
         previous,
         requirements,
         ...) {
    query <- list(Action = "UpdateAccountPasswordPolicy")
    if (!missing(allowchange)) {
        query[["AllowUsersToChangePassword"]] <- tolower(as.character(allowchange))
    }
    if (!missing(hardexpire)) {
        query[["HardExpiry"]] <- tolower(as.character(hardexpire))
    }
    if (!missing(age)) {
        if(age > 1095 | age < 6)
            stop("'age' must be between 1 and 1095")
        query[["MaxPasswordAge"]] <- age
    }
    if (!missing(length)) {
        if (length > 128 | length < 6) {
            stop("'length' must be between 6 and 128")
        }
        query[["MinPasswordLength"]] <- length
    }
    if (!missing(previous)) {
        if (previous > 24 | age < 0) {
            stop("'age' must be between 0 and 24")
        }
        query[["PasswordReusePrevention"]] <- previous
    }
    if (!missing(requirements)){
        if ("upper" %in% requirements) {
            query[["RequireUppercaseCharacters"]] <- "true"
        }
        if ("lower" %in% requirements) {
            query[["RequireLowercaseCharacters"]] <- "true"
        }
        if ("number" %in% requirements) {
            query[["RequireNumbers"]] <- "true"
        }
        if ("symbol" %in% requirements) { # ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
            query[["RequireSymbols"]] <- "true"
        }
    }
    out <- iamHTTP(query = query, ...)
    if (!inherits(out, "aws_error")) {
        out <- TRUE
    }
    out
}
