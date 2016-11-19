#' @rdname login_profiles
#' @title Instance Profiles
#' @description Coming soon\dots
# @export
create_login <- function(user, password, reset, ...){
    query <- list(Action = "CreateLoginProfile", 
                  UserName = user)
    if (nchar(password) > 128) {
        stop("'password' must be <= 128 characters")
    }
    query$Password <- password
    if (!missing(reset)) {
        query$ResetRequired <- tolower(as.character(reset))
    }
    iamHTTP(query = query, ...)
}

#' @rdname login_profiles
# @export
update_login <- function(user, password, reset, ...){
    query <- list(Action = "UpdateLoginProfile", 
                  UserName = user)
    if (!missing(password)) {
        if (nchar(password) > 128) {
            stop("'password' must be <= 128 characters")
        }
        query$Password <- password
    }
    if(!missing(reset))
        query$ResetRequired <- tolower(as.character(reset))
    iamHTTP(query = query, ...)
}

#' @rdname login_profiles
# @export
delete_login <- function(user, ...){
    query <- list(Action = "DeleteLoginProfile", UserName = user)
    iamHTTP(query = query, ...)
}

#' @rdname login_profiles
# @export
get_login <- function(user, ...){
    query <- list(Action = "GetLoginProfile", UserName = user)
    iamHTTP(query = query, ...)
}
