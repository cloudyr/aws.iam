#' @export
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

#' @export
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

#' @export
delete_login <- function(user, ...){
    query <- list(Action = "DeleteLoginProfile", UserName = user)
    iamHTTP(query = query, ...)
}

#' @export
get_login <- function(user, ...){
    query <- list(Action = "GetLoginProfile", UserName = user)
    iamHTTP(query = query, ...)
}
