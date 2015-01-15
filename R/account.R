create_alias <- function(alias, ...) {
    query <- list(Action = "CreateAccountAlias", AccountAlias = alias)
    iamHTTP(query = query, ...)
}

delete_alias <- function(alias, ...) {
    query <- list(Action = "DeleteAccountAlias", AccountAlias = alias)
    iamHTTP(query = query, ...)
}

list_aliases <- function(n, marker, ...) {
    query <- list(Action = "ListAccountAliases")
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    iamHTTP(query = query, ...)
}


change_pwd <- function(old, new, ...) {
    query <- list(Action = "ChangePassword", NewPassword = new, OldPassword = old)
    iamHTTP(query = query, ...)
}


set_pwdpolicy <- 
function(allowchange, 
         hardexpire,
         length,
         previous,
         require,
         ...) {
    query <- list(Action = "UpdateAccountPasswordPolicy")
    query$AllowUsersToChangePassword <- tolower(as.character(allowchange))
    query$HardExpiry <- tolower(as.character(hardexpire))
    if(!missing(age)) {
        if(age > 1095 | age < 6)
            stop("'age' must be between 1 and 1095")
        query$MaxPasswordAge <- age
    }
    if(!missing(length)) {
        if(length > 128 | length < 6)
            stop("'length' must be between 6 and 128")
        query$MinPasswordLength <- min_length
    }
    if(!missing(previous)) {
        if(previous > 24 | age < 0)
            stop("'age' must be between 0 and 24")
        query$PasswordReusePrevention <- previous
    }
    if(!missing(require)){
        if("upper" %in% requirements)
            query$RequireUppercaseCharacters <- "true"
        if("lower" %in% requirements)
            query$RequireLowercaseCharacters <- "true"
        if("number" %in% requirements)
            query$RequireNumbers <- "true"
        if("symbol" %in% requirements) # ! @ # $ % ^ & * ( ) _ + - = [ ] { } | '
            query$RequireSymbols <- "true"
    }
    iamHTTP(query = query, ...)
}

get_pwdpolicy <- function(...) {
    query <- list(Action = "GetAccountPasswordPolicy")
    iamHTTP(query = query, ...)
}

get_account <- function(...) {
    query <- list(Action = "GetAccountSummary")
    iamHTTP(query = query, ...)
}


credential_report <- function(...) {
    query <- list(Action = "GenerateCredentialReport")
    iamHTTP(query = query, ...)
}

auth_details <- function(type, n, marker, ...) {
    query <- list(Action = "GetAccountAuthorizationDetails")
    if(!missing(type)) {
        vtypes <- c("User", "Role", "Group")
        if(any(!type %in% vtypes))
            stop("'type' must be one (or more) of: ", paste0(vtypes, collapse = ", "))
        a <- as.list(type)
        names(a) <- paste0("Filter.member.",1:length(a))
        query <- c(query, a)
    }
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    iamHTTP(query = query, ...)
}
