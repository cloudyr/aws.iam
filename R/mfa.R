create_mfa <- function(mfa, path, ...){
    query <- list(Action = "CreateVirtualMFADevice", VirtualMFADeviceName = mfa)
    if(!missing(path))
        query$Path <- path
    iamHTTP(query = query, ...)
}

delete_mfa <- function(mfa, ...) {
    query <- list(Action = "DeleteVirtualMFADevice", SerialNumber = mfa)
    iamHTTP(query = query, ...)
}

list_mfas <- function(n, marker, user, status, virtual = FALSE, ...) {
    if(virtual) {
        query <- list(Action = "ListVirtualMFADevices")
        if(!missing(status))
            vstatus <- c("Assigned", "Unassigned", "Any")
            if(!status %in% vstatus)
                stop("'status' must be one of: ", paste0(status, collapse = ", "))
            query$AssignmentStatus <- status
        }
    } else {
        query <- list(Action = "ListMFADevices")
        if(!missing(user)){
            if(nchar(user) < 1 | nchar(user) > 128)
                stop("'user' must be between 1 and 128 characters")
            query$UserName <- user
        }    
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

activate_mfa <- function(mfa, user, code1, code2, ...) {
    query <- list(Action = "EnableMFADevice", 
                  SerialNumber = mfa, 
                  AuthenticationCode1 = code1,
                  AuthenticationCode2 = code2)
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)
}

deactivate_mfa <- function(mfa, user, ...){
    query <- list(Action = "DeactiveMFADevice", SerialNumber = mfa)
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)
}

sync_mfa <- function(mfa, user, code1, code2, ...) {
    query <- list(Action = "ResyncMFADevice", 
                  SerialNumber = mfa, 
                  AuthenticationCode1 = code1,
                  AuthenticationCode2 = code2)
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    iamHTTP(query = query, ...)
}
