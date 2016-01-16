# Server Certificates

post_servercert <- function(file, name, privatekey, path, ...) {
    query <- list(Action = "UploadServerCertificate")
    if(nchar(privatekey) > 16384 | nchar(privatekey) < 1)
        stop("'name' must be between 1 and 16384 characters")
    query$ServerCertificateName
    if(nchar(name) > 128 | nchar(name) < 1)
        stop("'name' must be between 1 and 128 characters")
    query$ServerCertificateName
    if(!missing(path)) {
        if(nchar(path) > 512 | nchar(path) < 1)
            stop("'path' must be between 1 and 512 characters")
        query$Path <- path
    }
    r <- iamHTTP(query = query, 
                 verb = "POST",
                 body = list(CertificateBody = upload_file(file)), 
                 ...)
    return(r)
}

get_servercert <- function(cert, ...){
    query <- list(Action = "GetServerCertificate", ServerCertificateName = cert)
    iamHTTP(query = query, ...)
}

update_servercert <- function(cert, newname, newpath, ...) {
    query <- list(Action = "UpdateServerCertificate", ServerCertificateName = cert)
    if(!missing(newname)){
        if(nchar(newname) < 1 | nchar(newname) > 128)
            stop("'newname' must be between 1 and 128 characters")
        query$NewServerCertificateName <- newname
    }
    if(!missing(path)){
        if(nchar(path) < 1 | nchar(path) > 512)
            stop("'path' must be between 1 and 512 characters")
        query$NewPath <- path
    }
    iamHTTP(query = query, ...)
}

delete_servercert <- function(cert, ...){
    query <- list(Action = "DeleteServerCertificate", ServerCertificateName = cert)
    iamHTTP(query = query, ...)
}

list_servercerts <- function(n, marker, prefix, ...) {
    query <- list(Action = "ListServerCertificates")
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(prefix))
        query$Prefix <- prefix
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    r <- iamHTTP(query = query, ...)
    return(r)
}


# Signing Certificates

post_signingcert <- function(file, user, ...) {
    query <- list(Action = "UploadSigningCertificate")
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    r <- iamHTTP(query = query, 
                 verb = "POST", 
                 body = list(CertificateBody = upload_file(file)), 
                 ...)
    return(r)
}


update_signingcert <- function(cert, status, user, ...) {
    query <- list(Action = "UpdateSigningCertificate", CertificiateId = cert)
    vstatus <- c("Active", "Inactive")
    if(!status %in% vstatus)
        stop("'status' must be one of: ", paste0(vstatus, collapse = ", "))
    query$Status <- status
    if(!missing(user)){
        if(nchar(user) < 1 | nchar(user) > 128)
            stop("'user' must be between 1 and 128 characters")
        query$UserName <- user
    }
    if(!missing(path)){
        if(nchar(path) < 1 | nchar(path) > 512)
            stop("'path' must be between 1 and 512 characters")
        query$NewPath <- path
    }
    iamHTTP(query = query, ...)
}


delete_signingcert <- function(cert, user, ...){
    query <- list(Action = "DeleteSigningCertificate", CertificateId = cert)
    if(!missing(user))
        query$UserName <- user
    iamHTTP(query = query, ...)
}

list_signingcerts <- function(n, marker, user, ...) {
    query <- list(Action = "ListSigningCertificates")
    if(!missing(marker))
        query$Marker <- marker
    if(!missing(user))
        query$UserName <- user
    if(!missing(n)) {
        if(!n %in% 1:1e3)
            stop("'n' must be in 1:1000")
        query$MaxItems <- n
    }
    r <- iamHTTP(query = query, ...)
    return(r)
}
