create_odic <- function(url, thumbprint, client, ...){
    query <- list(Action = "CreateOpenIDConnectProvider")
    if(nchar(url) < 1 | nchar(url) > 255)
        stop("'url' must be between 1 and 255 characters")
    query$Url <- url
    if(any(nchar(thumbprint) != 40))
        stop("'thumbprint' values must be length 40")
    a <- as.list(thumbprint)
    names(a) <- paste0("ThumbprintList.member.", 1:length(a))
    query <- c(query, a)
    if(!missing(client)) {
        if(any(nchar(client) < 1) | any(nchar(client) > 255))
            stop("'client' values must be between 1 and 255 characters")
        a <- as.list(client)
        names(a) <- paste0("ClientIDList.member.", 1:length(a))
        query <- c(query, a)
    }
    iamHTTP(query = query, ...)    
}

update_odic <- function(arn, thumbprint, ...){
    query <- list(Action = "UpdateOpenIDConnectProviderThumbprint")
    if(any(nchar(thumbprint) != 40))
        stop("'thumbprint' values must be length 40")
    a <- as.list(thumbprint)
    names(a) <- paste0("ThumbprintList.member.", 1:length(a))
    query <- c(query, a)
    if(nchar(arn) < 20 | nchar(arn) > 2048)
        stop("'arn' must be between 20 and 2048 characters")
    query$OpenIDConnectProviderArn <- arn
    iamHTTP(query = query, ...)    
}



delete_odic <- function(arn, ...){
    query <- list(Action = "DeleteOpenIDConnectProvider")
    if(nchar(arn) < 20 | nchar(arn) > 2048)
        stop("'arn' must be between 20 and 2048 characters")
    query$OpenIDConnectProviderArn <- arn
    iamHTTP(query = query, ...)    
}

get_odic <- function(arn, ...){
    query <- list(Action = "GetOpenIDConnectProvider")
    if(nchar(arn) < 20 | nchar(arn) > 2048)
        stop("'arn' must be between 20 and 2048 characters")
    query$OpenIDConnectProviderArn <- arn
    iamHTTP(query = query, ...)    
}

list_odics <- function(...){
    query <- list(Action = "ListOpenIDConnectProviders")
    iamHTTP(query = query, ...)    
}

add_odic_client <- function(arn, client, ...){
    query <- list(Action = "AddClientIDToOpenIDConnectProvider")
    if(nchar(arn) < 20 | nchar(arn) > 2048)
        stop("'arn' must be between 20 and 2048 characters")
    query$OpenIDConnectProviderArn <- arn
    if(nchar(client) < 1 | nchar(client) > 255)
        stop("'client' must be between 1 and 255 characters")
    query$ClientId <- client
    iamHTTP(query = query, ...)    
}

remove_odic_client <- function(arn, client, ...){
    query <- list(Action = "RemoveClientIDFromOpenIDConnectProvider")
    if(nchar(arn) < 20 | nchar(arn) > 2048)
        stop("'arn' must be between 20 and 2048 characters")
    query$OpenIDConnectProviderArn <- arn
    if(nchar(client) < 1 | nchar(client) > 255)
        stop("'client' must be between 1 and 255 characters")
    query$ClientId <- client
    iamHTTP(query = query, ...)    
}
