create_saml <- function(name, metadata, ...) {
    query <- list(Action = "CreateSAMLProvider",
                  Name = name, 
                  SAMLMetadataDocument = metadata)
    iamHTTP(query = query, ...)
}

delete_saml <- function(saml, ...) {
    query <- list(Action = "DeleteSAMLProvider", SAMLProviderArn = saml)
    iamHTTP(query = query, ...)
}

get_saml <- function(saml, ...) {
    query <- list(Action = "GetSAMLProvider", SAMLProviderArn = saml)
    iamHTTP(query = query, ...)
}

list_samls <- function(...) {
    query <- list(Action = "ListSAMLProviders")
    iamHTTP(query = query, ...)
}

update_saml <- function(saml, metadata, ...) {
    query <- list(Action = "UpdateSAMLProvider",
                  SAMLProviderArn = saml, 
                  SAMLMetadataDocument = metadata)
    iamHTTP(query = query, ...)
}
