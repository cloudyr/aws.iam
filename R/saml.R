#' @export
create_saml <- function(name, metadata, ...) {
    query <- list(Action = "CreateSAMLProvider",
                  Name = name, 
                  SAMLMetadataDocument = metadata)
    iamHTTP(query = query, ...)
}

#' @export
delete_saml <- function(saml, ...) {
    query <- list(Action = "DeleteSAMLProvider", SAMLProviderArn = saml)
    iamHTTP(query = query, ...)
}

#' @export
get_saml <- function(saml, ...) {
    query <- list(Action = "GetSAMLProvider", SAMLProviderArn = saml)
    iamHTTP(query = query, ...)
}

#' @export
list_samls <- function(...) {
    query <- list(Action = "ListSAMLProviders")
    iamHTTP(query = query, ...)
}

#' @export
update_saml <- function(saml, metadata, ...) {
    query <- list(Action = "UpdateSAMLProvider",
                  SAMLProviderArn = saml, 
                  SAMLMetadataDocument = metadata)
    iamHTTP(query = query, ...)
}
