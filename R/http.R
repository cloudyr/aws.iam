#' @rdname iamHTTP
#' @title Workhorse API Query Functions
#' @description These are the low-level API querying functions for IAM and STS. Users do not need to use these directly.
#' @param verb A character string specifying an HTTP verb. Either \dQuote{GET} or \dQuote{POST}.
#' @param query A named list specifying query arguments.
#' @param headers A list of headers to pass to the HTTP request.
#' @param version A character string specifying an API version. Default is \dQuote{2010-05-08}.
#' @param body A character string specifying a request body (if \code{verb = "POST"}).
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string specifying an AWS region. See \code{\link[aws.signature]{locate_credentials}}.
#' @param key A character string specifying an AWS Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string specifying an AWS Secret Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string specifying an AWS temporary Session Token to use in signing a request. See \code{\link[aws.signature]{locate_credentials}}.
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}} or \code{\link[httr]{POST}}
#' @import httr
#' @importFrom aws.signature signature_v4_auth locate_credentials
#' @importFrom xml2 read_xml as_list
#' @importFrom jsonlite fromJSON
#' @export
iamHTTP <- 
function(
  verb = "GET",
  query,
  headers = list(),
  body = "",
  version = "2010-05-08",
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"), 
  key = NULL, 
  secret = NULL, 
  session_token = NULL,
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    if (!"Version" %in% names(query)) {
        query[["Version"]] <- version
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "iam",
           verb = verb,
           action = "/",
           query_args = query,
           canonical_headers = list(host = paste0("iam.amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = "",
           key = key, 
           secret = secret,
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["x-amz-content-sha256"]] <- Sig$BodyHash
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    # execute request
    if (verb == "GET") {
        r <- GET(paste0("https://iam.amazonaws.com"), H, query = query, ...)
    } else if (verb == "POST") {
        r <- POST(paste0("https://iam.amazonaws.com"), H, query = query, body = body, ...)
    }
    if (http_error(r)) {
        x <- try(as_list(read_xml(content(r, "text", encoding = "UTF-8"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text", encoding = "UTF-8"), "unknown")
        }
    }
    return(out)
}

#' @rdname iamHTTP
#' @export
stsHTTP <-
function(
  query,
  headers = list(),
  body = "",
  version = "2011-06-15",
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"), 
  key = NULL, 
  secret = NULL, 
  session_token = NULL,
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    if (!"Version" %in% names(query)) {
        query[["Version"]] <- version
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "sts",
           verb = "GET",
           action = "/",
           query_args = query,
           canonical_headers = list(host = paste0("sts.amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = "",
           key = key, 
           secret = secret, 
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["x-amz-content-sha256"]] <- Sig$BodyHash
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    # execute request
    r <- GET(paste0("https://sts.amazonaws.com"), H, query = query, ...)
    if (http_error(r)) {
        x <- try(as_list(read_xml(content(r, "text", encoding = "UTF-8"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text", encoding = "UTF-8"), "unknown")
        }
    }
    return(out)
}
