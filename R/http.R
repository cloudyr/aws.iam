#' @title Workhorse API Query Function
#' @description This is the low-level API querying function for IAM. Users do not need to use this directly.
#' @param query A named list specifying query arguments.
#' @param version A character string specifying an API version. Default is \dQuote{2010-05-08}.
#' @param verb A character string specifying an HTTP verb. Either \dQuote{GET} or \dQuote{POST}.
#' @param body A character string specifying a request body (if \code{verb = "POST"}).
#' @param region A character string specifying an AWS region. The default is drawn from environment variable \env{AWS_DEFAULT_REGION}.
#' @param key A character string specifying an AWS Access Key. The default is drawn from environment variable \env{AWS_ACCESS_KEY_ID}.
#' @param secret A character string specifying an AWS Secret Key. The default is drawn from environment variable \env{AWS_SECRET_ACCESS_KEY}.
#' @param dots Additional arguments passed to \code{\link[httr]{GET}} or \code{\link[httr]{POST}}
#' @import httr
#' @importFrom aws.signature signature_v4_auth
#' @importFrom XML xmlToList xmlParse
#' @importFrom jsonlite fromJSON
#' @export
iamHTTP <- function(query, verb = "GET", body = "", 
                    version = "2010-05-08",
                    region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                    ...) {
    if (!"Version" %in% names(query)) {
        query[["Version"]] <- version
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    if (key == "") {
        H <- add_headers(`x-amz-date` = d_timestamp)
    } else {
        S <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "iam",
               verb = verb,
               action = "/",
               query_args = query,
               canonical_headers = list(host = paste0("iam.amazonaws.com"),
                                        `x-amz-date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        H <- add_headers(`x-amz-date` = d_timestamp, 
                         `x-amz-content-sha256` = S$BodyHash,
                         Authorization = S$SignatureHeader)
    }
    if (verb == "GET") {
        r <- GET(paste0("https://iam.amazonaws.com"), H, query = query, ...)
    } else if (verb == "POST") {
        r <- POST(paste0("https://iam.amazonaws.com"), H, query = query, body = body, ...)
    }
    if (http_status(r)$category == "Client error") {
        x <- try(xmlToList(xmlParse(content(r, "text", encoding = "UTF-8"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(fromJSON(content(r, "text", encoding = "UTF-8"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    } else {
        out <- try(fromJSON(content(r, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text", encoding = "UTF-8"), "unknown")
        }
    }
    return(out)
}
