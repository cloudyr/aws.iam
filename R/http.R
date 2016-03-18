#' @import httr
#' @importFrom signature_v4_auth
#' @importFrom XML xmlToList xmlParse
#' @importFrom jsonlite fromJSON
#' @export
iamHTTP <- function(query, verb = "GET", body = "", 
                    region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                    ...) {
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
               canonical_headers = list(host = paste0("iam.",region,".amazonaws.com"),
                                        `x-amz-date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        H <- add_headers(`x-amz-date` = d_timestamp, 
                         `x-amz-content-sha256` = S$BodyHash,
                         Authorization = S$SignatureHeader)
    }
    if (verb == "GET") {
        r <- GET(paste0("https://iam.",region,".amazonaws.com"), H, query = query, ...)
    } else if (verb == "POST") {
        r <- POST(paste0("https://iam.",region,".amazonaws.com"), H, query = query, body = body, ...)
    }
    if (http_status(r)$category == "client error") {
        x <- try(xmlToList(xmlParse(content(r, "text"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(fromJSON(content(r, "text"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    } else {
        out <- try(fromJSON(content(r, "text")), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text"), "unknown")
        }
    }
    return(out)
}
