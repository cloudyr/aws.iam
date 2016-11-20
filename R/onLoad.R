.onLoad <- function(libname, pkgname) {
    saved_env_vars[["SAVED"]] <- FALSE
    saved_env_vars[["AWS_ACCESS_KEY_ID"]] <- NA_character_
    saved_env_vars[["AWS_SECRET_ACCESS_KEY"]] <- NA_character_
    saved_env_vars[["AWS_SESSION_TOKEN"]] <- NA_character_
}
