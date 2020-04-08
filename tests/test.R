## simple test suite - avoid testthat! It has an insane amount of
## unnecessary dependencies. A test package should have exactly 0

assert <- function(msg, what) {
    cat("   . ", msg,"\n")
    stopifnot(what)
    .GlobalEnv$ok <- .GlobalEnv$ok + 1L
}

## none of these are fatal
info <- function(...) message(" -- ", ...)
err  <- function(...) message(" ** ERROR: ", ...)
warn <- function(...) message(" !! ", ...)

## all warnings (unless suppressed) are errors
options(warn=2)

library(aws.iam)

.GlobalEnv$ok <- 0L

info("Locating AWS credentials")
## let's see if we even have any credentials
root <- aws.signature::locate_credentials()

if (!nzchar(root$key) || !nzchar(root$secret)) {
    err("Cannot obtain any S3 credentials, cannot perform any tests!")
} else {
    ## Unfortuantely, aws.signature uses non-standard names
    ## in their credentials object, so we have to re-map it
    rc <- list(AccessKeyId=root$key,
               SecretAccessKey=root$secret,
               SessionToken=root$session_token)
    ## set root credentials without keeping any existing
    assert("set_credentials with located credentials",
           set_credentials(rc, save.previous=FALSE))

    info("Checking identity")
   
    assert("get_caller_identity",
           !is.null((ci <- get_caller_identity())$Account))

    ## prefix for iam ARNs in this account
    arn.prefix <- paste0("arn:aws:iam::", ci$Account, ":")

    info("Testing session tokens")
    assert("get_federation_token",
           !is.null((ft <- get_federation_token(name="Bob", policy_arns="arn:aws:iam::aws:policy/AmazonS3ReadOnlyAccess", use=TRUE))$SessionToken))
    if (requireNamespace("aws.s3", quietly=TRUE)) {
        assert("S3 bucketlist() with federated token", is.list(aws.s3::bucketlist()))
    }
    restore_credentials()
    
    assert("get_session_token",
           !is.null((stc <- get_session_token(tags=c(intent="test")))$SessionToken))
    assert(" - set new credentials",
           set_credentials(stc))

    assert("get_caller_identity()", is.character(get_caller_identity()$Account))

    ## explicitly save the temp creds
    save_credentials()
    
    ## get to root cred without losing the first token
    ## you cannot list roles using temp tokens so we need root
    restore_credentials(root=TRUE, pop=FALSE)
    
    info("Testing roles")
    assert("list_roles()", is.list(roles <- list_roles()))
    rn <- sapply(roles, function(o) o$RoleName)

    ## get back to temporary creds
    restore_credentials()

    ## AMI API will fail (this is Amazon's rule)
    assert("Failing IAM API on temp token",
           inherits(suppressWarnings(list_roles()), "aws_error"))

    ## but we can still use the temp creds to assume a role    
    if (!any(rn == "S3-automated-tests")) {
        info("Cannot find S3-automated-tests role, skipping tests requiting a role")
    } else {
        atr <- roles[[which(rn == "S3-automated-tests")]]
        assert("print role", nzchar(capture.output(print(atr))))
        ## WARN: cannot test tagging, need sts:TagSession on resource: arn:aws:iam::xxxxxxxx:role/S3-automated-tests
        assert("assume_role",
               is.character((r <- assume_role(atr, "test"))$AccessKeyId))
        set_credentials(r)
        assert("verify role identity",
               isTRUE(grepl("assumed-role/S3-automated-tests/", get_caller_identity()$Arn)))

        assert("invalid request", 
               inherits(suppressWarnings(assume_role("invalid", "test", 1234, id="foo", code=123, tags=c(foo="bar",test=1), transitive.tags="foo")), "aws_error"))
        ## pop role creds
        restore_credentials()
    }

    info("Cleanup")
    restore_credentials(root=TRUE)
    set_credentials(NULL)
    delete_saved_credentials(all=TRUE)
    cat("\n=========\n", ok, "successful tests.\n")
}
