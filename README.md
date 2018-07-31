# AWS IAM Client Package

**aws.iam** is a simple client package for the Amazon Web Services (AWS) [Identity and Access Management (IAM)](http://aws.amazon.com/iam/) and [Simple Token Service (STS)](http://docs.aws.amazon.com/STS/latest/APIReference) API, which can be used to manage access credentials for AWS services.

To use the package, you will need an AWS account and to enter your credentials into R. Your keypair can be generated on the [IAM Management Console](https://aws.amazon.com/) under the heading *Access Keys*. Note that you only have access to your secret key once. After it is generated, you need to save it in a secure location. New keypairs can be generated at any time if yours has been lost, stolen, or forgotten. The [**aws.iam** package](https://github.com/cloudyr/aws.iam) profiles tools for working with IAM, including creating roles, users, groups, and credentials programmatically; it is not needed to *use* IAM credentials.

A detailed description of how credentials can be specified is provided at: https://github.com/cloudyr/aws.signature/. The easiest way is to simply set environment variables on the command line prior to starting R or via an `Renviron.site` or `.Renviron` file, which are used to set environment variables in R during startup (see `? Startup`). They can be also set within R:

```R
Sys.setenv("AWS_ACCESS_KEY_ID" = "mykey",
           "AWS_SECRET_ACCESS_KEY" = "mysecretkey",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "mytoken")
```


## Code Examples

The package gives basically fully access to IAM functionality to create and manage groups and users, including creating and managing AWS credentials on-the-fly.


```r
library("aws.iam")

# create user
u <- create_user("example-user")

# create group
(g <- create_group("example"))
```

```
## GroupId:    AGPAIZHQL3T5B4GGXXVGU 
## GroupName:  /example 
## Arn:        arn:aws:iam::920667304251:group/example 
## CreateDate: 1498901165
```

```r
# rename group
update_group(g, "example2")
```

```
## [1] TRUE
```

```r
# add user to group
add_user(u, "example2")
```

```
## [1] TRUE
```

```r
get_group_users("example2")
```

```
## [[1]]
## UserName:   /example-user 
## UserId:     AIDAI54ZINZ2F3NUVS4XW 
## Arn:        arn:aws:iam::920667304251:user/example-user 
## CreateDate: 1498901164 
## 
## attr(,"group")
## GroupId:    AGPAIZHQL3T5B4GGXXVGU 
## GroupName:  /example2 
## Arn:        arn:aws:iam::920667304251:group/example2 
## CreateDate: 1498901165
```

```r
# create AWS credentials for user
k <- create_key(u)
# update key to inactive
update_key(k, u, "Inactive")
```

```
## [1] TRUE
```

```r
list_keys(u)
```

```
## [[1]]
## AccessKeyId: AKIAIUSX3NZJVKNGNTRA 
## CreateDate:  1498901167 
## Status:      Inactive 
## UserName:    example-user
```

```r
# cleanup
delete_key(k)
```

```
## [1] TRUE
```

```r
remove_user(u, "example2")
```

```
## [1] TRUE
```

```r
delete_user(u)
```

```
## [1] TRUE
```

```r
delete_group("example2")
```

```
## [1] TRUE
```

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/aws.iam)](https://cran.r-project.org/package=aws.iam)
![Downloads](https://cranlogs.r-pkg.org/badges/aws.iam)
[![Build Status](https://travis-ci.org/cloudyr/aws.iam.png?branch=master)](https://travis-ci.org/cloudyr/aws.iam)
[![codecov.io](https://codecov.io/github/cloudyr/aws.iam/coverage.svg?branch=master)](https://codecov.io/github/cloudyr/aws.iam?branch=master)

This package is not yet on CRAN. To install the latest development version you can install from the cloudyr drat repository:

```R
# latest stable version
install.packages("aws.iam", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
```

Or, to pull a potentially unstable version directly from GitHub:

```R
if (!require("ghit")) {
    install.packages("ghit")
}
ghit::install_github("cloudyr/aws.iam")
```

To install the latest version from CRAN, simply use `install.packages("aws.iam")`.

---
[![cloudyr project logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
