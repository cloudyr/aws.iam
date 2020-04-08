## aws.iam 0.1.9 (under development)

* assume_role() was incorrectly passing role's policy with the request
  in case the role was an \code{iam_role} object, which is both
  unnecessary (the role policy is aways applied regardless) and
  depending on the role policy illegal (e.g., if the role defined
  principal-based restrictions).
* add `get_federation_token(..., policy_arns=)` to support IAM-managed
  policies in addition to inline session policies.
* added a test suite (#1)

## aws.iam 0.1.8

* Fixed missing `location_credentials` import (#14)
* Added support for session tags via `tags` and `transitive.tags` parameters.
* Re-factored the way credentials are stored. Chained credentials are
  supported by providing a stack of credentials. Credential
  management functions are now exported and documented.

## aws.iam 0.1.7

## aws.iam 0.1.6

* Expanded documentation.

## aws.iam 0.1.5

* Bump **aws.signature** dependency to 0.3.4.

## aws.iam 0.1.4

* Documentation fixes. (#5)
* Swapped import of XML to xml2.

## aws.iam 0.1.3

* Implement the Security Token Service (STS) API. (#4)

## aws.iam 0.1.3

* All exported functions should be working.
* Add package documentation.

## aws.iam 0.1.1

* Initial release.
