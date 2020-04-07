## aws.aim 0.1.8

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
