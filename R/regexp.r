
#' @include utils.r
NULL

src <- list()

## The following Regular Expressions can be used for tokenizing,
## validating, and parsing SemVer version strings.

## ## Numeric Identifier
## A single `0`, or a non-zero digit followed by zero or more digits.

src$NUMERIC_IDENTIFIER <-        '0|[1-9]\\d*'
src$NUMERIC_IDENTIFIER_LOOSE <- '[0-9]+'

## ## Non-numeric Identifier
## Zero or more digits, followed by a letter or hyphen, and then zero or
## more letters, digits, or hyphens.

src$NON_NUMERIC_IDENTIFIER <-   '\\d*[a-zA-Z-][a-zA-Z0-9-]*'

## ## Main Version
## Three dot-separated numeric identifiers.

src$MAIN_VERSION <-
  '(' %+% src$NUMERIC_IDENTIFIER %+% ')\\.' %+%
  '(' %+% src$NUMERIC_IDENTIFIER %+% ')\\.' %+%
  '(' %+% src$NUMERIC_IDENTIFIER %+% ')'

src$MAIN_VERSION_LOOSE <-
  '(' %+% src$NUMERIC_IDENTIFIER_LOOSE %+% ')\\.' %+%
  '(' %+% src$NUMERIC_IDENTIFIER_LOOSE %+% ')\\.' %+%
  '(' %+% src$NUMERIC_IDENTIFIER_LOOSE %+% ')'

## ## Pre-release Version Identifier
## A numeric identifier, or a non-numeric identifier.

src$PRE_RELEASE_IDENTIFIER <-
  '(?:' %+% src$NUMERIC_IDENTIFIER %+%
  '|' %+% src$NON_NUMERIC_IDENTIFIER %+% ')'

src$PRE_RELEASE_IDENTIFIER_LOOSE <-
  '(?:' %+% src$NUMERIC_IDENTIFIER_LOOSE %+%
  '|' %+% src$NON_NUMERIC_IDENTIFIER %+% ')'

## ## Pre-release Version
## Hyphen, followed by one or more dot-separated pre-release version
## identifiers.

src$PRE_RELEASE <-
  '(?:-(' %+% src$PRE_RELEASE_IDENTIFIER %+%
  '(?:\\.' %+% src$PRE_RELEASE_IDENTIFIER %+% ')*))'

src$PRE_RELEASE_LOOSE <-
  '(?:-?(' %+% src$PRE_RELEASE_IDENTIFIER_LOOSE %+%
  '(?:\\.' %+% src$PRE_RELEASE_IDENTIFIER_LOOSE %+% ')*))'

## ## Build Metadata Identifier
## Any combination of digits, letters, or hyphens.

src$BUILD_IDENTIFIER <- '[0-9A-Za-z-]+'

## ## Build Metadata
## Plus sign, followed by one or more period-separated build metadata
## identifiers.

src$BUILD <-
  '(?:\\+(' %+% src$BUILD_IDENTIFIER %+%
  '(?:\\.' %+% src$BUILD_IDENTIFIER %+% ')*))'

## ## Full Version String
## A main version, followed optionally by a pre-release version and
## build metadata.

## Note that the only major, minor, patch, and pre-release sections of
## the version string are capturing groups.  The build metadata is not a
## capturing group, because it should not ever be used in version
## comparison.

FULL_PLAIN <-
  'v?' %+% src$MAIN_VERSION %+%
  src$PRE_RELEASE %+% '?' %+%
  src$BUILD %+% '?'

src$FULL <- '^' %+% FULL_PLAIN %+% '$'

## like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
## also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
## common in the npm registry.

LOOSE_PLAIN <-
  '[v=\\s]*' %+% src$MAIN_VERSION_LOOSE %+%
  src$PRE_RELEASE_LOOSE %+% '?' %+%
  src$BUILD %+% '?'

src$LOOSE <- '^' %+% LOOSE_PLAIN %+% '$'

src$GTLT <- '((?:<|>)?=?)'

## Something like "2.*" or "1.2.x".
## Note that "x.x" is a valid xRange identifer, meaning "any version"
## Only the first item is strictly required.

src$XRANGE_IDENTIFIER_LOOSE <- src$NUMERIC_IDENTIFIER_LOOSE %+% '|x|X|\\*'
src$XRANGE_IDENTIFIER <- src$NUMERIC_IDENTIFIER %+% '|x|X|\\*'

src$XRANGE_PLAIN <-
  '[v=\\s]*(' %+% src$XRANGE_IDENTIFIER %+% ')' %+%
  '(?:\\.(' %+% src$XRANGE_IDENTIFIER %+% ')' %+%
  '(?:\\.(' %+% src$XRANGE_IDENTIFIER %+% ')' %+%
  '(?:(' %+% src$PRE_RELEASE %+% ')' %+%
  ')?)?)?'

src$XRANGE_PLAIN_LOOSE <-
  '[v=\\s]*(' %+% src$XRANGE_IDENTIFIER_LOOSE %+% ')' %+%
  '(?:\\.(' %+% src$XRANGE_IDENTIFIER_LOOSE %+% ')' %+%
  '(?:\\.(' %+% src$XRANGE_IDENTIFIER_LOOSE %+% ')' %+%
  '(?:(' %+% src$PRE_RELEASE_LOOSE %+% ')' %+%
  ')?)?)?'

## >=2.x, for example, means >=2.0.0-0
## <1.x would be the same as "<1.0.0-0", though.

src$XRANGE <-
  '^' %+% src$GTLT %+% '\\s*' %+% src$XRANGE_PLAIN %+% '$'
src$XRANGE_LOOSE <-
  '^' %+% src$GTLT %+% '\\s*' %+% src$XRANGE_PLAIN_LOOSE %+% '$'

## Tilde ranges.
## Meaning is "reasonably at or greater than"

src$LONE_TILDE <- '(?:~>?)'

src$TILDE_TRIM <- '(\\s*)' %+% src$LONE_TILDE %+% '\\s+'
tilde_trim_replace <- '\\1~'

src$TILDE <- '^' %+% src$LONE_TILDE %+% src$XRANGE_PLAIN %+% '$'
src$TILDE_LOOSE <- '^' %+% src$LONE_TILDE %+% src$XRANGE_PLAIN_LOOSE %+% '$'

## Caret ranges.
## Meaning is "at least and backwards compatible with"

src$LONE_CARET <- '(?:\\^)'

src$CARET_TRIM <- '(\\s*)' %+% src$LONE_CARET %+% '\\s+'
caret_trim_replace <- '\\1^'

src$CARET <- '^' %+% src$LONE_CARET %+% src$XRANGE_PLAIN %+% '$'
src$CARET_LOOSE <- '^' %+% src$LONE_CARET %+% src$XRANGE_PLAIN_LOOSE %+% '$'

## A simple gt/lt/eq thing, or just "" to indicate "any version"

src$COMPARATOR_LOOSE =
  '^' %+% src$GTLT %+% '\\s*(' %+% LOOSE_PLAIN %+% ')$|^$'
src$COMPARATOR =
  '^' %+% src$GTLT %+% '\\s*(' %+% FULL_PLAIN %+% ')$|^$'


## An expression to strip any whitespace between the gtlt and the thing
## it modifies, so that `> 1.2.3` ==> `>1.2.3`

src$COMPARATOR_TRIM =
  '(\\s*)' %+% src$GTLT %+%
  '\\s*(' %+% LOOSE_PLAIN %+% '|' %+% src$XRANGE_PLAIN %+% ')'

## this one has to use the /g flag

comparator_trim_replace = '\\1\\2\\3'

## Something like `1.2.3 - 1.2.4`
## Note that these all use the loose form, because they'll be
## checked against either the strict or loose comparator form
## later.

src$HYPHEN_RANGE =
  '^\\s*(' %+% src$XRANGE_PLAIN %+% ')' %+%
  '\\s+-\\s+' %+%
  '(' %+% src$XRANGE_PLAIN %+% ')' %+%
  '\\s*$'

src$HYPHEN_RANGE_LOOSE =
  '^\\s*(' %+% src$XRANGE_PLAIN_LOOSE %+% ')' %+%
  '\\s+-\\s+' %+%
  '(' %+% src$XRANGE_PLAIN_LOOSE %+% ')' %+%
  '\\s*$'

## Star ranges basically just allow anything at all.

src$STAR = '(<|>)?=?\\s*\\*';
