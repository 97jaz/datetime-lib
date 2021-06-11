# datetime-lib
Date and time library for Racket

Intended to be a successor to [`gregor`](https://github.com/97jaz/gregor/).

## Differences from `gregor`

- `datetime-lib`, like `gregor` is meant to be a more featureful
  replacement for Racket's built-in date and time data structures and
  functions. But `datetime-lib` additionally permits its generic
  functions to work _with_ the built-in data structures. Specifically,
  the `date` and `date*` structs from `racket/base` that satisfy
  `base-date/compat?` will also satisfy `date-provider?`,
  `time-provider?`, `datetime-provider?`, `utc-offset-provider?`, as
  well as the relevant `*-arithmetic-provider?` predicates.
- This library uses a simpler and leaner internal representation. For
  example, in `gregor`, a `date` struct contains the year, month, and
  day-of-month (which are contained in their own `ymd` struct), in
  addition to a Julian Day number. The reason for this is that some
  operations are more efficient when operating on the former, and
  others when operating on the latter. In `datetime-lib`, however, we
  just store the year, month, and day. If/when we need the JDN, we
  compute it on-the-fly.
- The `moment` struct from `gregor` is replaced by two different
  structs: `datetime/offset` and `datetime/tz`. The former represents
  a datetime with a UTC offset; the latter has an IANA time zone.
- `datetime-lib` maintains a stricter type-discipline than
  `gregor`. For example, in `gregor`, the `period-between` function
  accepts two `datetime-provider?`s as input, which implies that it
  even makes sense to ask for how much time elapsed between a
  `datetime` and a `moment`, even though it does not.
- Whereas `gregor` assumes that users will use generics for nearly all
  operations other than construction, `datetime-lib` provides many
  more struct-specific functions, so that a user can choose to work
  with generics or not.
- `datetime-lib` will ship with a much more modest formatting and
  parsing library than `gregor`. Interested third parties are welcome
  (and encouraged) to provide a fully-featured and localized
  formatting and parsing library. (My current intention for this
  library is to provide basic `strftime`/`strptime`-like
  functionalty, in addition to some ISO 8601 functionality.)

## TODO

- Get rid of `period` (as it currently exists) and replace it with two
  different data structures that replace the current concepts of
  `date-period` and `time-period`. An [issue in the gregor
  project](https://github.com/97jaz/gregor/issues/46) has me leaning
  toward this separation.
- I think some functions are currently both in `date.rkt` and in
  `query.rkt`. I think my intention was to remove them from
  `date.rkt`, since they don't actually operate on `date`s.
- Revisit the flooring integer division functions in `math.rkt`. Maybe
  those implementations are ok. They do seem to work, at any rate, but
  the bitwise operations just seem out of place there. I don't
  remember where I got that idea from.
- Maybe add interval types. The tricky thing here is getting the API
  right, since there are some operations that make sense when the
  component/witness type is discrete (as with a `date-interval`)
  vs. when it is continuous or continuous-ish (as with a
  `time-interval`).

## Things to consider

- Real UTC support (i.e., support for leap seconds)
- Support for other time scales (e.g., TAI)

