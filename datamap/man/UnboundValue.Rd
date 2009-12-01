\name{UnboundValue}
\Rdversion{1.1}
\alias{UnboundValue}
\title{Function to return the R internal unbound value. }
\description{
	When using a datamap object attached to the search path, there may be instances when
	it's appropriate to return the R internal unbound value. This function is here for that purpose.
}
\usage{
UnboundValue( )
}
\value{
	The R internal unbound value.
}
\seealso{
	\code{\link{newMapper}}
	\code{\link{newMap}}
	\code{\link{attach}}
}
\keyword{programming}
\keyword{environment}
