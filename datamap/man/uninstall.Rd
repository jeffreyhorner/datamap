\name{uninstall}
\Rdversion{1.1}
\alias{uninstall}
\title{Function for removing named foreign objects from a  datamap object}
\description{
	\code{uninstall} removes the binding for the named foreign object
	from the datamap object. The foreign object is not deleted.
}
\usage{
	uninstall( symbols, map )
}
\arguments{
	\item{symbols}{
		A non-zero length character vector naming the bindings to be removed from the map.
	}
	\item{map}{
		A datamap object.
	}
}
\seealso{
	\code{\link{install}}
}
\keyword{programming}
\keyword{environment}
