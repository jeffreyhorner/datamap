\name{install}
\Rdversion{1.1}
\alias{install}
\title{Function for creating bindings to named foreign objects in a datamap object}
\description{
	\code{install} creates the binding for the named foreign object
	in the datamap object. The foreign object is not created.
}
\usage{
	install( symbols, map )
}
\arguments{
	\item{symbols}{
		A non-zero length character vector naming the bindings to be created in the map.
	}
	\item{map}{
		A datamap object.
	}
}
\seealso{
	\code{\link{uninstall}}
}
\keyword{programming}
\keyword{environment}
