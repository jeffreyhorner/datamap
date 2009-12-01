\name{delete}
\Rdversion{1.1}
\alias{delete}
\title{Function for deleting foreign objects in a datamap object}
\description{
	\code{delete} removes the named foreign objects.
}
\usage{
	delete( symbols, map, UNINSTALL=TRUE )
}
\arguments{
	\item{symbols}{
		A non-zero length character vector naming the foreign objects to delete.
	}
	\item{map}{
		A datamap object.
	}
	\item{UNINSTALL}{ 
		Logical. Whether or not to remove the binding from the
		map. Defaults to TRUE.
	}
}
\seealso{
	\code{\link{uninstall}}
	\code{\link{delete}}
}
\keyword{programming}
\keyword{environment}
