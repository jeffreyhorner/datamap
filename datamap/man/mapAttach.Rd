\name{mapAttach}
\Rdversion{1.1}
\alias{mapAttach}
\title{Attach a datamap object to the search path}
\description{
	Creates a user defined database object and attaches it to the search path.	
	Arguments are the same as \code{attach}.
}
\usage{
mapAttach(map,pos=2,name=NULL,warn.conflicts=TRUE)
}
\arguments{
	\item{map}{
		datamap object.
	}
	\item{pos}{
		integer specify where to attach map on search path.
	}
	\item{name}{
		name to use for the attached database. If name is missing,
		then it defaults to 'datamap:MapType' where MapType is
		the type of the datamap object.
	}
	\item{warn.conflicts}{
		see \code{attach}.
	}
}
\value{
	See \code{attach}.
}
\seealso{
	\code{\link{attach}}
}
\keyword{programming}
\keyword{environment}
