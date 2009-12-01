\name{newMap}
\Rdversion{1.1}
\alias{newMap}
\title{Function for creating datamap objects}
\description{
	A datamap is an object database for accessing and storing foreign objects.
	\code{newMap} creates the object based on the provided mapper type.
}
\usage{
	newMap( type=NULL, ... )
}
\arguments{
	\item{type}{
		Character vector length one describing the mapper to use.
	}
	\item{\dots}{
		Arguments to be passed to the mapper's \code{.init()} function. They MUST be named 
		arguments, i.e. name=val.
	}
}
\value{
	An object of class code{datamap}.
}
\seealso{
	\code{\link{newMapper}}
}
\keyword{programming}
\keyword{environment}
