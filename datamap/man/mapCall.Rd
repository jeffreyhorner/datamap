\name{mapCall}
\Rdversion{1.1}
\alias{mapCall}
\title{Call functions located in datamap objects}
\description{
	Some mappers may contain various functions in addition to the initialization and
accessor functions. \code{mapCall} allows the users to call those functions.
}
\usage{
mapCall(map,funName,...)
}
\arguments{
	\item{map}{
		a datamap object;
	}
	\item{funName}{
		name of the function to call.
	}
	\item{\dots}{
		Arguments to be passed to \code{funName}.
	}
}
\seealso{
	\code{\link{newMapper}}
	\code{\link{newMap}}
}
\keyword{programming}
\keyword{environment}
