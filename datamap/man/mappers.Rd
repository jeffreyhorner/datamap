\name{mappers}
\Rdversion{1.1}
\alias{mappers}
\title{Function for inspecting the global list of Mappers}
\description{
	Get information about the mappers in the global list of mappers.
}
\usage{
mappers( type )
}
\arguments{
	\item{type}{
		Character vector length one describing the unique type
		of foreign objects available through the new mapper.
	}
}
\value{
	If type is not missing, then it returns the environment which describes
	the mapper. Otherwise it returns a data frame summarizing the the presence of
	functions that define the mapper.
}
\seealso{
	\code{\link{newMapper}}
	\code{\link{newMap}}
}
\keyword{programming}
\keyword{environment}
