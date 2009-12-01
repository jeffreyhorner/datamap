\name{newMapper}
\Rdversion{1.1}
\alias{newMapper}
\title{Function for creating new mapper types }
\description{
	A mapper is a collection of functions that define how a type
	of foreign object is accessed. \code{newMapper} creates a new
	mapper object, assigns it a type, and places it in the global
	Mapper database.
}
\usage{
newMapper( type=NULL, .init=NULL, .get=NULL, .assign=NULL, .delete=NULL, 
           .finalize=NULL, ...)
}
\arguments{
	\item{type}{
		Character vector length one describing the unique type
		of foreign objects available through the new mapper.
	}
	\item{.init}{
		A function whose signature is \code{.init(map,...)} where
		map is a new map object and \dots are those from
		the \code{newMap} call. Returns TRUE for a successfull initialization
		of the map object or FALSE on error.
	}
	\item{.get}{
		A function whose signature is \code{.get(symbol)} where symbol is a
		character vector of length one defining the name of the
		foreign object to fetch. Returns the foreign object or
		\code{UnboundValue()}. NULL is also an acceptable value
		for \code{.get}.
	}
	\item{.assign}{
		A function whose signature is \code{.assign(symbol,val)}
		where symbol is a character string of length one defining
		the name of the foreign object to assign to and val is
		the value the foreign object will take. There are no
		requirements on the return value of \code{.assign}. NULL is
		also an acceptable value for \code{.assign}.
	}
	\item{.delete}{
		A function whose signature is \code{.delete(symbols)}
		where symbols is a non-zero length character vector. There
		are no requirements on \code{.delete}'s return value. NULL is
		also an acceptable value.
	}
	\item{.finalize}{
		A function whose signature is \code{'finalize()'}. There
		are no requirements on \code{.finalize}'s return
		value. NULL is also an acceptible value.
	}
	\item{\dots}{
		Additional objects that are also added to the mapper environment. They MUST be named 
		arguments, i.e. name=val.
	}
}
\value{
	Invisibly returns the new mapper, an object of class
	'dataMapper'. \code{newMapper} is called for its side effect of
	adding the new mapper to the global Mapper database.
}
\seealso{
	\code{\link{newMap}}
}
\examples{

# Complete example mapper
newMapper(
	type="EXAMPLE",
	.init=function(map,symbols=c'example',len=3){

		# Install symbols that the users passes in from newMap().
		lapply(symbols,install,map)

		# Now let's install a len symbol in our map
		# and also place a len variable in the map
		install('len',map)
		assign('len',len,internal(map))

		# Returning FALSE means failure
		return(TRUE)
	},
	.get = function(x) {
		cat("I'll just get",x,"for you"
	},
	.assign = function(x,val) dbWriteTable(con,x,val),
	.delete = function(x) dbRemoveTable(con,x),
	.finalize = function() dbDisconnect(con),
	foo <- 'bar'
)

}
\keyword{programming}
\keyword{environment}
