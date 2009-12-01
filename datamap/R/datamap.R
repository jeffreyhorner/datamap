Mappers <- new.env(hash=TRUE)
mappers <- function(type){
	if ( !missing(type) ){
		return(Mappers[[type]])
	}
	type <- character()
	.init <- .get <- .assign <- .delete <- .finalize <- logical()
	for(mapper in objects(Mappers,all.names=TRUE)){
		type <- append(type,mapper)
		.init <- append(.init,exists('.init',envir=Mappers[[mapper]],inherits=FALSE))
		.get <- append(.get,exists('.get',envir=Mappers[[mapper]],inherits=FALSE))
		.assign <- append(.assign,exists('.assign',envir=Mappers[[mapper]],inherits=FALSE))
		.delete <- append(.delete,exists('.delete',envir=Mappers[[mapper]],inherits=FALSE))
		.finalize <- append(.finalize,exists('.finalize',envir=Mappers[[mapper]],inherits=FALSE))
	}
	data.frame(type=type,.init=.init,.get=.init,.assign=.assign, .delete=.delete,.finalize=.finalize)
}
newMapper <- function( type=NULL, .init=NULL, .get=NULL, .assign=NULL, .delete=NULL, .finalize=NULL, ...){

	if (is.null(.init))
		stop("Need an init function at the least.")

	mapper <- new.env(hash=TRUE)

	assign('.type',type,mapper);

	environment(.init) <- mapper; 
	assign('.init',.init,mapper)

	if (!is.null(.get)){
		environment(.get) <- mapper
		assign('.get',.get,mapper)
	}
	if (!is.null(.assign)){
		environment(.assign) <- mapper
		assign('.assign',.assign,mapper)
	}
	if (!is.null(.delete)){
		environment(.delete) <- mapper
		assign('.delete',.finalize,mapper)
	}
	if (!is.null(.finalize)){
		environment(.finalize) <- mapper
		assign('.finalize',.finalize,mapper)
	}

	elems <- list(...)
	if (length(elems)>0){
		lapply(
			names(elems),
			function(sym) {
				if (sym==''){
					warning('Arguments in ... must be named')
					return(NULL)
				}
				obj <- elems[[sym]]
				if (typeof(obj) == "closure")
					environment(obj) <- mapper
				assign(sym,obj,mapper)
			}
		)
	}

	class(mapper) <- c("dataMapper",class(mapper))
	assign(type,mapper,Mappers)
	invisible(mapper)
}

if(.Platform$OS.type != "windows") {

	# A simple example of a read-only mapper for the UNIX command 'top'.
	newMapper(
		type="UNIX:top",
		.init= function(map){
			# Nothing realy to do but tell datamap
			# the name of our objects
			install('top',map)
			return(TRUE)
		},
		.get = function(x){
			con <- textConnection(system("top -b -n 1", intern=TRUE)[-1:-6])
			on.exit(close(con))
			read.table(con,header=TRUE)
		}
	)
}

newMapper(
	type="DBI:database",
	.init= function(map,...){

		# Various initialization code
		library(RMySQL)
		con <- dbConnect(MySQL(),...)

		# Assign persistent state into our map
		assign('con',con,map)

		# Install symbols within our map. Note that 'install' is
		# different than assigning state. Installed symbols become
		# the objects visible to R
		install(dbListTables(con),map)

		# Returning FALSE means something has failed
		return(TRUE)
	},
	.get = function(x) {
		if (dbExistsTable(con,x))
			return(dbReadTable(con,x))
		else
			return(UnboundValue())
	},
	.assign = function(x,val) dbWriteTable(con,x,val),
	.delete = function(x) dbRemoveTable(con,x),
	.finalize = function() dbDisconnect(con)
)

# Make R CMD check shut up
.type <- NULL; .get <- function(x) NULL; .assign <- function(x,y) NULL
binderGetOnly <- function(val,sym){
	if (!missing(val))
		warning(paste("Assignments to",sym,"are not possible with maps of type",.type,"."))
	return(.get(sym))
}

binderGetAssign <- function(val,sym){
	if (missing(val))
		return(.get(sym))
	else
		return(.assign(sym,val))
}

binderAssignOnly <- function(val,sym){
	if (missing(val))
		warning(paste("Cannot read variable",sym,"with maps of type",.type,"."))
	else
		return(.assign(sym,val))
}


newMap <- function(type=character(),...) {

	# Look up mapper type
	if (!exists(type,Mappers))
		stop(paste("Type",type,"not found!"))

	mapper <- get(type,Mappers)

	# The datamap object, an outward (map) facing environment
	# with a inward facing (intern) environment.
	intern <- new.env(hash=TRUE,globalenv())
	class(intern) <- c("datamapIntern",class(intern))
	map <- new.env(hash=TRUE,globalenv())
	class(map) <- c("datamap",class(map))
	assign('.intern',intern,map)
	assign('.map',map,intern)

	# Copy mapper into new map, fixing up environment for functions
	lapply(
		objects(mapper,all.names=TRUE),
		function(x) {
			obj <- get(x,mapper)
			if (typeof(obj) == "closure")
				environment(obj) <- intern
			assign(x,obj,intern)
		}
	)

	# Call init function with environment
	# Init returns TRUE on success
	# Any other return type results in an error
	init <- get('.init',mapper)
	ret <- try(init(intern,...))
	if (!is.logical(ret) || ret[1]!=TRUE)
		stop("init() must return TRUE to instantiate a datamap object.")

	reg.finalizer(map,mapFinalize)
	return(map)
}

reconstitute <- function(map){
	# Switch to external environment of the datamap
	if (inherits(map,"datamapIntern"))
		map <- get('.map',map)

	if (!inherits(map,"datamap"))
		stop("object not a datamap")

	map
}

mapFinalize <- function(map){
	map <- reconstitute(map)
	intern <- get('.intern',map)
	if (exists('.finalize',intern))
		get('.finalize',intern)()
	invisible(NULL)
}

install <- function(symbols,map){
	map <- reconstitute(map)
	intern <- get('.intern',map)

	if (exists('.assign',intern)){
		if (exists('.get',intern))
			binder <- binderGetAssign
		else
			binder <- binderAssignOnly
	} else if (exists('.get',intern)){
		binder <- binderGetOnly
	} else {
		stop("No get or assign methods for this map")
	}

	installed <- unlist(lapply(symbols,
		function(name){
			bind <- binder
			environment(bind) <- intern
			formals(bind)$sym=name
			makeActiveBinding(name,bind,map)
			name
		}
	))
	invisible(installed)
}

uninstall <- function(symbols,map){
	map <- reconstitute(map)
	rm(list=symbols,envir=map)
}

delete <- function(symbols,map,UNINSTALL=TRUE) {
	mapCall(map,'.delete',symbols)
	if (UNINSTALL)
		uninstall(symbols,map)
}

mapAttach <- function(map,pos=2,name=NULL,warn.conflicts=TRUE){
	# Create a UserDefinedDatabase object and attach to search path
	map <- reconstitute(map)
	if (missing(name))
		name <- paste('datamap',get('.type',get('.intern',map)),sep=':')
	attach(.Call('CreateUserDB',map,PACKAGE='datamap'),pos,name,warn.conflicts)
}

mapCall <- function(map,funName,...){
	map <- reconstitute(map)
	intern <- get('.intern',map)
	if (exists(funName,intern)){
		return(get(funName,intern)(...))
	} else {
		stop(paste(funName,"not in map!"))
	}
}

internal(map) get('.internal',map)

UnboundValue <- function() .Call('UnboundValue',PACKAGE='datamap')
