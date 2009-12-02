#include "UserDB.h"

/* based on code from  src/envir.c in R distribution */
static int FrameSize(SEXP frame) {
	int count = 0;
	while (frame != R_NilValue) {
		if (CHAR(PRINTNAME(TAG(frame)))[0] != '.' && CAR(frame) != R_UnboundValue)
			count += 1;
		frame = CDR(frame);
	}
	return count;
}

static void FrameNames(SEXP frame, SEXP names, int *indx) {
	while (frame != R_NilValue) {
		if (CHAR(PRINTNAME(TAG(frame)))[0] != '.' && CAR(frame) != R_UnboundValue) {
			SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
			(*indx)++;
		}
		frame = CDR(frame);
	}
}

static int HashTableSize(SEXP table) {
	int count = 0;
	int n = length(table);
	int i;
	for (i = 0; i < n; i++)
		count += FrameSize(VECTOR_ELT(table, i));
	return count;
}

static void HashTableNames(SEXP table, SEXP names, int *indx) {
	int n = length(table);
	int i;
	for (i = 0; i < n; i++)
		FrameNames(VECTOR_ELT(table, i), names, indx);
}

static Rboolean existsInFrame(const char * const sym, SEXP frame){
	while (frame != R_NilValue) {
		if (!strcmp(sym,CHAR(PRINTNAME(TAG(frame)))))
			return TRUE;
		frame = CDR(frame);
	}
	return FALSE;
}

static Rboolean existsInHash(const char * const sym, SEXP table){
	int n = length(table);
	int i;
	for (i = 0; i < n; i++){
		if (existsInFrame(sym,VECTOR_ELT(table, i)) == TRUE) return TRUE;
	}
	return FALSE;
}

static Rboolean symExists(const char * const sym, SEXP map){
	if (HASHTAB(map) != R_NilValue) 
		return existsInHash(sym,HASHTAB(map));
	return existsInFrame(sym,FRAME(map));
}

static SEXP udb_callWithName(const char * const funName, const char * const name, SEXP intern){
	SEXP e, fun, val;
	Rboolean old;
	int errorOccurred = FALSE;

	/* fun(name) */
	PROTECT(fun = findVarInFrame3(intern,install(funName),TRUE));

	if (fun == R_UnboundValue || TYPEOF(fun) != CLOSXP){
		UNPROTECT(1);
		warning("%s not found in map.",funName);
		return R_NilValue;
	}
	PROTECT(e = allocVector(LANGSXP,2));
	SETCAR(e, fun);
	SETCAR(CDR(e), val = NEW_CHARACTER(1));
	SET_STRING_ELT(val, 0, COPY_TO_USER_STRING(name));

	val = R_tryEval(e, NULL, &errorOccurred);
	if(errorOccurred){
	   	val = R_NilValue;
	}

	UNPROTECT(2);
	return(val);
}

static Rboolean udb_exists(const char * const symstr, Rboolean *canCache, R_ObjectTable *udb){
	SEXP map = (SEXP)udb->privateData;

	if(udb->active == FALSE) return FALSE;

	if (canCache) *canCache = FALSE; /* never let R cache values */
	return symExists(symstr,map);
}

static SEXP udb_get(const char * const name, Rboolean *canCache, R_ObjectTable *udb){
	SEXP e, fun, val, intern, map = (SEXP)udb->privateData;
	Rboolean old;
	int errorOccurred = FALSE;

	if(udb->active == FALSE) return R_UnboundValue;
	if (symExists(name,map) == FALSE) return R_UnboundValue;
	

	old = udb->active;
	udb->active = FALSE;

	PROTECT(intern = findVarInFrame3(map,install(".intern"),TRUE));
	if (intern == R_UnboundValue || TYPEOF(intern) != ENVSXP){
		UNPROTECT(1);
		warning("invalid map during get()");
		return R_NilValue;
	}
	/* fun(name) */
	PROTECT(fun = findVarInFrame3(intern,install("get"),TRUE));

	if (fun == R_UnboundValue || TYPEOF(fun) != CLOSXP){
		UNPROTECT(2);
		warning("%s not found in map.","get");
		return R_NilValue;
	}
	PROTECT(e = allocVector(LANGSXP,2));
	SETCAR(e, fun);
	SETCAR(CDR(e), val = NEW_CHARACTER(1));
	SET_STRING_ELT(val, 0, COPY_TO_USER_STRING(name));

	val = R_tryEval(e, NULL, &errorOccurred);
	if(errorOccurred){
	   	val = R_NilValue;
	}

	UNPROTECT(3);

	if (canCache) *canCache = FALSE; /* never let R cache these values */
	udb->active = old;

	return val;
}

static SEXP udb_assign(const char * const name, SEXP val, R_ObjectTable *udb){
	SEXP map = (SEXP)udb->privateData;
	SEXP e, fun, nval, retval, intern;
	Rboolean old;
	int errorOccurred = FALSE;

	/* Not sure if this is correct behavior */
	if(udb->active == FALSE) return R_UnboundValue;
	if (symExists(name,map) == FALSE) return R_UnboundValue;

	old = udb->active;
	udb->active = FALSE;

	PROTECT(intern = findVarInFrame3(map,install(".intern"),TRUE));
	if (intern == R_UnboundValue || TYPEOF(intern) != ENVSXP){
		UNPROTECT(1);
		warning("invalid map during remove()");
		return R_NilValue;
	}

	/* fun(name,val) */
	PROTECT(fun = findVarInFrame3(intern,install("assign"),TRUE));
	if (fun == R_UnboundValue || TYPEOF(fun) != CLOSXP){
		UNPROTECT(1);
		warning("assign not found in map.");
		return R_NilValue;
	}
	PROTECT(e = allocVector(LANGSXP,3));
	SETCAR(e, fun);
	SETCAR(CDR(e), nval = NEW_CHARACTER(1));
	SET_STRING_ELT(nval, 0, COPY_TO_USER_STRING(name));
	SETCAR(CDR(CDR(e)), val);

	retval = R_tryEval(e, NULL, &errorOccurred);
	if(errorOccurred){
	   	val = R_NilValue;
	}

	udb->active = old;

	UNPROTECT(3);
	return(retval);
}

static SEXP udb_objects(R_ObjectTable *udb){
	SEXP val, map = (SEXP)udb->privateData;
	int k;
	Rboolean old;

	if (udb->active == FALSE) return R_NilValue;

	old = udb->active;
	udb->active = FALSE;

	/* Step 1 : Compute the Vector Size */
	k = 0;
	if (HASHTAB(map) != R_NilValue)
		k += HashTableSize(HASHTAB(map));
	else
		k += FrameSize(FRAME(map));

	/* Step 2 : Allocate and Fill the Result */
	PROTECT(val = allocVector(STRSXP, k));
	k = 0;
	if (HASHTAB(map) != R_NilValue)
		HashTableNames(HASHTAB(map), val, &k);
	else
		FrameNames(FRAME(map), val, &k);

    UNPROTECT(1);

	udb->active = old;

	return(val);
}

void FinalizeUserDB(SEXP map){
	R_ObjectTable *udb = (R_ObjectTable *)R_ExternalPtrAddr(map);

	/* Rprintf("%s","Finalizing UserDB\n"); */

	if (udb){		
		R_ReleaseObject((SEXP)udb->privateData);
		free(udb);
	}

	/* Otherwise it's already finalized? */

}

SEXP CreateUserDB(SEXP map){
	R_ObjectTable *udb;
	SEXP klass, val;

	udb = (R_ObjectTable *) calloc(1,sizeof(R_ObjectTable));
	udb->active = TRUE;
	R_PreserveObject(map);
	udb->privateData = (void *)map;

	udb->exists = udb_exists;
	udb->get = udb_get;
	udb->assign = udb_assign;
	udb->objects = udb_objects;

	PROTECT(val = R_MakeExternalPtr(udb, Rf_install("UserDefinedDatabase"), R_NilValue));
	PROTECT(klass = NEW_CHARACTER(1));
	SET_STRING_ELT(klass, 0, COPY_TO_USER_STRING("UserDefinedDatabase"));
	SET_CLASS(val, klass);
	R_RegisterCFinalizer(val,FinalizeUserDB);
	UNPROTECT(2);

	return val;
}

static R_CallMethodDef callMethods[]  = {
	{"CreateUserDB", (DL_FUNC)&CreateUserDB, 1},
	{NULL, NULL, 0}
};

static void R_init_datamap(DllInfo *info){
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

static void R_unload_datamap(DllInfo *info){
}
