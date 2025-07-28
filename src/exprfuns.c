#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/* PhG: code adapted from R 4.6.0 alpha src (/src/main/list.c)
 * because all.names does not handle functions only */
/* The following code is used to recursive traverse a block */
/* of code and extract all the symbols present in that code. */

typedef struct {
  SEXP ans;
  int UniqueNames;
  int IncludeFunctions;
  int StoreValues;
  int ItemCounts;
  int MaxCount;
} NameWalkData;

static void namewalk(SEXP s, NameWalkData *d, int isfun, SEXP ex)
{
  SEXP name;

  switch(TYPEOF(s)) {
  case SYMSXP:
    if(!isfun) goto ignore;
    name = PRINTNAME(s);
    /* skip blank symbols */
    if(CHAR(name)[0] == '\0') goto ignore;
    /* skip names in the exclude list */
    if(ex != R_NilValue) {
      for(int i = 0 ; i < LENGTH(ex) ; i++) {
        if(STRING_ELT(ex, i) == name)
          goto ignore;
      }
    }
    if(d->ItemCounts < d->MaxCount) {
      if(d->StoreValues) {
        if(d->UniqueNames) {
          for(int j = 0 ; j < d->ItemCounts ; j++) {
            if(STRING_ELT(d->ans, j) == name)
              goto ignore;
          }
        }
        SET_STRING_ELT(d->ans, d->ItemCounts, name);
      }
      d->ItemCounts++;
    }
    ignore:
      break;
  case LANGSXP:
    /*if(!d->IncludeFunctions) s = CDR(s);*/
    namewalk(CAR(s), d, 1, ex);
    s = CDR(s);
    while(s != R_NilValue) {
      namewalk(CAR(s), d, 0, ex);
      s = CDR(s);
    }
    break;
  case EXPRSXP:
   for(R_xlen_t i = 0 ; i < XLENGTH(s) ; i++)
      namewalk(VECTOR_ELT(s, i), d, 0, ex);
    break;
  default:
    /* it seems the intention is to do nothing here! */
    break;
  }
}

/* Use .Call(exprfuns, expr, max.names, unique, exclude.names) */
SEXP exprfuns(SEXP expr, SEXP maxnames, SEXP unique, SEXP excludenames)
{
  int i, savecount;
  NameWalkData data = {NULL, 0, 0, 0, 0, 0};

  data.MaxCount = Rf_asInteger(maxnames);
  if(data.MaxCount == -1) data.MaxCount = INT_MAX;
  if(data.MaxCount < 0 || data.MaxCount == NA_INTEGER)
    data.MaxCount = 0;

  data.UniqueNames = Rf_asLogical(unique);
  if(data.UniqueNames == NA_LOGICAL)
    data.UniqueNames = 1;

  namewalk(expr, &data, 0, excludenames);
  savecount = data.ItemCounts;

  data.ans = Rf_allocVector(STRSXP, data.ItemCounts);

  data.StoreValues = 1;
  data.ItemCounts = 0;
  namewalk(expr, &data, 0, excludenames);

  if(data.ItemCounts != savecount) {
    PROTECT(expr = data.ans);
    data.ans = Rf_allocVector(STRSXP, data.ItemCounts);
    for(i = 0 ; i < data.ItemCounts ; i++)
      SET_STRING_ELT(data.ans, i, STRING_ELT(expr, i));
    UNPROTECT(1);
  }

  return data.ans;
}
