#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>


SEXP tr(SEXP mat, SEXP umat, SEXP ind, SEXP ind2, 
  SEXP uind, SEXP N, SEXP COL, SEXP NL)
{
  int i, j, k, n, nl, col, one, two;
    
  n = INTEGER(N)[0];
  nl = INTEGER(NL)[0];
  col = INTEGER(COL)[0];
    
  int *iptr, *uiptr;
  iptr = INTEGER(ind);
  uiptr = INTEGER(uind);
    
  double *mptr, *umptr, *i2ptr, s;
  mptr = REAL(mat);
  umptr = REAL(umat);
  i2ptr = REAL(ind2);
        
  for(j = 0; j < nl; ++j) {
    s = 0;
    two = uiptr[j];
    for(i = 0; i < n; ++i) {
      one = iptr[i];
      if(one == two) {
        s += 1;
      }
    }
    s = 1/s;
    for(i = 0; i < n; ++i) {
      one = iptr[i];
      if(one == two)
        i2ptr[i] = s;
    }
  }
  for(i = 0; i < n; ++i)
    for(k = 0; k < col; ++k)
      mptr[k*n + i] = mptr[k*n + i] * i2ptr[i];
  for(j = 0; j < nl; ++j)
    for(i = 0; i < n; ++i) {
      one = iptr[i];
      two = uiptr[j];
      for(k = 0; k < col; ++k)
        if(one == two)
          umptr[k*nl + j] = umptr[k*nl + j] + mptr[k*n + i];
    }

  return umat;            
}
    

SEXP getuit(SEXP x, SEXP xu, SEXP n, SEXP m, SEXP check)
{
  int ii, jj, N, M, nProtected = 0, n_comps = 0, *iptr, ch;
  double one, two, *xptr, *xuptr;
  SEXP res, ind, names;

  N = INTEGER(n)[0];
  M = INTEGER(m)[0];
  ch = INTEGER(check)[0];

  PROTECT(ind = allocVector(INTSXP, N));
  ++nProtected;
  ++n_comps;
	
  xptr = REAL(x);
  xuptr = REAL(xu);
  iptr = INTEGER(ind);

  if(ch == 1) {
    for(jj = 0; jj < M; ++jj) {
      for(ii = 0; ii < N; ++ii) {
        one = xptr[ii];
        two = xuptr[jj];
        if(one == two) {
          iptr[ii] = jj + 1;
        }
      }
    }
  }
  if(ch == 2) {
    double one2,two2;
    for(jj = 0; jj < M; ++jj) {
      for(ii = 0; ii < N; ++ii) {
        one = xptr[ii];
        one2 = xptr[ii + N];
        two = xuptr[jj];
        two2 = xuptr[jj + M];
        if((one == two) && (one2 == two2)) {
          iptr[ii] = jj + 1;
        }
      }
    }
  }

  PROTECT(res = allocVector(VECSXP, n_comps));
  ++nProtected;
  SET_VECTOR_ELT(res, 0, ind);

  PROTECT(names = allocVector(STRSXP, n_comps));
  ++nProtected;
  SET_STRING_ELT(names, 0, mkChar("ind"));
  setAttrib(res, R_NamesSymbol, names);
        
  UNPROTECT(nProtected);

  return res;
}
	
    
SEXP cpos(SEXP p, SEXP K)
{
  int i, n, k;
  n = INTEGER(K)[0];
  k = n + 1;
    
  double tmp, *pptr, asum, xsum, ysum;
    
  pptr = REAL(p);
    
  asum = 0;
  xsum = 0;
  ysum = 0;
    
  for(i = 0; i < n; ++i) {
    tmp = pptr[i]*pptr[i + k + 1] - pptr[i + 1]*pptr[i + k];
    asum += tmp;
    xsum += (pptr[i] + pptr[i + 1])*tmp;
    ysum += (pptr[i + k] + pptr[i + k + 1])*tmp;
  }
        
  tmp = 1/(3*asum);

  SEXP pos;
  PROTECT(pos = allocVector(REALSXP, 2));

  REAL(pos)[0] = tmp*xsum;
  REAL(pos)[1] = tmp*ysum;

  UNPROTECT(1);
    
  return pos;
}
        

SEXP  getListElement(SEXP list, char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  int i;
     
  for(i=0;i<length(list);i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
            
  return elmt;
}
    

SEXP change(SEXP mat)
{
  int i, j, n, k, T, nProtected = 0;
  n = nrows(mat);
  k = ncols(mat);
    
  T = n*k + k;
    
  double *mptr,*optr;
    
  SEXP out;
  PROTECT(out = allocVector(REALSXP, T));
  ++nProtected;
    
  mptr = REAL(mat);
  optr = REAL(out);
    
  for(i = 0; i < n; ++i) {
    for(j = 0; j < k; ++j) {
      optr[j*n + i + j] = mptr[j*n + i];            
    }
  }
  for(j = 0; j < k; ++j) {
    optr[j*n + n + j] = mptr[j*n];
  }
    
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP, 2));
  ++nProtected;   

  INTEGER(dim)[0] = (n + 1); 
  INTEGER(dim)[1] = k;   
  setAttrib(out, R_DimSymbol, dim);
    
  UNPROTECT(nProtected);

  return out;
}

   
SEXP myNArem(SEXP mat)
{
  int i, j, ok, n, k, dsum, nProtected = 0, check;
  n = nrows(mat);
  k = ncols(mat);
    
  double *mptr,*nmptr;
  int *iptr;
    
  SEXP ind;
  PROTECT(ind = allocVector(INTSXP, n));
  ++nProtected;
    
  dsum = 0, ok = 0;
  iptr = INTEGER(ind);
  mptr = REAL(mat);
    
  for(i=0;i<n;++i) {
    check = 0;
    for(j=0;j<k;++j) {
      if(ISNA(mptr[j*n + i])) {
        check += 1;
      }
    }
    iptr[i] = check;
    if(iptr[i] > 0)
      dsum += 1;
  }

  int T = n - dsum;
    
  SEXP newmat;
  PROTECT(newmat = allocVector(REALSXP, T * k));
  ++nProtected;
    
  nmptr = REAL(newmat);
    
  for(i = 0; i < n; ++i) {
    if(iptr[i] < 1) {
      for(j = 0; j < k; ++j) {
        nmptr[j*T + ok] = mptr[j*n + i];
      }
      ok += 1;
    }
  } 
    
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP, 2));
  ++nProtected;   
    
  INTEGER(dim)[0] = T; 
  INTEGER(dim)[1] = k;   
  setAttrib(newmat, R_DimSymbol, dim);
        
  UNPROTECT(nProtected);

  return newmat;
}

    
SEXP cdist(SEXP map, SEXP N, SEXP b)
{
  int i, j, n, nProtected = 0, nn1, nn2;
  n = INTEGER(N)[0];
    
  double dx,dy;

  SEXP p1 = R_NilValue;
  PROTECT(p1);
  ++nProtected;
    
  SEXP p2 = R_NilValue;
  PROTECT(p2);
  ++nProtected;
    
  SEXP pos1;
  PROTECT(pos1 = allocVector(REALSXP, 2));
  ++nProtected;
    
  SEXP pos2;
  PROTECT(pos2 = allocVector(REALSXP, 2));
  ++nProtected;

  SEXP NP1;
  PROTECT(NP1 = allocVector(INTSXP, 1));
  ++nProtected;
    
  SEXP NP2;
  PROTECT(NP2 = allocVector(INTSXP, 1));
  ++nProtected;
    
  SEXP tmp1;
  PROTECT(tmp1 = allocVector(REALSXP, 2));
  ++nProtected;
  REAL(tmp1)[0] = 0;
  REAL(tmp1)[1] = 0;
    
  SEXP tmp2;
  PROTECT(tmp2 = allocVector(REALSXP, 2));
  ++nProtected;
  REAL(tmp2)[0] = 0;
  REAL(tmp2)[1] = 0;
    
  SEXP names;
  PROTECT(names = allocVector(STRSXP, 2));
  ++nProtected;
     
  SEXP out;   
  PROTECT(out = allocVector(VECSXP, 2));
  ++nProtected;
    
  SEXP ctr;   
  PROTECT(ctr = allocVector(REALSXP, n*2));
  ++nProtected;
    
  SEXP dimb;
  PROTECT(dimb = allocVector(INTSXP, 2));
  ++nProtected;
    
  SEXP dimctr;
  PROTECT(dimctr = allocVector(INTSXP, 2));
  ++nProtected;
    
  int *np1ptr, *np2ptr;
  double *ctrptr, *bptr;
    
  np1ptr = INTEGER(NP1);
  np2ptr = INTEGER(NP2);
  ctrptr = REAL(ctr);
  bptr = REAL(b);
    
  int ii, kk;
  double tmp, *pptr, asum, xsum, ysum;
    
  for(i = 0; i < n; ++i) {
    for(j=0;j<n;++j) {
      if((i != j) && (j > i)) {               
        SET_VECTOR_ELT(map, i, myNArem(VECTOR_ELT(map, i)));
        np1ptr[0] = nrows(VECTOR_ELT(map, i)) - 1;

        SET_VECTOR_ELT(map, j, myNArem(VECTOR_ELT(map, j)));
        np2ptr[0] = nrows(VECTOR_ELT(map, j)) - 1;
                    
        nn1 = np1ptr[0] + 1;
        nn2 = np2ptr[0] + 1;
                    
        if(REAL(VECTOR_ELT(map, i))[0] != REAL(VECTOR_ELT(map, i))[nn1 - 1] || REAL(VECTOR_ELT(map, i))[nn1] != REAL(VECTOR_ELT(map, i))[2*nn1 - 1]) {
          SET_VECTOR_ELT(map, i, change(VECTOR_ELT(map, i)));
          np1ptr[0] = nrows(VECTOR_ELT(map, i)) - 1;
        }
        if(REAL(VECTOR_ELT(map, j))[0] != REAL(VECTOR_ELT(map, j))[nn2 - 1] || REAL(VECTOR_ELT(map, j))[nn2] != REAL(VECTOR_ELT(map, j))[2*nn2 - 1]) {
          SET_VECTOR_ELT(map, j, change(VECTOR_ELT(map, j)));
          np2ptr[0] = nrows(VECTOR_ELT(map, j)) - 1;
        }
                
        //pos1 = cpos(p1,NP1,tmp1);
        //pos2 = cpos(p2,NP2,tmp2);
                
        asum = 0;
        xsum = 0;
        ysum = 0;
        kk = np1ptr[0] + 1;
                
        pptr = REAL(VECTOR_ELT(map, i));
    
        for(ii = 0; ii < np1ptr[0]; ++ii) {
          tmp = pptr[ii]*pptr[ii + kk + 1] - pptr[ii + 1]*pptr[ii + kk];
          asum += tmp;
          xsum += (pptr[ii] + pptr[ii + 1])*tmp;
          ysum += (pptr[ii + kk] + pptr[ii + kk + 1])*tmp;
        }
        
        tmp = 1/(3*asum);
        REAL(pos1)[0] = tmp*xsum;
        REAL(pos1)[1] = tmp*ysum;
                
        asum = 0;
        xsum = 0;
        ysum = 0;
        kk = np2ptr[0] + 1;
               
        pptr = REAL(VECTOR_ELT(map, j));
  
        for(ii = 0; ii < np2ptr[0]; ++ii) {
          tmp = pptr[ii]*pptr[ii + kk + 1] - pptr[ii + 1]*pptr[ii + kk];
          asum += tmp;
          xsum += (pptr[ii] + pptr[ii + 1])*tmp;
          ysum += (pptr[ii + kk] + pptr[ii + kk + 1])*tmp;
        }
        
        tmp = 1/(3*asum);
        REAL(pos2)[0] = tmp*xsum;
        REAL(pos2)[1] = tmp*ysum;
                
        ctrptr[i] = REAL(pos1)[0];
        ctrptr[n + i] = REAL(pos1)[1];
                
        if(j == (n - 1)) {
          ctrptr[j] = REAL(pos2)[0];
          ctrptr[n + j] = REAL(pos2)[1];
        }
                
        dx = REAL(pos1)[0] - REAL(pos2)[0];
        dy = REAL(pos1)[1] - REAL(pos2)[1];
               
        bptr[j*n + i] = hypot(dx,dy);
        bptr[i*n + j] = bptr[j*n + i];
      }
    }
  }
     
  INTEGER(dimb)[0] = n; 
  INTEGER(dimb)[1] = n;   
  setAttrib(b, R_DimSymbol, dimb);
    
  INTEGER(dimctr)[0] = n;   
  INTEGER(dimctr)[1] = 2;   
  setAttrib(ctr, R_DimSymbol, dimctr);
        
  SET_VECTOR_ELT(out, 0, b);
  SET_VECTOR_ELT(out, 1, ctr);
    
  SET_STRING_ELT(names, 0, mkChar("distance"));
  SET_STRING_ELT(names, 1, mkChar("centroids"));
    
  setAttrib(out, R_NamesSymbol, names);    
        
  UNPROTECT(nProtected);

  return out;
}


SEXP cpoint(SEXP poly1, SEXP poly2)
{
  int i, j, np1, np2;
    
  np1 = nrows(poly1) - 1;
  np2 = nrows(poly2) - 1;
    
  double *pptr1, *pptr2;
  pptr1 = REAL(poly1);
  pptr2 = REAL(poly2);
    
  SEXP out;
  PROTECT(out = allocVector(INTSXP, 1));
  INTEGER(out)[0] = 0;
    
  for(i = 0; i < np1; ++i)
    for(j = 0; j < np2; ++j)
      if(pptr1[i] == pptr2[j])
        if(pptr1[i + np1] == pptr2[j + np2]) {
          INTEGER(out)[0] = 1;
          break;
        }
     
  UNPROTECT(1);

  return out;
}


SEXP unique_id(SEXP x, SEXP xu)
{
  int i, j, nx, nux;
  nx = length(x);
  nux = length(xu);

  SEXP out;
  PROTECT(out = allocVector(INTSXP, nx));

  double *xptr, *xuptr;
  int *outptr;
  xptr = REAL(x);
  xuptr = REAL(xu);
  outptr = INTEGER(out);    
    
  for(i = 0; i < nx; ++i)
    for(j = 0; j < nux; ++j)
      if(xptr[i] == xuptr[j]) {
        outptr[i] = j + 1;
        break;
      }
     
  UNPROTECT(1);

  return out;
}    
