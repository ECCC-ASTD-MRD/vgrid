#if !defined(__ARMNLIB_H__)
#define __ARMNLIB_H__

#include <rpnmacros.h>

extern "C" int c_fnom(int *iun,char *filename,char *options,int i);
extern "C" int c_fstouv(int iun,char *filename,char *options);
extern "C" void c_fstvoi(int iun,char *options);
extern "C" int c_fstrwd(int iun);
extern "C" int c_fstinf(int iun,int *ni, int *nj,int *nk,int datev,char *etiket,
    int ip1,int ip2,int ip3,char *typvar,char *nomvar);
extern "C" int c_fstsui(int iun,int *ni, int *nj,int *nk);
extern "C" int c_fstinfx(int inhandle,int iun,int *ni, int *nj,int *nk,int datev,char *etiket,
    int ip1,int ip2,int ip3,char *typvar,char *nomvar);
extern "C" int c_fstprm(int handle,int *dateo,int *deet,int *npas,int *ni,int *nj,int *nk,
    int *nbits,int *datyp,int *ip1,int *ip2,int *ip3,char *TYPVAR,char *NOMVAR,
    char *ETIKET,char *GRTYP,int *ig1,int *ig2,int *ig3,int *ig4,
    int *swa,int *lng,int *dltf,int *ubc,int *extra1,int *extra2,int *extra3);
extern "C" int c_fstinl(int iun,int *ni, int *nj,int *nk,int datev,char *etiket,
    int ip1,int ip2,int ip3,char *typvar,char *nomvar,int *liste,int *nliste,int nmax);
extern "C" int c_fstluk(void *data,int handle,int *ni, int *nj,int *nk);
extern "C" int c_fst_edit_dir(int handle,int date,int deet,int npas,int ni,int nj,int nk,
    int ip1,int ip2,int ip3,char *typvar,char *nomvar,char *etiket,char *grtyp,
    int ig1,int ig2,int ig3,int ig4,int datyp);
extern "C" int c_fstecr(void *data,void *work,int nbits,int iun,int dateo,int deet,int npas,
    int ni,int nj,int nk,int ip1,int ip2,int ip3,char *typvar,char *nomvar,
    char *etiket,char *grtyp,int ig1,int ig2,int ig3,int ig4,int datypdtl,int rewrit);
extern "C" int c_fsteff(int handle);
extern "C" int c_fstfrm(int iun);
extern "C" int c_fclos(int iun);
extern "C" void f77name(cxgaig)(char *grtyp,
    F77_INTEGER *ig1,F77_INTEGER *ig2,F77_INTEGER *ig3,F77_INTEGER *ig4,
    F77_REAL *xg1,F77_REAL *xg2,F77_REAL *xg3,F77_REAL *xg4);
extern "C" void f77name(cigaxg)(char *grtyp,
    F77_REAL *xg1,F77_REAL *xg2,F77_REAL *xg3,F77_REAL *xg4,
    F77_INTEGER *ig1,F77_INTEGER *ig2,F77_INTEGER *ig3,F77_INTEGER *ig4);
extern "C" void f77name(convip_plus)(F77_INTEGER *ipnew,F77_REAL *level,F77_INTEGER *fkind,
    F77_INTEGER *fmode,char *strg,F77_INTEGER *flag);
extern "C" int f77name(newdate)(F77_INTEGER *fdat1,F77_INTEGER *fdat2,
    F77_INTEGER *fdat3,F77_INTEGER *fmode);
extern "C" int f77name(difdatr)(F77_INTEGER *fdat1,F77_INTEGER *fdat2,F77_REAL8 *fnhours);
extern "C" int f77name(incdatr)(F77_INTEGER *fdat1,F77_INTEGER *fdat2,F77_REAL8 *fnhours);
extern "C" int c_gdll(int gdid, float *lat, float *lon);
extern "C" int c_gdxyfll(int gdid,float *x,float *y,float *lat,float *lon,int n);
extern "C" int c_gdllfxy(int gdid,float *lat,float *lon,float *x,float *y,int n);
extern "C" int c_ezdefset(int gdid_dst,int gdid_src);
extern "C" int c_ezgdef_fmem(int ni,int nj,char *grtypZ,char *grref,
    int ig1,int ig2,int ig3,int ig4,float *xs,float *ys);
extern "C" int c_ezqkdef(int ni,int nj,char *grtyp,int ig1,int ig2,int ig3,int ig4,int x);
extern "C" int c_ezuvint(void *newarray,void *newarray2,void *arrayin,void *arrayin2);
extern "C" int c_ezsint(void *newarray,void *arrayin);
// Add get/setopt and val and ival
extern "C" int c_ezgetopt(char * option, char * value);
extern "C" int c_ezsetopt(char * option, char * value);
extern "C" int c_ezgetval(char * option, float * value);
extern "C" int c_ezsetval(char * option, float * value);
extern "C" int c_ezgetival(char * option, int * value);
extern "C" int c_ezsetival(char * option, int value); // This appears to pass a literal integer
extern "C" int c_fstopi(char *optname,int lvl,int setget);
// Add grid release function
extern "C" int c_gdrls(int gdid);

// Add wind conversion routines
extern "C" int c_gdwdfuv(int gdid, float * spdllout, float * dirllout, float * uugdin,
               float * vvgdin, float * lat, float * lon, int npts);
extern "C" int c_gduvfwd(int gdid, float * uugdout, float * vvgdout, float * spdllin,
               float * dirllin, float * lat, float * lon, int npts);

// Scattered point interpolation

// (lat,lon) scalar interpolation
extern "C" int c_gdllsval(int gdid, float * zvals, float * zin, float * lat, float * lon, int n);
// (x,y) scalar interpolation
extern "C" int c_gdxysval(int gdid, float * zvals, float * zin, float * x, float * y, int n);
// (lat,lon) vector interpolation
extern "C" int c_gdllvval(int gdid, float * uuvals, float * vvvals, float * uuin, float * vvin,
               float * lat, float * lon, int n);
// (x,y) vector interpolation
extern "C" int c_gdxyvval(int gdid, float * uuvals, float * vvvals, float * uuin, float * vvin,
               float * x, float * y, int n);

#endif
