#include <stdio.h>
#include <stdlib.h>
#include "../libc/vgrid.h"

int c_use_new_read () {

  int ier, iun = 10, iun2 = 11;
  int *ip1 = NULL, *ip2 = NULL, *kind = NULL, *version = NULL, *quiet = NULL, *i_val = NULL;
  int nl_t, nt, ni, nj, nk, k;
  char filename[]="../tests/data/dm_5005_from_model_run";
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  float *f_val = NULL;
  double *a_8_t = NULL, *b_8_t = NULL, *table = NULL;
  TVGrid *vgd = NULL, *vgd2 = NULL;

  printf("c_use_new_read\n");

  ier = c_fnom(iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return 1;
  }
  ier = c_fstouv(iun,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return 1;
  }
  
  if( Cvgd_new_read(&vgd, iun, format, ip1, ip2, kind, version) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return 1;
  }
  if( Cvgd_get_int_1d(vgd, "VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return 1;
  }
  if( Cvgd_get_real_1d(vgd, "VCDT", &f_val, NULL , quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real_1d for VCDT\n");
    return 1;
  }
  if( Cvgd_get_real8_1d(vgd, "CA_T", &a_8_t, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real8_1d for CA_T\n");
    return 1;
  }
  if( Cvgd_get_real8_1d(vgd, "CB_T", &b_8_t, &nt, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real8_1d for CB_T\n");
    return 1;
  }

  // Size of thermo may also be obtained by this:
  ier = Cvgd_get_int(vgd, "NL_T", &nl_t, quiet);
  if(nl_t != nt ) {
    printf("ERROR: nt and nl_t should be equal, got %d, %d\n",nt, nl_t);
    return(-1);
  }
  printf("nl_t = %d\n", nl_t);
  
  for( k = 0; k < nl_t; k++) {
    printf("k = %d, ip1 = %d, val = %f, A = %f, B = %f\n",
  	   k, i_val[k], f_val[k], a_8_t[k], b_8_t[k]);
  }

  // Load table (this is the actual data in fst record !! which may also be
  // obtained with fstlir, but why do it id vgd already contains it!)
  if ( Cvgd_get_real8_3d(vgd, "VTBL", &table, &ni, &nj, &nk, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real8_3d for VTBL\n");
    return 1;
  }
  
  // Constructing new vgd with this table
  if ( Cvgd_new_from_table(&vgd2, table, ni, nj, nk) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_new_from_table for VTBL\n");
    return 1;
  }

  // Comparing new table with original table, must be the same.
  if( Cvgd_vgdcmp(vgd, vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same\n");
    return(1);
  }

  // Write descriptor in new file
  ier = c_fnom(iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return 1;
  }
  ier = c_fstouv(iun2,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return 1;
  }
  if( Cvgd_write_desc(vgd, iun2, "FST") == VGD_ERROR ){
    printf("ERROR with Cvgd_write_desc on iun2\n");
    return 1;
  }
  ier = c_fstfrm(iun2);
  ier = c_fclos(iun2);

  // Re open, read and compare
  ier = c_fnom(iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return 1;
  }
  ier = c_fstouv(iun2,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return 1;
  }
  if( Cvgd_new_read(&vgd2, iun, format, ip1, ip2, kind, version) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read vgd2\n");
    return 1;
  }
  if( Cvgd_vgdcmp(vgd, vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same after write in file, read from file\n");
    return(1);
  }

  Cvgd_vgd_free(&vgd);
  Cvgd_vgd_free(&vgd2);
  free(table);
  free(i_val);
  free(f_val);
  free(a_8_t);
  free(b_8_t);

  return(0);
}