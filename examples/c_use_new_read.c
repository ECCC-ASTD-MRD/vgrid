#include <stdio.h>
#include <stdlib.h>
#include "../libc/vgrid.h"

int c_use_new_read () {

  int ier, iun = 10;
  char  filename[]="../tests/data/dm_5005_from_model_run";
  char mode[]="RND";
  TVGrid *vgd = NULL;
  int *ip1 = NULL, *ip2 = NULL, *kind = NULL, *version = NULL, *quiet = NULL;
  char  format[] = "FST";
  char name[5];
  int *i_val = NULL, nl_t, *i_ptr, k;
  float *f_val = NULL;
  double *a_8_t = NULL, *b_8_t = NULL;

  printf("c_use_new_read\n");

  ier = c_fnom(iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom\n");
    return 1;
  }
  ier = c_fstouv(iun,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv\n");
    return 1;
  }
  
  if( Cvgd_new_read(&vgd, iun, format, ip1, ip2, kind, version) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read\n");
    return 1;
  }
  if( Cvgd_get_int_1d(vgd, "VIPT", &i_val, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return 1;
  }
  if( Cvgd_get_real_1d(vgd, "VCDT", &f_val, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real_1d for VCDT\n");
    return 1;
  }
  if( Cvgd_get_real8_1d(vgd, "CA_T", &a_8_t, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real8_1d for CA_T\n");
    return 1;
  }
  if( Cvgd_get_real8_1d(vgd, "CB_T", &b_8_t, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_real8_1d for CB_T\n");
    return 1;
  }

  i_ptr = &nl_t;
  ier = Cvgd_get_int(vgd, "NL_T", &i_ptr, quiet);
  printf("nl_t = %d\n", nl_t);

  
  for( k = 0; k < nl_t; k++) {
    printf("k = %d, ip1 = %d, val = %f, A = %f, B = %f\n",
  	   k, i_val[k], f_val[k], a_8_t[k], b_8_t[k]);
  }

  Cvgd_vgd_free(&vgd);
  free(i_val);
  free(f_val);
  free(a_8_t);
  free(b_8_t);

  return(0);
}
