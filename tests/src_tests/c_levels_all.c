#include <stdio.h>
#include <stdlib.h>
#include "vgrid.h"

char *filenames[] = {
    "data/dm_1001_from_model_run",
    "data/dm_1002_from_model_run",
    "data/dm_5001_from_model_run",
    "data/dm_5002_from_model_run",
    "data/dm_5005_from_model_run",
};

#define n_file (sizeof (filenames) / sizeof (const char *))

int test_it(char *filename) {

  int ier, iun = 10;
  int *ip1 = NULL, *ip2 = NULL, *kind = NULL, *version = NULL, *quiet = NULL, *i_val = NULL, *in_log = NULL, *dpidpis = NULL;
  int nl_t, ni, nj, nk, ni2, nj2, nk2, k, key, ij, ijk;
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  float *f_val = NULL, *p0 = NULL, *px = NULL, *levels = NULL;
  double *p0_8 = NULL, *levels_8 = NULL;
  vgrid_descriptor *vgd = NULL;
      
  ier = c_fnom(iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return(VGD_ERROR);
  }
  ier = c_fstouv(iun,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return(VGD_ERROR);
  }

  if( Cvgd_new_read(&vgd, iun, ip1, ip2, kind, version) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(VGD_ERROR);
  }
  //ier = Cvgd_print_desc(vgd, NULL, NULL);

  if( Cvgd_get_int_1d(vgd, "VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return(VGD_ERROR);
  }

  ier = Cvgd_get_int(vgd, "NL_T", &nl_t, quiet);
  if(ier == VGD_ERROR){
    return(VGD_ERROR);
  }
      
  // Compute 3D pressure levels
  // First get P0 (the reference field)
  key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", -1, -1, -1, " ", "P0");
  if(key < 0){
    printf("Problem getting info for P0\n");
    return(VGD_ERROR);
  }
  p0 = malloc(ni2*nj2 * sizeof(float));
  if(! p0){
    printf("Problem allocating p0 of size %d\n",ni2*nj2);
    return(VGD_ERROR);
  }
  p0_8 = malloc(ni2*nj2 * sizeof(double));
  if(! p0_8){
    printf("Problem allocating p0_8 of size %d\n",ni2*nj2);
    return(VGD_ERROR);
  }
  levels = malloc(ni2*nj2*nl_t * sizeof(float));
  if(! levels){
    printf("Problem allocating levels of size %d\n",ni2*nj2);
    return(VGD_ERROR);
  }
  levels_8 = malloc(ni2*nj2*nl_t * sizeof(double));
  if(! levels_8){
    printf("Problem allocating levels_8 of size %d\n",ni2*nj2);
    return(VGD_ERROR);
  }
  ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );
  if(ier < 0){
    printf("Problem with fstluk for p0\n");
    return(VGD_ERROR);
  }
  for( ij = 0; ij < ni2*nj2; ij++, ijk++){
    p0[ij] = p0[ij]*100.;
    p0_8[ij] = p0[ij];
  }
  printf("   testing float interface\n");
  ier = Cvgd_levels(vgd, ni2, nj2, nl_t, i_val, levels, p0, in_log);
  if(ier == VGD_ERROR){
    printf("Error with Cvgd_diag_withref\n");
    return(VGD_ERROR);
  }
  printf("   testing double interface\n");
  ier = Cvgd_levels_8(vgd, ni2, nj2, nl_t, i_val, levels_8, p0_8, in_log);
  if(ier == VGD_ERROR){
    printf("Error with Cvgd_diag_withref_8\n");
    return(VGD_ERROR);
  }
  // Load PX to see if pressure computation is OK
  printf("nl_t=%d\n",nl_t);
  for( k = 0, ijk = 0; k < nl_t; k++){
    key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", i_val[k], -1, -1, " ", "PX");
    if(key < 0){
      printf("Problem getting info for PX for ip1 = %d\n",i_val[k]);
      return(VGD_ERROR);
    }
    // To simplify, PX are assumed to be on the same grid as P0. bur this should be check in an operational program!
    ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );
    if( ier < 0 ){
      printf("Error with c_fstluk on level = %d\n",i_val[k]);
      return(VGD_ERROR);
    }
    for( ij = 0; ij < ni2*nj2; ij++, ijk++){
      if( abs(p0[ij] - levels[ijk]/100.) > 1.e-6 ) {
	printf("Difference is too large (float), expected %f, got %f\n", p0[ij], levels[ijk]/100.);
	return(VGD_ERROR);
      }
      if( abs(p0[ij] - levels_8[ijk]/100.) > 1.e-6 ) {
	printf("Difference is too large (double), expected %f, got %f\n", p0_8[ij], levels_8[ijk]/100.);
	return(VGD_ERROR);
      }
    }
  }
  ier = c_fstfrm(iun);
  ier = c_fclos(iun);
      
  Cvgd_vgd_free(&vgd);
  free(p0);
  free(levels);
  free(p0_8);
  free(levels_8);
  free(i_val);
  
  return(VGD_OK);
  
}

//========================================================================
//========================================================================

void c_levels_all() {
  
  int i, ier, status = VGD_OK;
  for (i = 0; i < n_file; i++) {
    printf ("Testing %s\n", filenames[i]);
    if(test_it(filenames[i]) == VGD_ERROR)
      status = VGD_ERROR;
  }  
  ier = c_ut_report(status,"testing Cvgd_levels");  
  
}
