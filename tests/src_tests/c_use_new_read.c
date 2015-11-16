#include <stdio.h>
#include <stdlib.h>
#include "vgrid.h"

void c_use_new_read() {

  int ier, iun = 10, iun2 = 11;
  int *quiet = NULL, *i_val = NULL, *in_log = NULL, *dpidpis = NULL;
  int nl_t, nt, ni, nj, nk, ni2, nj2, nk2, k, key, ij, ijk, status;
  char filename[]="/users/dor/afsg/apm/ords/cmdn/vgrid/tests/data/dm_5005_from_model_run";
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  float *f_val = NULL, *p0 = NULL, *px = NULL;
  double *a_8_t = NULL, *b_8_t = NULL, *table = NULL, *levels_8 = NULL, *p0_8 = NULL;
  vgrid_descriptor *vgd = NULL, *vgd2 = NULL;

  status = VGD_OK;

  ier = c_fnom(iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return;
  }
  ier = c_fstouv(iun,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return;
  }
  
  if( Cvgd_new_read(&vgd, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return;
  }
  if( Cvgd_get_int_1d(vgd, "VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return;
  }
  if( Cvgd_get_float_1d(vgd, "VCDT", &f_val, NULL , quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_float_1d for VCDT\n");
    return;
  }
  if( Cvgd_get_double_1d(vgd, "CA_T", &a_8_t, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_1d for CA_T\n");
    return;
  }
  if( Cvgd_get_double_1d(vgd, "CB_T", &b_8_t, &nt, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_1d for CB_T\n");
    return;
  }

  // Size of thermo may also be obtained by this:
  ier = Cvgd_get_int(vgd, "NL_T", &nl_t, quiet);
  if(nl_t != nt ) {
    printf("ERROR: nt and nl_t should be equal, got %d, %d\n",nt, nl_t);
    return;
  }
  printf("nl_t = %d\n", nl_t);
  
  for( k = 0; k < nl_t; k++) {
    printf("k = %d, ip1 = %d, val = %f, A = %f, B = %f\n",
  	   k, i_val[k], f_val[k], a_8_t[k], b_8_t[k]);
  }

  // Load table (this is the actual data in fst record !! which may also be
  // obtained with fstlir, but why do it id vgd already contains it!)
  if ( Cvgd_get_double_3d(vgd, "VTBL", &table, &ni, &nj, &nk, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_3d for VTBL\n");
    return;
  }
  
  // Constructing new vgd with this table
  if ( Cvgd_new_from_table(&vgd2, table, ni, nj, nk) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_new_from_table for VTBL\n");
    return;
  }

  // Comparing new table with original table, must be the same.
  if( Cvgd_vgdcmp(vgd, vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same\n");
    return;
  }

  // Write descriptor in new file
  ier = c_fnom(iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return;
  }
  ier = c_fstouv(iun2,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return;
  }
  if( Cvgd_write_desc(vgd, iun2, "FST") == VGD_ERROR ){
    printf("ERROR with Cvgd_write_desc on iun2\n");
    return;
  }

  // Compute 3D pressure levels
  // First get P0 (the reference field
  key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", -1, -1, -1, " ", "P0");
  if(key < 0){
    printf("Problem getting info for P0\n");
    return;
  }
  p0 = malloc(ni2*nj2 * sizeof(float));
  if(! p0){
    printf("Problem allocating P0 of size %d\n",ni2*nj2);
    return;
  }
  p0_8 = malloc(ni2*nj2 * sizeof(double));
  if(! p0_8){
    printf("Problem allocating P0_8 of size %d\n",ni2*nj2);
    return;
  }
  levels_8 = malloc(ni2*nj2*nl_t * sizeof(double));
  if(! levels_8){
    printf("Problem allocating levels_8 of size %d\n",ni2*nj2);
    return;
  }
  ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );
  if( ier < 0 ){
    printf("Problem with fstluk on p0\n");
  }
  for( k = 0; k < ni2*nj2; k++){
    p0_8[k] = p0[k]*100.;
  }
  ier = Cvgd_diag_withref_8(vgd, ni2, nj2, nl_t, i_val, levels_8, p0_8, in_log, dpidpis);
  if(ier == VGD_ERROR){
    status=VGD_ERROR;
  }

  // Load PX to see if pressure computation is OK
  for( k = 0, ijk = 0; k < nl_t; k++){
    key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", i_val[k], -1, -1, " ", "PX");
    if(key < 0){
      printf("Problem getting info for PX for ip1 = %d\n",i_val[k]);
      return;
    }
    // To simplify, PX are assumed to be on the same grid as P0. bur rhis should be check in an operational program!
    ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );    
    if( ier < 0 ){
      printf("Problem with fstluk on px ip1 = %d\n", i_val[k]);
      return;
    }
    for( ij = 0; ij < ni2*nj2; ij++, ijk++){
      if( abs(p0[ij] - levels_8[ijk]/100.) > 1.e-6 ) {
	printf("Difference is too large, expected %f, got %f\n", p0[ij], levels_8[ijk]/100.);
	return;
      }
    }
  }

  ier = c_fstfrm(iun2);
  ier = c_fclos(iun2);

  // Re open, read and compare
  ier = c_fnom(iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return;
  }
  ier = c_fstouv(iun2,"RND");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return;
  }
  if( Cvgd_new_read(&vgd2, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read vgd2\n");
    return;
  }
  if( Cvgd_vgdcmp(vgd, vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same after write in file, read from file\n");
    return;
  }

  Cvgd_free(&vgd);
  Cvgd_free(&vgd2);
  free(table);
  free(i_val);
  free(f_val);
  free(a_8_t);
  free(b_8_t);

  ier = c_ut_report(status,"testing Cvgd_levels");
  
}
