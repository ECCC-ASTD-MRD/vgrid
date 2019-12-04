/* libdescrip - Vertical grid descriptor library for FORTRAN programming
 * Copyright (C) 2016  Direction du developpement des previsions nationales
 *                     Centre meteorologique canadien
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vgrid.hpp"
#include "c_ut_report.h"
#include "armnlib.hpp"

extern "C" void c_use_new_read() {

  int ier, iun = 10, iun2 = 11;
  int quiet = 0, *i_val = NULL, in_log = 0, dpidpis = 0;
  int nl_t, nt, ni, nj, nk, ni2, nj2, nk2, k, key, ij, ijk, status;
  char filename[]="data/dm_5005_from_model_run";
  char mode[]="RND";
  float *f_val = NULL, *p0 = NULL;
  double *a_8_t = NULL, *b_8_t = NULL, *table = NULL, *levels_8 = NULL, *p0_8 = NULL;
  vgrid my_vgd;
  vgrid my_vgd2;

  status = VGD_OK;

  ier = c_fnom(&iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return;
  }
  ier = c_fstouv(iun,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return;
  }
  
  if( my_vgd.Cvgd_new_read(iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return;
  }
  if( my_vgd.Cvgd_get_int_1d("VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return;
  }
  if( my_vgd.Cvgd_get_float_1d("VCDT", &f_val, NULL , quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_float_1d for VCDT\n");
    return;
  }
  if( my_vgd.Cvgd_get_double_1d("CA_T", &a_8_t, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_1d for CA_T\n");
    return;
  }
  if( my_vgd.Cvgd_get_double_1d("CB_T", &b_8_t, &nt, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_1d for CB_T\n");
    return;
  }

  // Size of thermo may also be obtained by this:
  ier = my_vgd.Cvgd_get_int("NL_T", &nl_t, quiet);
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
  // obtained with fstlir, but why do it if vgd already contains it!)
  if ( my_vgd.Cvgd_get_double_3d("VTBL", &table, &ni, &nj, &nk, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_double_3d for VTBL\n");
    return;
  }
  
  // Constructing new vgd with this table
  if ( my_vgd2.Cvgd_new_from_table(table, ni, nj, nk) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_new_from_table for VTBL\n");
    return;
  }

  // Comparing new table with original table, must be the same.
  if( my_vgd.Cvgd_vgdcmp(&my_vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same\n");
    return;
  }

  // Write descriptor in new file  
  system("rm -f to_erase");
  ier = c_fnom(&iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return;
  }
  ier = c_fstouv(iun2,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return;
  }
  if( my_vgd.Cvgd_write_desc(iun2) == VGD_ERROR ){
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
  p0 = (float*)malloc(ni2*nj2 * sizeof(float));
  if(! p0){
    printf("Problem allocating P0 of size %d\n",ni2*nj2);
    return;
  }
  p0_8 = (double*)malloc(ni2*nj2 * sizeof(double));
  if(! p0_8){
    printf("Problem allocating P0_8 of size %d\n",ni2*nj2);
    return;
  }
  levels_8 = (double*)malloc(ni2*nj2*nl_t * sizeof(double));
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
  ier = my_vgd.Cvgd_diag_withref_8(ni2, nj2, nl_t, i_val, levels_8, p0_8, in_log, dpidpis);
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
      if( fabs(p0[ij] - levels_8[ijk]/100.)/p0[ij] > 1.e-6 ) {
	printf("Difference is too large, expected %f, got %f\n", p0[ij], levels_8[ijk]/100.);
	return;
      }
    }
  }

  ier = c_fstfrm(iun2);
  ier = c_fclos(iun2);

  // Re open, read and compare
  ier = c_fnom(&iun2,"to_erase",mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2\n");
    return;
  }
  ier = c_fstouv(iun2,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2\n");
    return;
  }
  if( my_vgd2.Cvgd_new_read(iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read vgd2\n");
    return;
  }
  if( my_vgd.Cvgd_vgdcmp(&my_vgd2) != 0 ){
    printf("ERROR, vgd and vgd2 shouldne the same after write in file, read from file\n");
    return;
  }

  free(table);
  free(i_val);
  free(f_val);
  free(a_8_t);
  free(b_8_t);

  ier = c_ut_report(status,"testing new_read");
  
}
