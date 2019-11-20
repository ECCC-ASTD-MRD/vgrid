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

extern "C" void c_levels() {

  int ier, iun = 10;
  int quiet = NULL, *i_val = NULL, in_log = NULL, *dpidpis = NULL;
  int nl_t, ni, nj, nk, ni2, nj2, nk2, k, key, ij, ijk, status;
  char filename[]="data/dm_5001_from_model_run";
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  float *p0 = NULL, *levels = NULL;
  double *p0_8 = NULL, *levels_8 = NULL;
  vgrid_descriptor vgd, *vgd_p;
  vgrid_descriptor my_vgd;
  vgrid my_vgrid(&my_vgd);

  vgd_p = &vgd;
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
  
  if( my_vgrid.Cvgd_new_read(vgd_p, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return;
  }

  if( my_vgrid.Cvgd_get_int_1d(vgd_p, "VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return;
  }

  ier = my_vgrid.Cvgd_get_int(vgd_p, "NL_T", &nl_t, quiet);
  if(ier == VGD_ERROR){
    status = VGD_ERROR;
  }

  // Compute 3D pressure levels
  // First get P0 (the reference field)
  key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", -1, -1, -1, " ", "P0");
  if(key < 0){
    printf("Problem getting info for P0\n");
    return;
  }
  p0 = (float*)malloc(ni2*nj2 * sizeof(float));
  if(! p0){
    printf("Problem allocating p0 of size %d\n",ni2*nj2);
    return;
  }
  p0_8 = (double*)malloc(ni2*nj2 * sizeof(double));
  if(! p0_8){
    printf("Problem allocating p0_8 of size %d\n",ni2*nj2);
    return;
  }
  levels = (float*)malloc(ni2*nj2*nl_t * sizeof(float));
  if(! levels){
    printf("Problem allocating levels of size %d\n",ni2*nj2);
    return;
  }
  levels_8 = (double*)malloc(ni2*nj2*nl_t * sizeof(double));
  if(! levels_8){
    printf("Problem allocating levels_8 of size %d\n",ni2*nj2);
    return;
  }
  ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );
  if(ier < 0){
    printf("Problem with fstluk for p0\n");
    return;
  }
  for( ij = 0; ij < ni2*nj2; ij++, ijk++){
    p0[ij] = p0[ij]*100.;
    p0_8[ij] = p0[ij];
  }

  ier = my_vgrid.Cvgd_get_int(vgd_p, "NL_T", &nl_t, quiet);
  if(ier == VGD_ERROR){
    status = VGD_ERROR;
  }
  printf("DANS TEST nl_t = %d\n", nl_t);

  ier = my_vgrid.Cvgd_levels(vgd_p, ni2, nj2, nl_t, i_val, levels, p0, in_log);
  if(ier == VGD_ERROR){
    printf("Error with Cvgd_diag_withref\n");
    status = VGD_ERROR;
  }
  ier = my_vgrid.Cvgd_levels_8(vgd_p, ni2, nj2, nl_t, i_val, levels_8, p0_8, in_log);
  if(ier == VGD_ERROR){
    printf("Error with Cvgd_diag_withref_8\n");
    status = VGD_ERROR;
   }
  // Load PX to see if pressure computation is OK
  printf("nl_t=%d\n",nl_t);
  for( k = 0, ijk = 0; k < nl_t; k++){
    key = c_fstinf( iun, &ni2, &nj2, &nk2, -1, " ", i_val[k], -1, -1, " ", "PX");
    if(key < 0){
      printf("Problem getting info for PX for ip1 = %d\n",i_val[k]);
      return;
    }
    // To simplify, PX are assumed to be on the same grid as P0. but this should be check in an operational program!
    ier = c_fstluk( p0, key, &ni2, &nj2, &nk2 );
    if( ier < 0 ){
      printf("Error with c_fstluk on level = %d\n",i_val[k]);
      return;
    }
    for( ij = 0; ij < ni2*nj2; ij++, ijk++){
      if( fabs(p0[ij] - levels[ijk]/100.)/p0[ij] > 1.e-6 ) {
	printf("Difference is too large (float), expected %f, got %f\n", p0[ij], levels[ijk]/100.);
	return;
      }
      if( fabs(p0[ij] - levels_8[ijk]/100.)/p0[ij] > 1.e-6 ) {
	printf("Difference is too large (double), expected %f, got %f\n", p0_8[ij], levels_8[ijk]/100.);
	return;
      }
    }
  }
  ier = c_fstfrm(iun);
  ier = c_fclos(iun);

  free(p0);
  free(p0_8);
  free(levels);
  free(levels_8);
  free(i_val);
  
  ier = c_ut_report(status,"testing Cvgd_levels");

}
