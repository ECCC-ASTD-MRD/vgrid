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
#include "vgrid.h"
#include "armnlib.h"

void c_new_from_table() {

  int ni, nj, nk, iun=10, ier;
  double *table = NULL;
  char filename[]="data/dm_5005_from_model_run";
  vgrid_descriptor *vgd = NULL,  *vgd2 = NULL;

  if( c_fnom(&iun,filename,"RND+R/O",0) < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return;
  }
  if( c_fstouv(iun,"RND","") < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return;
  }  
  if( Cvgd_new_read(&vgd, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return;
  }
  // Get table  
  if( Cvgd_get_double_3d(vgd, "VTBL", &table, &ni, &nj, &nk, 0) == VGD_ERROR ){
    printf("ERROR with Cvgd_get_double_3d on VTBL\n");
    return;
  }
  if( Cvgd_new_from_table(&vgd2, table, ni, nj, nk) == VGD_ERROR ){
    printf("ERROR with Cvgd_new_from_table\n");
    return;
  }
  // Test equality
  ier = Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return;
  } else {
    printf("     Descritors are equal.\n");
  }
  Cvgd_free(&vgd);
  Cvgd_free(&vgd2);
  free(table);
  ier = c_ut_report(VGD_OK,"testing new_build");  
}
