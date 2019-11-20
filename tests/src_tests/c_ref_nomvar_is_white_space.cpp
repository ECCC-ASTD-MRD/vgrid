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
#include "vgrid.hpp"
#include "c_ut_report.h"
#include "armnlib.hpp"

extern "C" int c_ref_nomvar_is_white_space() {

  int iun = 10;
  char mode[]="RND+R/O", ok=VGD_OK;
  char filename[]="data/dm_2001_from_editfst";
  char value[]="VGD_NO_REF_NOMVAR";
  vgrid_descriptor vgd, *vgd_p;
  vgrid_descriptor my_vgd;
  vgrid my_vgrid(&my_vgd);

  vgd_p = &vgd;

  if( c_fnom(&iun,filename,mode,0) < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return(1);
  }
  if( c_fstouv(iun,mode,"") < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return(1);
  }
  if( my_vgrid.Cvgd_new_read(vgd_p, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(1);
  }

  if( my_vgrid.Cvgd_get_char(vgd_p, "RFLD", value, 1) == VGD_OK ){
    printf("RFLD='%s'\n",value);
    printf("In test, problem with vgd_get on 'RFLD', should have returned an error but returned VGD_OK\n");
    ok = VGD_ERROR;
  }

  if( my_vgrid.Cvgd_get_char(vgd_p, "RFLS", value, 1) == VGD_OK ){
    printf("RFLS='%s'\n",value);
    printf("In test, problem with vgd_get on 'RFLS', should have returned an error but returned VGD_OK\n");
    ok = VGD_ERROR;
  }

  return c_ut_report(ok,"For Vcode without ref fields nomvar must be '    '");
  
}
