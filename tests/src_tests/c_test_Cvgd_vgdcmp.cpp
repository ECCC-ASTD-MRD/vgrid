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
#include <string.h>
#include "vgrid.hpp"
#include "c_ut_report.h"
#include "armnlib.hpp"

extern "C" void c_test_Cvgd_vgdcmp() {

  int ier, iun = 10, iun2 = 11, status;
  char filename[]="data/dm_5005_from_model_run";
  char filename2[]="data/dm_5001_from_model_run";
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  vgrid_descriptor vgd, vgd2;
  vgrid_descriptor vgd3, vgd4;
  vgrid my_vgrid;

  vgrid_descriptor *vgd_p, *vgd2_p, *vgd3_p, *vgd4_p;

  vgd_p  = &vgd;
  vgd2_p = &vgd2;
  vgd3_p = &vgd3;
  vgd4_p = &vgd4;
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
  if( my_vgrid.Cvgd_new_read(vgd2_p, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return;
  }

  ier = c_fnom(&iun2,filename2,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun2, file %s\n", filename2);
    return;
  }
  ier = c_fstouv(iun2,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun2, file %s\n", filename2);
    return;
  }
  if( my_vgrid.Cvgd_new_read(vgd3_p, iun2, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun2\n");
    return;
  }
  if( my_vgrid.Cvgd_new_read(vgd4_p, iun2, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun2\n");
    return;
  }

  // Comparing all
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) != 0 ){
    printf("ERROR, vgd and vgd2 should be the same\n");
    return;
  }

  vgd2_p->vcode=0;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to vcode\n");
    status = VGD_ERROR;
  }
  vgd2_p->vcode=vgd_p->vcode;
  
  vgd2_p->kind=0;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to kind\n");
    status = VGD_ERROR;
  }
  vgd2_p->kind=vgd_p->kind;

  vgd2_p->version=0;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to version\n");
    status = VGD_ERROR;
  }
  vgd2_p->version=vgd_p->version;
 
  strncpy(vgd2_p->ref_name,"NULL",4);
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ref_name\n");
    status = VGD_ERROR;
  }
  strncpy(vgd2_p->ref_name,vgd_p->ref_name,4);

  vgd4_p->ptop_8=0.;
  if( my_vgrid.Cvgd_vgdcmp(vgd3_p, vgd4_p) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to ptop_8\n");
    status = VGD_ERROR;
  }
  vgd4_p->ptop_8=vgd3_p->ptop_8;

  vgd4_p->pref_8=0.;
  if( my_vgrid.Cvgd_vgdcmp(vgd3_p, vgd4_p) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to pref_8\n");
    status = VGD_ERROR;
  }
  vgd4_p->pref_8=vgd3_p->pref_8;

  vgd4_p->rcoef1=0.;
  if( my_vgrid.Cvgd_vgdcmp(vgd3_p, vgd4_p) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to rcoef1\n");
    status = VGD_ERROR;
  }
  vgd4_p->rcoef1=vgd3_p->rcoef1;

  vgd4_p->rcoef2=0.;
  if( my_vgrid.Cvgd_vgdcmp(vgd3_p, vgd4_p) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to rcoef2\n");
    status = VGD_ERROR;
  }
  vgd4_p->rcoef2=vgd3_p->rcoef2;

  vgd2_p->ip1_m[vgd2_p->nl_m-1]=vgd_p->ip1_m[vgd_p->nl_m-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ip1_m\n");
    status = VGD_ERROR;
  }
  vgd2_p->ip1_m[vgd2_p->nl_m-1]=vgd_p->ip1_m[vgd2_p->nl_m-1];
  
  vgd2_p->ip1_t[vgd2_p->nl_t-1]=vgd_p->ip1_t[vgd_p->nl_t-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ip1_t\n");
    status = VGD_ERROR;
  }
  vgd2_p->ip1_t[vgd2_p->nl_t-1]=vgd_p->ip1_t[vgd2_p->nl_t-1];
  
  vgd2_p->a_m_8[vgd2_p->nl_m-1]=vgd_p->a_m_8[vgd_p->nl_m-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to a_m_8\n");
    status = VGD_ERROR;
  }
  vgd2_p->a_m_8[vgd2_p->nl_m-1]=vgd_p->a_m_8[vgd2_p->nl_m-1];

  vgd2_p->b_m_8[vgd2_p->nl_m-1]=vgd_p->b_m_8[vgd_p->nl_m-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to b_m_8\n");
    status = VGD_ERROR;
  }
  vgd2_p->b_m_8[vgd2_p->nl_m-1]=vgd_p->b_m_8[vgd2_p->nl_m-1];

  vgd2_p->a_t_8[vgd2_p->nl_t-1]=vgd_p->a_t_8[vgd_p->nl_t-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to a_t_8\n");
    status = VGD_ERROR;
  }
  vgd2_p->a_t_8[vgd2_p->nl_t-1]=vgd_p->a_t_8[vgd2_p->nl_t-1];

  vgd2_p->b_t_8[vgd2_p->nl_t-1]=vgd_p->b_t_8[vgd_p->nl_t-1]+1.;
  if( my_vgrid.Cvgd_vgdcmp(vgd_p, vgd2_p) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to b_t_8\n");
    status = VGD_ERROR;
  }
  vgd2_p->b_t_8[vgd2_p->nl_t-1]=vgd_p->b_t_8[vgd2_p->nl_t-1];

  ier = c_ut_report(status,"testing Cvgd_levels");
  
}
