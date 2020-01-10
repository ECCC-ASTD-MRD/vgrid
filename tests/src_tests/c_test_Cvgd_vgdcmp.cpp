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

// N.B.:  CLASS VGRID VARIABLES MUST BE DECLARED PUBLIC, FOR THIS TEST TO WORK
// N.B.:  CLASS VGRID VARIABLES MUST BE DECLARED PUBLIC, FOR THIS TEST TO WORK
// N.B.:  CLASS VGRID VARIABLES MUST BE DECLARED PUBLIC, FOR THIS TEST TO WORK
//            The test should be improved so as not to require this.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vgrid.hpp"
#include "vgrid_creators.hpp"
#include "c_ut_report.h"
#include "armnlib.hpp"

extern "C" void c_test_Cvgd_vgdcmp() {

  int ier, iun = 10, iun2 = 11, status;
  char filename[]="data/dm_5005_from_model_run";
  char filename2[]="data/dm_5001_from_model_run";
  char mode[]="RND";
  char format[] = "FST";
  char name[5];
  vgrid *my_vgrid;
  vgrid *my_vgrid2;
  vgrid *my_vgrid3;
  vgrid *my_vgrid4;

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
  if( Cvgd_read_vgrid_from_file(&my_vgrid, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_read_vgrid_from_file on iun\n");
    return;
  }
  if( Cvgd_read_vgrid_from_file(&my_vgrid2, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_read_vgrid_from_file on iun\n");
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
  if( Cvgd_read_vgrid_from_file(&my_vgrid3, iun2, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_read_vgrid_from_file on iun2\n");
    return;
  }
  if( Cvgd_read_vgrid_from_file(&my_vgrid4, iun2, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_read_vgrid_from_file on iun2\n");
    return;
  }

  // Comparing all
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) != 0 ){
    printf("ERROR, vgd and vgd2 should be the same\n");
    return;
  }

  my_vgrid->vcode=0;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to vcode\n");
    status = VGD_ERROR;
  }
  my_vgrid2->vcode=my_vgrid->vcode;
  
  my_vgrid2->kind=0;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to kind\n");
    status = VGD_ERROR;
  }
  my_vgrid2->kind=my_vgrid->kind;

  my_vgrid2->version=0;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to version\n");
    status = VGD_ERROR;
  }
  my_vgrid2->version=my_vgrid->version;
 
  strncpy(my_vgrid2->ref_name,"NULL",4);
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ref_name\n");
    status = VGD_ERROR;
  }
  strncpy(my_vgrid2->ref_name,my_vgrid->ref_name,4);

  my_vgrid4->ptop_8=0.;
  if( my_vgrid3->Cvgd_vgdcmp(my_vgrid4) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to ptop_8\n");
    status = VGD_ERROR;
  }
  my_vgrid4->ptop_8=my_vgrid3->ptop_8;

  my_vgrid4->pref_8=0.;
  if( my_vgrid3->Cvgd_vgdcmp(my_vgrid4) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to pref_8\n");
    status = VGD_ERROR;
  }
  my_vgrid4->pref_8=my_vgrid3->pref_8;

  my_vgrid4->rcoef1=0.;
  if( my_vgrid3->Cvgd_vgdcmp(my_vgrid4) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to rcoef1\n");
    status = VGD_ERROR;
  }
  my_vgrid4->rcoef1=my_vgrid3->rcoef1;

  my_vgrid4->rcoef2=0.;
  if( my_vgrid3->Cvgd_vgdcmp(my_vgrid4) == 0 ){
    printf("ERROR, vgd3 and vgd4 should not be the same due to rcoef2\n");
    status = VGD_ERROR;
  }
  my_vgrid4->rcoef2=my_vgrid3->rcoef2;

  my_vgrid2->ip1_m[my_vgrid2->nl_m-1]=my_vgrid->ip1_m[my_vgrid->nl_m-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ip1_m\n");
    status = VGD_ERROR;
  }
  my_vgrid2->ip1_m[my_vgrid2->nl_m-1]=my_vgrid->ip1_m[my_vgrid2->nl_m-1];
  
  my_vgrid2->ip1_t[my_vgrid2->nl_t-1]=my_vgrid->ip1_t[my_vgrid->nl_t-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to ip1_t\n");
    status = VGD_ERROR;
  }
  my_vgrid2->ip1_t[my_vgrid2->nl_t-1]=my_vgrid->ip1_t[my_vgrid2->nl_t-1];
  
  my_vgrid2->a_m_8[my_vgrid2->nl_m-1]=my_vgrid->a_m_8[my_vgrid->nl_m-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to a_m_8\n");
    status = VGD_ERROR;
  }
  my_vgrid2->a_m_8[my_vgrid2->nl_m-1]=my_vgrid->a_m_8[my_vgrid2->nl_m-1];

  my_vgrid2->b_m_8[my_vgrid2->nl_m-1]=my_vgrid->b_m_8[my_vgrid->nl_m-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to b_m_8\n");
    status = VGD_ERROR;
  }
  my_vgrid2->b_m_8[my_vgrid2->nl_m-1]=my_vgrid->b_m_8[my_vgrid2->nl_m-1];

  my_vgrid2->a_t_8[my_vgrid2->nl_t-1]=my_vgrid->a_t_8[my_vgrid->nl_t-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to a_t_8\n");
    status = VGD_ERROR;
  }
  my_vgrid2->a_t_8[my_vgrid2->nl_t-1]=my_vgrid->a_t_8[my_vgrid2->nl_t-1];

  my_vgrid2->b_t_8[my_vgrid2->nl_t-1]=my_vgrid->b_t_8[my_vgrid->nl_t-1]+1.;
  if( my_vgrid->Cvgd_vgdcmp(my_vgrid2) == 0 ){
    printf("ERROR, vgd and vgd2 should not be the same due to b_t_8\n");
    status = VGD_ERROR;
  }
  my_vgrid2->b_t_8[my_vgrid2->nl_t-1]=my_vgrid->b_t_8[my_vgrid2->nl_t-1];

  ier = c_ut_report(status,"testing Cvgd_vgdcmp");
  
}
