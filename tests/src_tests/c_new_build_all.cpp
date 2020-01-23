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
#include <math.h>
#include "vgrid.hpp"
#include "vgrid_creators.hpp"
#include "c_ut_report.h"
#include "armnlib.hpp"

char *filenames[] = {
    "data/dm_1001_from_model_run",
    "data/dm_1002_from_model_run",
    "data/2001_from_model_run",
    "data/dm_4001_from_model_run",
    "data/dm_5001_from_model_run",
    "data/dm_5002_from_model_run",
    "data/dm_5005_from_model_run",
    "data/dm_5100_from_model_run",
    "data/dm_5999_from_model_run",
    "data/dm_21001_from_model_run_SLEVE",
    "data/dm_21001_from_model_run_NON_SLEVE",
    "data/dm_21002_from_model_run_SLEVE",
    "data/dm_21002_from_model_run_NON_SLEVE"
};

#define n_file (sizeof (filenames) / sizeof (const char *))

//========================================================================
int check_build_1001_2001_5999_4001(vgrid *my_vgrid, int vcode){
  int kind, version, nk, ier;
  int *ip1_m = NULL;
  double *a_m_8 = NULL, *b_m_8 = NULL;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  printf("  Testing generic interface\n");
  if( Cvgd_new_build_vert(&my_vgrid2, kind, version, nk, -1, -1, NULL, NULL, NULL, NULL,
  			  a_m_8, b_m_8, NULL, NULL, ip1_m, NULL, nk, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  printf("  Testing specific interface\n");
  free(my_vgrid2);
  my_vgrid2 = nullptr;
  switch(vcode) {
  case 1001:
    if( Create_from_ab_1001(&my_vgrid2, -1, -1, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 2001:
    if( Create_from_ab_2001(&my_vgrid2, -1, -1, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 4001:
    if( Create_from_ab_4001(&my_vgrid2, -1, -1, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 5999:
    if( Create_from_ab_5999(&my_vgrid2, -1, -1, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  default:
    printf("In test ERROR in check_build_1001_2001_5999_4001, unsupported Vcode %d\n",vcode);
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(a_m_8);
  
  free(b_m_8);
  free(my_vgrid2);

  return(VGD_OK);
}
//========================================================================
int check_build_1002(vgrid *my_vgrid){
  int kind, version, nk, ier;
  int *ip1_m = NULL;
  double *a_m_8 = NULL, *b_m_8 = NULL, ptop_8;
  vgrid *my_vgrid2;
  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_build_vert(&my_vgrid2, kind, version, nk, -1, -1, &ptop_8, NULL, NULL, NULL,
  			  a_m_8, b_m_8, NULL, NULL, ip1_m, NULL, nk, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal (generic itf), Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  free(my_vgrid2);
  my_vgrid2 = nullptr;
  if( Create_from_ab_1002(&my_vgrid2, -1, -1, ptop_8, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal (specific itf), Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(a_m_8);
  free(b_m_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int check_build_5001(vgrid *my_vgrid){
  int kind, version, nk, ier;
  int *ip1_m = NULL;
  float rc_1;
  double *a_m_8 = NULL, *b_m_8 = NULL, ptop_8, pref_8;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_build_vert(&my_vgrid2, kind, version, nk, -1, -1, &ptop_8, &pref_8, &rc_1, NULL,
  			  a_m_8, b_m_8, NULL, NULL, ip1_m, NULL, nk, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  printf("  Testing specific interface\n");
  free(my_vgrid2);
  my_vgrid2 = nullptr;
  if( Create_from_ab_5001(&my_vgrid2, -1, -1, ptop_8, pref_8, rc_1, a_m_8, b_m_8, ip1_m, nk) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(a_m_8);
  free(b_m_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int check_build_5002(vgrid *my_vgrid){
  int kind, version, nl_m, nl_t, ier;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2;
  double *a_m_8 = NULL, *b_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL, ptop_8, pref_8;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nl_m, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nl_m, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl_m, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_T", &a_t_8, &nl_t, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_T", &b_t_8, &nl_t, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPT", &ip1_t, &nl_t, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_build_vert(&my_vgrid2, kind, version, 0, -1, -1, &ptop_8, &pref_8, &rc_1, &rc_2,
  			  a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m, nl_t) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  printf("  Testing specific interface\n");
  free(my_vgrid2);
  my_vgrid2 = nullptr;
  if( Create_from_ab_5002(&my_vgrid2, -1, -1, ptop_8, pref_8, rc_1, rc_2,
			  a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m, nl_t)
                         == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(ip1_m);
  free(ip1_t);
  free(a_m_8);
  free(b_m_8);
  free(a_t_8);
  free(b_t_8);
  free(my_vgrid2);
  return(VGD_OK);
}
//========================================================================
int check_build_5005(vgrid *my_vgrid){
  int kind, version, nl, ier;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2;
  double *a_m_8 = NULL, *b_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL, pref_8;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_T", &a_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_T", &b_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPT", &ip1_t, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_build_vert(&my_vgrid2, kind, version, -1, -1, -1, NULL, &pref_8, &rc_1, &rc_2,
  			  a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl, nl) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  printf("  Testing specific interface\n");
  free(my_vgrid2);
  my_vgrid2 = nullptr;
  if( Create_from_ab_5005(&my_vgrid2, -1, -1, pref_8, rc_1, rc_2,
			  a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl) == VGD_ERROR)
  {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(ip1_t);
  free(a_m_8);
  free(b_m_8);
  free(a_t_8);
  free(b_t_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int check_build_5100(vgrid *my_vgrid){
  int kind, version, nl, ier;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2, rc_3, rc_4;
  double *a_m_8 = NULL, *b_m_8 = NULL, *c_m_8 = NULL, *a_t_8 = NULL,
    *b_t_8 = NULL, *c_t_8 = NULL, pref_8;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_M", &c_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_T", &a_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_T", &b_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_T", &c_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPT", &ip1_t, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_3", &rc_3, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_4", &rc_4, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  // No generic interface for 5100
  printf("  Testing specific interface\n");
  if( Create_from_ab_5100(&my_vgrid2, -1, -1, pref_8, rc_1, rc_2, rc_3, rc_4,
			  a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, ip1_m, ip1_t, nl
			 ) == VGD_ERROR)
  {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(ip1_m);
  free(ip1_t);
  free(a_m_8);
  free(b_m_8);
  free(c_m_8);
  free(a_t_8);
  free(b_t_8);
  free(c_t_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int check_build_21001(vgrid *my_vgrid){
  int kind, version, nl, ier;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2, rc_3, rc_4;
  double *a_m_8 = NULL, *b_m_8 = NULL, *c_m_8 = NULL, *a_t_8 = NULL,
    *b_t_8 = NULL, *c_t_8 = NULL, pref_8;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_M", &c_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_T", &a_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_T", &b_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_T", &c_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPT", &ip1_t, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_3", &rc_3, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_4", &rc_4, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  // No generic interface for 21001
  printf("  Testing specific interface\n");
  if( Create_from_ab_21001(&my_vgrid2, -1, -1, rc_1, rc_2, rc_3, rc_4,
  			   a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, ip1_m, ip1_t, nl) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(ip1_m);
  free(ip1_t);
  free(a_m_8);
  free(b_m_8);
  free(c_m_8);
  free(a_t_8);
  free(b_t_8);
  free(c_t_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int check_build_21002(vgrid *my_vgrid){
  int kind, version, nl, ier;
  int *ip1_m = NULL, *ip1_t = NULL, *ip1_w = NULL;
  float rc_1, rc_2, rc_3, rc_4;
  double
    *a_m_8 = NULL, *b_m_8 = NULL, *c_m_8 = NULL,
    *a_t_8 = NULL, *b_t_8 = NULL, *c_t_8 = NULL,
    *a_w_8 = NULL, *b_w_8 = NULL, *c_w_8 = NULL;
  vgrid *my_vgrid2;

  my_vgrid2 = nullptr;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_M", &a_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_M", &b_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_M", &c_m_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_T", &a_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_T", &b_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_T", &c_t_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPT", &ip1_t, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CA_W", &a_w_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CB_W", &b_w_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double_1d("CC_W", &c_w_8, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPW", &ip1_w, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_3", &rc_3, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("RC_4", &rc_4, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  
  // No generic interface for 21001
  printf("  Testing specific interface\n");
  if( Create_from_ab_21002(&my_vgrid2, -1, -1, rc_1, rc_2, rc_3, rc_4,
			   a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, a_w_8, b_w_8, c_w_8,
		           ip1_m, ip1_t, ip1_w, nl) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid->Cvgd_vgdcmp(my_vgrid2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(ip1_m);
  free(ip1_t);
  free(ip1_w);
  free(a_m_8);
  free(b_m_8);
  free(c_m_8);
  free(a_t_8);
  free(b_t_8);
  free(c_t_8);
  free(a_w_8);
  free(b_w_8);
  free(c_w_8);
  free(my_vgrid2);
  return(VGD_OK);
}

//========================================================================
int test_it(char *filename, int ind) {
  int ier, iun, vcode;
  iun = 10 + ind;
  char mode[]="RND";
  vgrid *my_vgrid;

  ier = c_fnom(&iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return(VGD_ERROR);
  }
  ier = c_fstouv(iun,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return(VGD_ERROR);
  }
  if( Cvgd_read_vgrid_from_file(&my_vgrid, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VCOD", &vcode, 0) == VGD_ERROR ){
    printf("ERROR with  Cvgd_get_int on VCOD\n");
    return(VGD_ERROR);
  }
  switch(vcode) {
  case 1001:
  case 2001:
  case 4001:
  case 5999:
    if( check_build_1001_2001_5999_4001(my_vgrid, vcode) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 1002:
    if( check_build_1002(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5001:
    if( check_build_5001(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5002:
    if( check_build_5002(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5005:
    if( check_build_5005(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5100:
    if( check_build_5100(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 21001:
    if( check_build_21001(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 21002:
    if( check_build_21002(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  default:
    printf("In test ERROR unsupported Vcode %d\n",vcode);
    return(VGD_ERROR);
    break;
  }
  ier = c_fstfrm(iun);
  ier = c_fclos(iun);
  return(VGD_OK);
}

//========================================================================
extern "C" void c_new_build_all() {
  
  int i, ier, status = VGD_OK;

  ier = vgrid::Cvgd_putopt_int("ALLOW_SIGMA",1);

  for (i = 0; i < (int) n_file; i++) {
    if(test_it(filenames[i],i) == VGD_ERROR)
    {
      status = VGD_ERROR;
    }
  }  

  ier = c_ut_report(status,"testing new_build");  
  
}
