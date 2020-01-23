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
};

#define n_file (sizeof (filenames) / sizeof (const char *))

static float c_convip_IP2Level(int IP,int *kind) {

   int    mode=-1, flag=0, strglen=0;
   float  level=0.0;
   char   format;

   /*Convertir en niveau reel*/
    f77name(convip_plus)(&IP,&level,kind,&mode,&format,&flag);

   return(level);
}

//========================================================================
int check_gen_1001_2001_4001(vgrid *my_vgrid, int vcode){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float *hyb;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_1001_2001_4001, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_gen(&my_vgrid2, kind, version, hyb, nk, NULL, NULL, NULL, NULL, NULL,
		   -1, -1, NULL, NULL, 0) == VGD_ERROR ) {
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
    if( Cvgd_build_from_hyb_1001(&my_vgrid2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 2001:
    if( Cvgd_build_from_hyb_2001(&my_vgrid2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 4001:
    if( Cvgd_new_gen_4001(&my_vgrid2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  default:
    printf("In test ERROR in check_gen_1001_2001_4001, unsupported Vcode %d\n",vcode);
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
  free(hyb);
  return(VGD_OK);
}
//========================================================================
int check_gen_1002(vgrid *my_vgrid){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float *hyb;
  double ptop_8;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_double("PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_1002, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
  }
  printf("  Testing generic interface\n");
  if( Cvgd_new_gen(&my_vgrid2, kind, version, hyb, nk, NULL, NULL, &ptop_8, NULL, NULL,
		   0, 0, NULL, NULL, 0) == VGD_ERROR) {
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
  if( Cvgd_new_gen_1002(&my_vgrid2, hyb, nk, ptop_8, 0, 0) == VGD_ERROR) {
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
  free(hyb);
  return(VGD_OK);
}

//========================================================================
int check_gen_5001(vgrid *my_vgrid){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float rc_1, *hyb;
  double ptop_8, pref_8;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("NL_M", &nk, 0) == VGD_ERROR ){
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
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_1002, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_gen(&my_vgrid2, kind, version, hyb, nk, &rc_1, NULL,
	      &ptop_8, &pref_8, NULL, 0, 0, NULL, NULL, 0) == VGD_ERROR) {
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
  if( Cvgd_new_gen_5001(&my_vgrid2, hyb, nk, ptop_8, pref_8, rc_1, 0, 0) == VGD_ERROR) {
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
    
  free(hyb);
  free(ip1_m);

  return(VGD_OK);
}

//========================================================================
int check_gen_5002(vgrid *my_vgrid){
  int kind, kind2, version, nl_m, ier, nk, k;
  int *ip1_m = NULL;
  float rc_1, rc_2, *hyb;
  double ptop_8, pref_8;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl_m, 0) == VGD_ERROR ){
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
  nk = nl_m - 1;
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_5002, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
    //printf("k = %d, hyb[k] = %f\n", k, hyb[k]);
  }
 
  printf("  Testing generic interface\n");
  if( Cvgd_new_gen(&my_vgrid2, kind, version, hyb, nk, &rc_1, &rc_2,
		   &ptop_8, &pref_8, NULL,0, 0, NULL, NULL, 0) == VGD_ERROR ) {
    printf("ERROR: check_gen_5002, problem with Cvgd_new_gen\n");
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
  if( Cvgd_new_gen_5002(&my_vgrid2, hyb, nk, ptop_8, pref_8, rc_1, rc_2, 0, 0) == VGD_ERROR) {
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
  
  free(hyb);
  free(ip1_m);
  return(VGD_OK);
}
//========================================================================
int check_gen_5005(vgrid *my_vgrid){
  int kind, kind2, version, nl, ier, nk, k;
  int *ip1_m = NULL;
  float rc_1, rc_2, *hyb, dhm, dht;
  double pref_8, ptop_out_8;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
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
  if( my_vgrid->Cvgd_get_float("DHM ", &dhm, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("DHT ", &dht, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  nk = nl - 2;
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_5005, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
    //printf("k = %d, hyb[k] = %f\n", k, hyb[k]);
  }
  
  printf("  Testing generic interface\n");
  if( Cvgd_new_gen(&my_vgrid2, kind, version, hyb, nk, &rc_1, &rc_2, 
		      NULL, &pref_8, &ptop_out_8, 0, 0, &dhm, &dht, 0) == VGD_ERROR) {
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
  if( Cvgd_new_gen_5005(&my_vgrid2, hyb, nk, pref_8, &ptop_out_8, rc_1, rc_2, 0, 0, dhm, dht) == VGD_ERROR) {
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

  free(hyb);
  free(ip1_m);
  return(VGD_OK);
}

//========================================================================
int check_gen_5100(vgrid *my_vgrid){
  int kind, kind2, version, nl, ier, nk, k;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2, rc_3, rc_4, *hyb, dhm, dht;
  double pref_8, ptop_out_8;
  vgrid *my_vgrid2;

  if( my_vgrid->Cvgd_get_int("KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int("VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_int_1d("VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
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
  if( my_vgrid->Cvgd_get_float("DHM ", &dhm, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid->Cvgd_get_float("DHT ", &dht, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  nk = nl - 2;
  hyb = (float*)malloc( nk * sizeof(float) );
  if(! hyb ){ 
    printf("ERROR: check_gen_5100, problem allocating hyb\n");
    return(VGD_ERROR);
  }
  for( k=0; k < nk; k++){
    hyb[k] = c_convip_IP2Level(ip1_m[k], &kind2);
    //printf("k = %d, hyb[k] = %f\n", k, hyb[k]);
  }
  // No generic interface for 5100
  printf("  Testing specific interface\n");
  if( Cvgd_new_gen_5100(&my_vgrid2, hyb, nk, pref_8, &ptop_out_8, rc_1, rc_2, rc_3, rc_4, 0, 0, dhm, dht, 1) == VGD_ERROR) {
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
  
  free(hyb);
  free(ip1_m);
  free(ip1_t);
  return(VGD_OK);
}

//========================================================================
int test_it(char *filename, int ind) {
  int ier, iun, vcode;
  iun = 10 + ind;
  char mode[]="RND+R/O";
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
    if( check_gen_1001_2001_4001(my_vgrid, vcode) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 1002:
    if( check_gen_1002(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5001:
    if( check_gen_5001(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5002:
    if( check_gen_5002(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5005:
    if( check_gen_5005(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5100:
    if( check_gen_5100(my_vgrid) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  default:
    printf("In test ERROR unsupported Vcode %d\n",vcode);
    return(VGD_ERROR);
  } 
  return(VGD_OK);
}

//========================================================================
extern "C" void c_new_gen_all() {
  
  int i, ier, status = VGD_OK;

  ier = vgrid::Cvgd_putopt_int("ALLOW_SIGMA",1);

  for (i = 0; i < (int) n_file; i++) {
    printf ("Testing %s\n", filenames[i]);
    if(test_it(filenames[i],i) == VGD_ERROR)
      status = VGD_ERROR;
  }  

  ier = c_ut_report(status,"testing new_gen");  
  
}
