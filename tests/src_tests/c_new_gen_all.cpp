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
int check_gen_1001_2001_4001(vgrid_descriptor *vgd, int vcode){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float *hyb;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
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
  if( my_vgrid.Cvgd_new_gen(&vgd2, kind, version, hyb, nk, NULL, NULL, NULL, NULL, NULL,
		   -1, -1, NULL, NULL, 0) == VGD_ERROR ) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  my_vgrid.Cvgd_free(&vgd2);

  printf("  Testing specific interface\n");
  switch(vcode) {
  case 1001:
    if( my_vgrid.Cvgd_new_gen_1001(&vgd2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 2001:
    if( my_vgrid.Cvgd_new_gen_2001(&vgd2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  case 4001:
    if( my_vgrid.Cvgd_new_gen_4001(&vgd2, hyb, nk, 0, 0) == VGD_ERROR) {
      return(VGD_ERROR);
    }
    break;
  default:
    printf("In test ERROR in check_gen_1001_2001_4001, unsupported Vcode %d\n",vcode);
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(hyb);
  my_vgrid.Cvgd_free(&vgd2);
  return(VGD_OK);
}
//========================================================================
int check_gen_1002(vgrid_descriptor *vgd){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float *hyb;
  double ptop_8;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PTOP", &ptop_8, 0) == VGD_ERROR ){
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
  if( my_vgrid.Cvgd_new_gen(&vgd2, kind, version, hyb, nk, NULL, NULL, &ptop_8, NULL, NULL,
		   0, 0, NULL, NULL, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  my_vgrid.Cvgd_free(&vgd2);
  
  printf("  Testing specific interface\n");
  if( my_vgrid.Cvgd_new_gen_1002(&vgd2, hyb, nk, ptop_8, 0, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  free(ip1_m);
  free(hyb);
  my_vgrid.Cvgd_free(&vgd2);
  return(VGD_OK);
}

//========================================================================
int check_gen_5001(vgrid_descriptor *vgd){
  int kind, kind2, version, nk, ier, k;
  int *ip1_m = NULL;
  float rc_1, *hyb;
  double ptop_8, pref_8;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"NL_M", &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nk, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_1", &rc_1, 0) == VGD_ERROR ){
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
  if( my_vgrid.Cvgd_new_gen(&vgd2, kind, version, hyb, nk, &rc_1, NULL,
	      &ptop_8, &pref_8, NULL, 0, 0, NULL, NULL, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  my_vgrid.Cvgd_free(&vgd2);
  
  printf("  Testing specific interface\n");
  if( my_vgrid.Cvgd_new_gen_5001(&vgd2, hyb, nk, ptop_8, pref_8, rc_1, 0, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
    
  free(hyb);
  free(ip1_m);
  my_vgrid.Cvgd_free(&vgd2);

  return(VGD_OK);
}

//========================================================================
int check_gen_5002(vgrid_descriptor *vgd){
  int kind, kind2, version, nl_m, ier, nk, k;
  int *ip1_m = NULL;
  float rc_1, rc_2, *hyb;
  double ptop_8, pref_8;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nl_m, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PTOP", &ptop_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_2", &rc_2, 0) == VGD_ERROR ){
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
  if( my_vgrid.Cvgd_new_gen(&vgd2, kind, version, hyb, nk, &rc_1, &rc_2,
		   &ptop_8, &pref_8, NULL,0, 0, NULL, NULL, 0) == VGD_ERROR ) {
    printf("ERROR: check_gen_5002, problem with Cvgd_new_gen\n");
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  my_vgrid.Cvgd_free(&vgd2);

  printf("  Testing specific interface\n");
  if( my_vgrid.Cvgd_new_gen_5002(&vgd2, hyb, nk, ptop_8, pref_8, rc_1, rc_2, 0, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(hyb);
  free(ip1_m);
  my_vgrid.Cvgd_free(&vgd2);
  return(VGD_OK);
}
//========================================================================
int check_gen_5005(vgrid_descriptor *vgd){
  int kind, kind2, version, nl, ier, nk, k;
  int *ip1_m = NULL;
  float rc_1, rc_2, *hyb, dhm, dht;
  double pref_8, ptop_out_8;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"DHM ", &dhm, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"DHT ", &dht, 0) == VGD_ERROR ){
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
  if( my_vgrid.Cvgd_new_gen(&vgd2, kind, version, hyb, nk, &rc_1, &rc_2, 
		      NULL, &pref_8, &ptop_out_8, 0, 0, &dhm, &dht, 0) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  // Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  my_vgrid.Cvgd_free(&vgd2);

  printf("  Testing specific interface\n");
  if( my_vgrid.Cvgd_new_gen_5005(&vgd2, hyb, nk, pref_8, &ptop_out_8, rc_1, rc_2, 0, 0, dhm, dht) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }

  free(hyb);
  free(ip1_m);
  my_vgrid.Cvgd_free(&vgd2);
  return(VGD_OK);
}

//========================================================================
int check_gen_5100(vgrid_descriptor *vgd){
  int kind, kind2, version, nl, ier, nk, k;
  int *ip1_m = NULL, *ip1_t = NULL;
  float rc_1, rc_2, rc_3, rc_4, *hyb, dhm, dht;
  double pref_8, ptop_out_8;
  vgrid_descriptor *vgd2 = NULL;
  vgrid my_vgrid;

  if( my_vgrid.Cvgd_get_int(vgd,"KIND", &kind, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd,"VERS", &version, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int_1d(vgd,"VIPM", &ip1_m, &nl, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_double(vgd,"PREF", &pref_8, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_1", &rc_1, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_2", &rc_2, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_3", &rc_3, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"RC_4", &rc_4, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"DHM ", &dhm, 0) == VGD_ERROR ){
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_float(vgd,"DHT ", &dht, 0) == VGD_ERROR ){
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
  my_vgrid.Cvgd_free(&vgd2);
  printf("  Testing specific interface\n");
  if( my_vgrid.Cvgd_new_gen_5100(&vgd2, hyb, nk, pref_8, &ptop_out_8, rc_1, rc_2, rc_3, rc_4, 0, 0, dhm, dht, 1) == VGD_ERROR) {
    return(VGD_ERROR);
  }
  //Test equality
  ier = my_vgrid.Cvgd_vgdcmp(vgd, vgd2);
  if( ier != 0 ){
    printf("     Descritors not equal, Cvgd_vgdcmp code is %d\n", ier);
    return(VGD_ERROR);
  } else {
    printf("     Descritors are equal.\n");
  }
  
  free(hyb);
  free(ip1_m);
  free(ip1_t);
  my_vgrid.Cvgd_free(&vgd2);
  return(VGD_OK);
}

//========================================================================
int test_it(char *filename, int ind) {
  int ier, iun, vcode;
  iun = 10 + ind;
  char mode[]="RND+R/O";
  vgrid_descriptor vgd;
  vgrid_descriptor *vgd_p = NULL;
  vgrid my_vgrid;

  vgd_p=&vgd;

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

  if( my_vgrid.Cvgd_new_read(vgd_p, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(VGD_ERROR);
  }
  if( my_vgrid.Cvgd_get_int(vgd_p, "VCOD", &vcode, 0) == VGD_ERROR ){
    printf("ERROR with  Cvgd_get_int on VCOD\n");
    return(VGD_ERROR);
  }
  switch(vcode) {
  case 1001:
  case 2001:
  case 4001:
    if( check_gen_1001_2001_4001(vgd_p, vcode) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 1002:
    if( check_gen_1002(vgd_p) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5001:
    if( check_gen_5001(vgd_p) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5002:
    if( check_gen_5002(vgd_p) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5005:
    if( check_gen_5005(vgd_p) == VGD_ERROR){
      return(VGD_ERROR);
    }
    break;
  case 5100:
    if( check_gen_5100(vgd_p) == VGD_ERROR){
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
  vgrid my_vgrid;

  ier = my_vgrid.Cvgd_putopt_int("ALLOW_SIGMA",1);

  for (i = 0; i < (int) n_file; i++) {
    printf ("Testing %s\n", filenames[i]);
    if(test_it(filenames[i],i) == VGD_ERROR)
      status = VGD_ERROR;
  }  

  ier = c_ut_report(status,"testing new_gen");  
  
}
