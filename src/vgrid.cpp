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

#include "vgrid.hpp"
#include "vgrid_subclasses.hpp"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "rpnmacros.h"
#include "armnlib.hpp"

#define STR_INIT(str,len) if(len>1) memset(str,' ',len-1); if(len>0) str[len-1] = '\0'

#define STDA76_N_LAYER 7
static float stda76_tgrad[STDA76_N_LAYER]     = { -6.5E-3,    0.0,    1.0E-3, 2.8E-3, 0.0,    -2.8E-3, -2.0E-3 };
static float stda76_zgrad[STDA76_N_LAYER + 1] = { 0., 11000., 20000., 32000., 47000., 51000.,  71000.,  84852. };

// Constants
#define MAX_VKIND    100
// Macros
#define FREE(x) if(x) { free(x); x=NULL; }

// Options
int ALLOW_SIGMA = 0;

// Don't want to depend on modelutils so define constantes here.
// These are not used to transform variables like T to GZ so it will not
// produce any inconsistancies with data in fst files. 
static float VGD_RGASD = 0.287050000000E+03;
static float VGD_GRAV  = 0.980616000000E+01;
static float VGD_TCDK  = 0.273150000000E+03;

// equality operator for VGD_TFSTD_ext
// bool operator==(const VGD_TFSTD_ext& lhs, const VGD_TFSTD_ext& rhs)
// {
//   if(lhs.dateo != rhs.dateo)return false;
//   if(lhs.datev != rhs.datev)return false;
//   if(lhs.deet != rhs.deet)return false;
//   if(lhs.npas != rhs.npas)return false;
//   if(lhs.nbits != rhs.nbits)return false;
//   if(lhs.datyp != rhs.datyp)return false;
//   if(lhs.ip1 != rhs.ip1)return false;
//   if(lhs.ip2 != rhs.ip2)return false;
//   if(lhs.ip3 != rhs.ip3)return false;
//   if(lhs.ni != rhs.ni)return false;
//   if(lhs.nj != rhs.nj)return false;
//   if(lhs.nk != rhs.nk)return false;
//   if(lhs.ig1 != rhs.ig1)return false;
//   if(lhs.ig2 != rhs.ig2)return false;
//   if(lhs.ig3 != rhs.ig3)return false;
//   if(lhs.ig4 != rhs.ig4)return false;
//   if(lhs.swa != rhs.swa)return false;
//   if(lhs.lng != rhs.lng)return false;
//   if(lhs.dltf != rhs.dltf)return false;
//   if(lhs.ubc != rhs.ubc)return false;
//   if(lhs.extra1 != rhs.extra1)return false;
//   if(lhs.extra2 != rhs.extra2)return false;
//   if(lhs.extra3 != rhs.extra3)return false;
//   if(strcmp(lhs.typvar, rhs.typvar) != 0)return false;
//   if(strcmp(lhs.nomvar, rhs.nomvar) != 0)return false;
//   if(strcmp(lhs.etiket, rhs.etiket) != 0)return false;
//   if(strcmp(lhs.grtyp, rhs.grtyp) != 0)return false;
//   return true;
// }

// inequality operator for VGD_TFSTD_ext
// bool operator!=(const VGD_TFSTD_ext& lhs, const VGD_TFSTD_ext& rhs)
// {
//   return !(lhs == rhs);
// }


// beginning of class vgrid

int vgrid::is_valid(int *table_valid)
{
  int k;
  for( k = 0; k < VALID_TABLE_SIZE; k++){
    if(this->vcode == table_valid[k]){
      return(1);
    }
  }
  return 0;
}

int vgrid::is_option(int *table_option)
{
  int k;
  for( k = 0; k < VALID_TABLE_SIZE; k++){
    if(this->vcode == table_option[k]){
      return(1);
    }
  }
  return 0;
}

int vgrid::Cvgd_is_valid(char *valid_table_name)
{
  if( strcmp(valid_table_name, "SELF") == 0 ){
    return(this->valid);
  } else if( strcmp(valid_table_name, "ptop_out_8_valid") == 0 ){
    return(this->is_valid(ptop_out_8_valid));
  } else if( strcmp(valid_table_name, "ptop_8_valid")     == 0 ){
    return(this->is_valid(ptop_8_valid));
  } else if( strcmp(valid_table_name, "pref_8_valid")     == 0 ){
    return(this->is_valid(pref_8_valid));
  } else if( strcmp(valid_table_name, "rcoef1_valid")     == 0 ){
    return(this->is_valid(rcoef1_valid));
  } else if( strcmp(valid_table_name, "rcoef2_valid")     == 0 ){
    return(this->is_valid(rcoef2_valid));
  } else if( strcmp(valid_table_name, "rcoef3_valid")     == 0 ){
    return(this->is_valid(rcoef3_valid));
  } else if( strcmp(valid_table_name, "rcoef4_valid")     == 0 ){
    return(this->is_valid(rcoef4_valid));
  } else if( strcmp(valid_table_name, "a_m_8_valid")      == 0 ){
    return(this->is_valid(a_m_8_valid));
  } else if( strcmp(valid_table_name, "b_m_8_valid")      == 0 ){
    return(this->is_valid(b_m_8_valid));
  } else if( strcmp(valid_table_name, "c_m_8_valid")      == 0 ){
    return(this->is_valid(c_m_8_valid));
  } else if( strcmp(valid_table_name, "a_t_8_valid")      == 0 ){
    return(this->is_valid( a_t_8_valid));
  } else if( strcmp(valid_table_name, "b_t_8_valid")      == 0 ){
    return(this->is_valid(b_t_8_valid));
  } else if( strcmp(valid_table_name, "c_t_8_valid")      == 0 ){
    return(this->is_valid(c_t_8_valid));
  } else if( strcmp(valid_table_name, "ip1_m_valid")      == 0 ){
    return(this->is_valid(ip1_m_valid));
  } else if( strcmp(valid_table_name, "ip1_t_valid")      == 0 ){
    return(this->is_valid(ip1_t_valid));
  } else if( strcmp(valid_table_name, "ref_name_valid")   == 0 ){
    if( strcmp(this->ref_name,VGD_NO_REF_NOMVAR) == 0 ){
      return 0;
    } else {
      return 1;
    }
  } else if( strcmp(valid_table_name, "ref_namel_valid")  == 0 ){
    if( strcmp(this->ref_namel,VGD_NO_REF_NOMVAR) == 0 ){
      return 0;
    } else {
      return 1;
    }
  } else if( strcmp(valid_table_name, "dhm_valid")        == 0 ){
    return(this->is_valid(dhm_valid));
  } else if( strcmp(valid_table_name, "dht_valid")        == 0 ){
    return(this->is_valid(dht_valid));
  } else if( strcmp(valid_table_name, "dhw_valid")        == 0 ){
    return(this->is_valid(dhw_valid));
  } else if( strcmp(valid_table_name, "is_in_logp")       == 0 ){
    return(this->is_valid(is_in_logp));
  } else {
    printf("(Cvgd) Warning : in Cvgd_is_valid, valid_table_name '%s' does not exist\n",valid_table_name);
    return(0);
  }
}

int vgrid::is_required_double( double *ptr, int *table_valid, char *message) {
  if( this->is_valid(table_valid)) {
    if (! ptr) {
      printf("(Cvgd) ERROR: %s is a required constructor entry\n", message);
      return(0);
    }
  } else {
    if (ptr) {
      printf("(Cvgd) ERROR: %s is not a required constructor entry\n", message);
      return(0);
    }
  }
  return(1);
}
int vgrid::is_required_float(float *ptr, int *table_valid, char *message) {
  if( this->is_valid(table_valid)) {
    if (! ptr) {
      printf("(Cvgd) ERROR: %s is a required constructor entry\n", message);
      return(0);
    }
  } else {
    if (ptr) {
      printf("(Cvgd) ERROR: %s is not a required constructor entry\n", message);
      return(0);
    }
  }
  return(1);
}

void vgrid::set_match_ipig(int match_ipig)
{
  this->match_ipig = match_ipig;
}

int my_alloc_int(int **vec, int size, char *message){
  *vec = (int*)malloc ( size * sizeof(int) );
  if(! *vec){    
    printf("%s %d\n",message, size);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int my_alloc_float(float **vec, int size, char *message){
  *vec = (float*)malloc ( size * sizeof(float) );
  if(! *vec){    
    printf("%s %d\n",message, size);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int my_alloc_double(double **vec, int size, char *message){
  *vec = (double*)malloc ( size * sizeof(double) );
  if(! *vec){    
    printf("%s %d\n",message, size);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

void vgrid::flip_transfer_d2c(char *name, double val_8) {  
  // NOTE : Character un-stuffing from double is done right to left (Little endian style)
  int i;
  union {
    double d;
    uint64_t tt;
  } u;
  u.d = val_8;
  //printf("DECODE  %16.16lx %f\n",u.tt, val_8);
  name[4] = '\0';
  for( i = 0; i < 4; i++ ){
    name[i] = (u.tt >> 8*(i)) & 0xFF;
  }
  //printf("DECODE '%s'\n",name);
}

void vgrid::flip_transfer_c2d(char *name, void *val_8) {
  // NOTE : Character stuffing into double is done right to left (Little endian style)
  int i;
  uint64_t *xx = (unsigned long *)val_8;
  uint64_t tt, bl;
  tt = 0;
  bl = ' ';
  //printf("ENCODE name='%s'\n",name);  
  for( i = strlen(name); i < 4; i++ ){
    tt <<= 8;
    tt |= bl;
  }
  for( i = 0; i < strlen(name); i++ ){
    if( i == 4 ) {
      break;
    }
    //printf("ENCODE name[i]='%c'\n",name[i]);
    tt = (tt << 8) | name[3-i];
    //printf("ENCODE tt=%16.16lx\n",tt);
  }
  *xx = tt;
  //printf("ENCODE tt=%16.16lx\n",tt);
}

int max_int(int *vec, int ni) {
  int i, ind = 0;
  for( i = 1; i < ni; i++){
    if( vec[i] > vec[ind] )
      ind = i;
  }
  return(vec[ind]);
}

double vgrid::c_get_error(char *key, int quiet) {
  if (! quiet) {
    printf("(Cvgd) ERROR in c_get_error, attempt to retrieve invalid key %s\n",key);
  }
  return(VGD_MISSING);
}

void vgrid::c_hypsometric (float *pkp, float pk, float Tk, float gammaT, float zk, float zkp){
  // Compute pressure pkp which is at top of the following atmopheric layer
  //
  //  \(pkp,zkp)
  //   \
  //    \ gammaT (laspe rate in the layer, may be zero or very small)
  //     \
  //      \(Tk,pk,zk) 
  //
  static float epsilon = 1.e-6;
  if( gammaT > -epsilon && gammaT < epsilon ) {
    *pkp = (float) (pk * exp( -VGD_GRAV/(VGD_RGASD*Tk) * (zkp-zk) ));
  } else {
    *pkp = (float) (pk * exp( -VGD_GRAV/(VGD_RGASD*gammaT) * log(gammaT*(zkp-zk)/Tk+1.) ));
  }
}

int vgrid::c_set_stda_layer(int ind, float Tk, float pk, float *zk, float *zkp, float *gammaT, float *pkp, int *zero_lapse_rate) {

  //Andre PLante 2017
  //
  //Example of a Standard Atmophere Layer
  //
  //  \(pkp,zkp)
  //   \
  //    \ gammaT (laspe rate in the layer)
  //     \
  //      \(Tk,pk,zk) 
  //
  //Where
  //      ind    (in)  is the index of the Standard Atmophere layer
  //      gammaT (out) is the laspe rate in the layer ind
  //      Tk     (in)  is the temperature at the base of the layer ind, it is specified by the user
  //      pk     (in)  is the pressure    at the base of the layer ind, it is specified by the user
  //      zk     (out) is the height      at the base of the layer ind, given by the Standard Atmophere
  //      zkp    (out) is the height      at the top  of the layer ind, given by the Standard Atmophere
  //      pkp    (out) is the pressure    at the top  of the layer ind, computed here with the above parameters
  
  static float epsilon = 1.e-6;

  if( ind >=  STDA76_N_LAYER ){
    printf("(Cvgd) ERROR in c_set_stda_layer, maximum layer excedded\n");    
    return(VGD_ERROR);
  }
  *zero_lapse_rate = 0;
  if(stda76_tgrad[ind] > -epsilon && stda76_tgrad[ind] < epsilon){
    *zero_lapse_rate = 1;
  }   
  *zk  = stda76_zgrad[ind];
  *zkp = stda76_zgrad[ind+1];
  *gammaT = stda76_tgrad[ind];
  c_hypsometric (pkp, pk, Tk, *gammaT, *zk, *zkp);
  return(VGD_OK);
}

int vgrid::c_get_stda76(float *Tk, float *pk, float *zk, float *gammaT, 
			int *zero_lapse_rate){
  int ind;

  // Prepare temp, press and height array for the STDA76_N_LAYER + 1
  // standard atmosphere interfaces.
  // Start at Normal Temperature and Pressure
  // Tk, pk, zk size STDA76_N_LAYER + 1
  // gammaT, zero_lapse_rate size STDA76_N_LAYER
			  
  Tk[0]  = VGD_STDA76_SFC_T;
  pk[0]  = VGD_STDA76_SFC_P;
  for(ind=0; ind < STDA76_N_LAYER; ind++){
    if( c_set_stda_layer( ind, Tk[ind], pk[ind], &zk[ind], &zk[ind+1],
			  &gammaT[ind], &pk[ind+1], &zero_lapse_rate[ind])
	== VGD_ERROR ){
      return(VGD_ERROR);
    }
    if( zero_lapse_rate[ind] ){
      Tk[ind+1] = Tk[ind];
    } else {
      Tk[ind+1] = Tk[ind] + gammaT[ind]*(zk[ind+1] - zk[ind]);
    }
  }
  
  //for(ind=0; ind < STDA76_N_LAYER+1; ind++){
  //  printf("ind = %d, Tk[ind] = %f, pk[ind] = %f, zk[ind] = %f\n",
  //          ind, Tk[ind], pk[ind], zk[ind]);
  //}
  //ind = 0, Tk[ind] = 288.149994, pk[ind] = 101325.000000, zk[ind] = 0.000000
  //ind = 1, Tk[ind] = 216.649994, pk[ind] = 22633.392578, zk[ind] = 11000.000000
  //ind = 2, Tk[ind] = 216.649994, pk[ind] = 5475.515137, zk[ind] = 20000.000000
  //ind = 3, Tk[ind] = 228.649994, pk[ind] = 868.180603, zk[ind] = 32000.000000
  //ind = 4, Tk[ind] = 270.649994, pk[ind] = 110.935928, zk[ind] = 47000.000000
  //ind = 5, Tk[ind] = 270.649994, pk[ind] = 66.958076, zk[ind] = 51000.000000
  //ind = 6, Tk[ind] = 214.649994, pk[ind] = 3.957995, zk[ind] = 71000.000000
  //ind = 7, Tk[ind] = 186.945999, pk[ind] = 0.373567, zk[ind] = 84852.000000
  return(VGD_OK);
}

int vgrid::c_stda76_temp_from_press(int *i_val, int nl, float *temp){
  int k, ind, zero_lapse_rate;
  float pkp, Tk, pk, hgts_stda, zk, zkp, gammaT;
  float *levs = NULL;
  
  levs = (float*)malloc( nl * sizeof(float) );
  if(! levs){
    printf("(Cvgd) ERROR in c_stda76_temp_from_press, problem allocating levs\n");
    free(levs);
    return(VGD_ERROR);
  }
  
  if(! this->Cvgd_is_valid("ref_namel_valid") ){    
    if( this->Cvgd_levels(1, 1, nl, i_val, levs, &VGD_STDA76_SFC_P, 0) == VGD_ERROR){
      printf("(Cvgd) ERROR in c_stda76_temp_from_press, problem with Cvgd_levels (computing pressure profile with one ref)\n");
      return(VGD_ERROR);
    }
  } else {
    if( this->Cvgd_levels_2ref(1, 1, nl, i_val, levs, &VGD_STDA76_SFC_P, &VGD_STDA76_SFC_P, 0) == VGD_ERROR){
      printf("(Cvgd) ERROR in c_stda76_temp_from_press, problem with Cvgd_levels (computing pressure profile with two refs)\n");
      return(VGD_ERROR);
    }
  }
  ind=0; pkp=-1.f;
  // Start at Normal Temperature and Pressure
  Tk  = VGD_STDA76_SFC_T;
  pk  = VGD_STDA76_SFC_P;
  if( c_set_stda_layer( ind, Tk, pk, &zk, &zkp, &gammaT, &pkp, &zero_lapse_rate) == VGD_ERROR ){
    return(VGD_ERROR);
  }  
  for( k = nl-1; k >= 0; k--){
    // Integrate the hypsometric equation, change temperature gradiant when height is above high boundary stda76_zgrad
    while( levs[k] <= pkp){
      // Current pressure level is above current stda layer
      Tk = Tk + gammaT * (zkp - zk);
      pk = pkp;
      ind++;
      if( c_set_stda_layer( ind, Tk, pk, &zk, &zkp, &gammaT, &pkp, &zero_lapse_rate) == VGD_ERROR){
	return(VGD_ERROR);
      }
    }
    if( zero_lapse_rate ){
      //hgts_stda = (float) zk - (VGD_RGASD*Tk)/VGD_GRAV * log(levs[k]/pk);
      temp[k] = Tk;
    } else {
      hgts_stda = (float) (zk + Tk/gammaT * ( exp(-(VGD_RGASD*gammaT)/VGD_GRAV * log(levs[k]/pk )) - 1.f ));
      temp[k] = Tk + gammaT*(hgts_stda-zk);
    }
  }
  free(levs);
  return(VGD_OK);
}

int vgrid::c_stda76_temp_pres_from_heights(int *i_val, int nl, float *temp, float *pres, float *sfc_temp, float *sfc_pres){
  int ind = 0, k, zero_lapse_rate;
  float *levs = NULL;
  float zero = 0.f, pk, pkp, Tk, zk, zkp, gammaT;

  levs = (float*)malloc( nl * sizeof(float) );
  if(! levs){
    printf("(Cvgd) ERROR in c_stda76_temp_pres_from_heights, problem allocating levs\n");
    free(levs);
    return(VGD_ERROR);
  }
  
  if(! this->Cvgd_is_valid("ref_namel_valid") ){    
    if( this->Cvgd_levels(1, 1, nl, i_val, levs, &zero, 0) == VGD_ERROR){
      printf("(Cvgd) ERROR in c_stda76_temp_pres_from_heights, problem with Cvgd_levels (computing heights profile with one ref)\n");
      return(VGD_ERROR);
    }
  } else {
    if( this->Cvgd_levels_2ref(1, 1, nl, i_val, levs, &zero, &zero, 0) == VGD_ERROR){
      printf("(Cvgd) ERROR in c_stda76_temp_pres_from_heights, problem with Cvgd_levels (computing heights profile with two refs)\n");
      return(VGD_ERROR);
    }
  }
  // Start at Normal Temperature and Pressure
  if( sfc_temp ){
    Tk = *sfc_temp;
  } else {
    Tk = VGD_STDA76_SFC_T;
  }
  if( sfc_pres ){
    pk = *sfc_pres;
  } else {
    pk = VGD_STDA76_SFC_P;
  }
  if( c_set_stda_layer( ind, Tk, pk, &zk, &zkp, &gammaT, &pkp, &zero_lapse_rate) == VGD_ERROR ){
    printf("Cvgd ERROR in c_stda76_temp_pres_from_heights with c_set_stda_layer\n");
    return(VGD_ERROR);
  }
  for( k = nl-1; k >= 0; k--){
    // Change temperature gradiant when height is above high boundary stda76_zgrad
    while( levs[k] >= zkp ){
      Tk = Tk + gammaT * (zkp - zk);
      pk = pkp;
      ind++;
      if( c_set_stda_layer( ind, Tk, pk, &zk, &zkp, &gammaT, &pkp, &zero_lapse_rate) == VGD_ERROR){
	return(VGD_ERROR);
      }
    }
    // Complete the layer with current gradient and levs[k] height
    temp[k] =  Tk + stda76_tgrad[ind] * (levs[k] - stda76_zgrad[ind]);
    //printf("k=%d, levs[k]=%f, temp[k]=%f\n",k,levs[k],temp[k]);
    // Compute pressure at zkp with temp or temp profile
    c_hypsometric (pres + k , pk, Tk, gammaT, zk, levs[k]);
  }
  free(levs);
  return(VGD_OK);

}

void vgrid::Cvgd_table_shape(int **tshape) {
  (*tshape)[0] = this->table_ni;
  (*tshape)[1] = this->table_nj;
  (*tshape)[2] = this->table_nk;
}

void my_copy_double(double *aa, double **bb, int ind){
  while (ind--) {
    (*bb)[ind] = aa[ind];
  }
}

void my_copy_int(int *aa, int **bb, int ind){
  while (ind--) {
    (*bb)[ind] = aa[ind];
  }
}

int vgrid::same_vec_i(int *vec1, int n1, int *vec2, int n2) {
  int i;
  if(vec1) {
    if (vec2) {
      if ( n1 == n2 ) {
	for(i = 0; i < n1; i++) {
	  if ( vec1[i] != vec2[i] ) return(-1);
	}
      } else {
	// Vectors are not the same size.
	return(-2);
      }
    } else {
      // vec2 not allocated
      return(-3);
    }
  }
  // Vector are the same or are not allocated.
  return(0);
}

int vgrid::same_vec_r8(double *vec1, int n1, double *vec2, int n2) {
  if(vec1) {
    if (vec2) {
      if ( n1 == n2 ) {	
	//for(i = 0; i < n1; i++) {
	//  if ( vec1[i] != vec2[i] ) return(-1);
	//}
	if( memcmp( vec1, vec2, n1*sizeof(double)/sizeof(char) ) ) 
	  return(-1);
      } else {
	// Vectors are not the same size.
	return(-2);
      }
    } else {
      // vec2 not allocated
      return(-3);
    }
  }
  // Vector are the same or are not allocated.
  return(0);
}

int vgrid::similar_vec_r8(double *vec1, int n1, double *vec2, int n2) {
  int i;
  if(vec1) {
    if (vec2) {
      if ( n1 == n2 ) {
	for(i = 0; i < n1; i++) {
	  if( fabs(vec1[i]) < 1.e-307 ){
	    if( fabs(vec2[i]) > 1.e-307 ){
	      //printf("zero, vec1[i] = %f, vec2[i] = %f\n", vec1[i], vec2[i]);
	      return(-1);
	    }
	  } else {
	    if ( fabs(vec1[i]-vec2[i])/fabs(vec1[i]) > 1.e-15 ){
	      //printf("non zero, vec1[i] = %f, vec2[i] = %f, fabs(vec1[i]-vec2[i])/vec1[i]= %f\n", vec1[i], vec2[i], fabs(vec1[i]-vec2[i])/vec1[i]);
	      return(-1);
	    }
	  }
	}
      } else {
	// Vectors are not the same size.
	return(-2);
      }
    } else {
      // vec2 not allocated
      return(-3);
    }
  }
  // Vector are the same or are not allocated.
  return(0);
}

int vgrid::c_convip_Level2IP(float level, int kind) {

  int    mode=2,flag=0, IP, strglen=0; 
  char   format;
  
  // Convertir niveau reel en ip1a
  f77name(convip_plus)(&IP,&level,&kind,&mode,&format,&flag);
    
  return(IP);
}

int vgrid::c_convip_Level2IP_old_style(float level, int kind) {

  int    mode=3,flag=0, IP, strglen=0; 
  char   format; 
  
  // Convertir niveau reel en ip1a
  f77name(convip_plus)(&IP,&level,&kind,&mode,&format,&flag);
    
  return(IP);
}

float vgrid::c_convip_IP2Level(int IP,int *kind) {

   int    mode=-1,flag=0, strglen=0;
   float  level=0.0;
   char   format;

   /*Convertir en niveau reel*/
    f77name(convip_plus)(&IP,&level,kind,&mode,&format,&flag);

   return(level);
}

void vgrid::decode_HY(VGD_TFSTD_ext var, double *ptop_8, double *pref_8, float *rcoef){
  // In consultation with Vivian Lee, with decode explicitly instead of using f77 read_decode_hyb
  int kind;
  *ptop_8 = c_convip_IP2Level(var.ip1, &kind) * 100.;
  *pref_8 = var.ig1 * 100.;
  *rcoef = var.ig2/1000.f;
}

int my_fstprm(int key,VGD_TFSTD_ext *ff) {
  //var->ip1 = 62;
  double nhours;
  STR_INIT(ff->typvar,VGD_LEN_TYPVAR);
  STR_INIT(ff->nomvar,VGD_LEN_NAME);
  STR_INIT(ff->etiket,VGD_LEN_ETIK);
  STR_INIT(ff->grtyp, VGD_LEN_GRTYP);
 
  if( c_fstprm(key,
	       &ff->dateo,  &ff->deet,   &ff->npas, 
	       &ff->ni,     &ff->nj,     &ff->nk,
	       &ff->nbits,  &ff->datyp,  
	       &ff->ip1,    &ff->ip2,    &ff->ip3,
	        ff->typvar,  ff->nomvar,  ff->etiket,
	        ff->grtyp,  &ff->ig1,    &ff->ig2,    &ff->ig3, &ff->ig4,
	       &ff->swa,    &ff->lng,    &ff->dltf,   &ff->ubc,
	       &ff->extra1, &ff->extra2, &ff->extra3) < 0 ) {
    printf("(Cvgd) ERROR: cannot fstprm for fstkey %d\n",key);
    return(VGD_ERROR);
  }
  nhours = ff->deet * ff->npas / 3600.;
  f77name(incdatr)(&ff->datev,&ff->dateo,&nhours);
  return(VGD_OK);
}

int vgrid::correct_kind_and_version(int key, int kind, int version, VGD_TFSTD_ext *var, int *status) {
  
  int kind_from_ig1;
  *status=0;
  if( my_fstprm(key, var) == VGD_ERROR ) {
    printf("(Cvgd) ERROR in correct_kind_and_version, with my_fstprm on key %d\n",key);
    return(VGD_ERROR);
  }
  if(kind != -1 && version != -1) {
    if(var->ig1 != kind*1000 + version ) {
      return(VGD_OK);
    }
  } else {
    if(kind != -1) {
      // Get kind from fst vcode (ig1)
      kind_from_ig1 = var->ig1 / 1000;
      if( kind_from_ig1 != kind) {
	return(VGD_OK);
      }
    }
    if(version != -1) {
      // Get version from fst vcode (ig1)
      if(var->ig1-(int)round(var->ig1 / 1000.) != version) {
	return(VGD_OK);
      }
    }
  }
  // If we reach this point, we have a match
  *status = 1;
  return(VGD_OK);

}

int vgrid::C_load_toctoc( VGD_TFSTD_ext var, int key)
{
  int table_size, istat, ni, nj, nk;

  this->table_ni = var.ni;
  this->table_nj = var.nj;
  this->table_nk = var.nk;

  table_size = this->table_ni * this->table_nj * this->table_nk;
  this->table = (double*)malloc ( table_size * sizeof(double) );
  if(! this->table ) {
    printf("(Cvgd) ERROR in C_load_toctoc, cannot allocate table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  istat = c_fstluk(this->table, key, &ni, &nj, &nk);
  if(istat < 0) {
    printf("(Cvgd) ERROR in C_load_toctoc, problem with fstluk\n");
    free(this->table);
    return(VGD_ERROR);
  }
  this->kind             = (int) this->table[0];
  this->version          = (int) this->table[1];

  istat = C_load_var(var);

  return(istat);
}


int vgrid::C_load_var(VGD_TFSTD_ext var)
{
  //this->table_ni = var.ni;
  //this->table_nj = var.nj;
  //this->table_nk = var.nk;

  if(this->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in C_load_var, problem creating record information\n");
    return(VGD_ERROR);
  }

  this->rec.dateo        = var.dateo;
  this->rec.deet         = var.deet;
  this->rec.npas         = var.npas;
  this->rec.nbits        = var.nbits;
  this->rec.datyp        = var.datyp;
  this->rec.ip1          = var.ip1;
  this->rec.ip2          = var.ip2;
  this->rec.ip3          = var.ip3;
  strcpy(this->rec.typvar, var.typvar);
  strcpy(this->rec.nomvar, var.nomvar);
  strcpy(this->rec.etiket, var.etiket);
  this->rec.ig1          = var.ig1;
  this->rec.ig2          = var.ig2;
  this->rec.ig3          = var.ig3;
  this->rec.ig4          = var.ig4;

  return(VGD_OK);
}

int vgrid::Cvgd_vgdcmp(vgrid *vgd2) {

  //int nt1, nt2;
  // Check each element of the structure (except FST attributes) for equality

  if (vcode != vgd2->vcode)                   return(-1);
  if (kind != vgd2->kind)                     return(-2);
  if (version != vgd2->version)               return(-3);
  if (strcmp(ref_name, vgd2->ref_name) != 0 ) return(-4);
  if (strcmp(ref_namel, vgd2->ref_namel) != 0 )return(-20);
  if (nl_w != vgd2->nl_w) return(-23);
  // Note, size nl_m and nl_t are tested in call to same_vec_i below
  if (memcmp(&(ptop_8),&(vgd2->ptop_8), sizeof(double)/sizeof(char) ))return(-5);
  if (memcmp(&(pref_8),&(vgd2->pref_8), sizeof(double)/sizeof(char) ))return(-6);
  if (memcmp(&(rcoef1),&(vgd2->rcoef1), sizeof(float) /sizeof(char) ))return(-7);
  if (memcmp(&(rcoef2),&(vgd2->rcoef2), sizeof(float) /sizeof(char) ))return(-8);
  if (memcmp(&(rcoef3),&(vgd2->rcoef3), sizeof(float) /sizeof(char) ))return(-16);
  if (memcmp(&(rcoef4),&(vgd2->rcoef4), sizeof(float) /sizeof(char) ))return(-17);

   // Check pointer associations and values
  if(same_vec_i (ip1_m, nl_m, vgd2->ip1_m, vgd2->nl_m) != 0) return (-9);
  if(same_vec_i (ip1_t, nl_t, vgd2->ip1_t, vgd2->nl_t) != 0) return (-10);
  if(same_vec_i (ip1_w, nl_w, vgd2->ip1_w, vgd2->nl_w) != 0) return (-24);
  if(same_vec_r8(a_m_8, nl_m, vgd2->a_m_8, vgd2->nl_m) != 0) return (-11);
  //if(same_vec_r8(b_m_8, nl_m, vgd2->b_m_8, vgd2->nl_m) != 0) return (-12);
  if(similar_vec_r8(b_m_8, nl_m, vgd2->b_m_8, vgd2->nl_m) != 0) return (-12);
  //if(same_vec_r8(c_m_8, nl_m, vgd2->c_m_8, vgd2->nl_m) != 0) return (-18);
  if(same_vec_r8(a_t_8, nl_t, vgd2->a_t_8, vgd2->nl_t) != 0) return (-13);
  //if(same_vec_r8(b_t_8, nl_t, vgd2->b_t_8, vgd2->nl_t) != 0) return (-14);
  if(similar_vec_r8(b_t_8, nl_t, vgd2->b_t_8, vgd2->nl_t) != 0) return (-14);
  //if(same_vec_r8(c_t_8, nl_t, vgd2->c_t_8, vgd2->nl_t) != 0) return (-19);
  if(same_vec_r8(a_w_8, nl_w, vgd2->a_w_8, vgd2->nl_w) != 0) return (-25);
  if(similar_vec_r8(b_w_8, nl_w, vgd2->b_w_8, vgd2->nl_w) != 0) return (-26);
  //if(same_vec_r8(c_w_8, nl_w, vgd2->c_w_8, vgd2->nl_w) != 0) return (-27);
 
  // Do not check table since all above parameters consist in a full check, at least it should.
  // Also, the transfer from char to real(kind=8) do not always give same real8 value which
  // made equality to be false even if above parameters are equal.
  //nt1 = table_ni * table_nj * table_nk;
  //nt2 = vgd2->table_ni * vgd2->table_nj * vgd2->table_nk;
  //if(same_vec_r8(table, nt1, vgd2->table, nt2 )            != 0) return (-15);

  return(0);
}

double vgrid::c_comp_diag_a_height(double pref_8, float height) {
  return log(pref_8) - VGD_GRAV*height/(VGD_RGASD*VGD_TCDK);
}
double vgrid::c_comp_diag_a_ip1(double pref_8, int ip1) {
  int kind;
  return log(pref_8) - VGD_GRAV * c_convip_IP2Level(ip1,&kind) / (VGD_RGASD*VGD_TCDK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Cvgd_FindIp1Idx>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Trouver l'index d'un ip1 dans une liste d'ip1
 *
 * Parametres :
 *  <Ip1>   : Paramètres de l'application
 *  <Lst>   : La référence verticale
 *  <Size>  : Header RPN
 *
 * Retour   : L'index de l'ip1 dans la liste ou -1 si pas trouvé
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int vgrid::Cvgd_FindIp1Idx(int Ip1,int *Lst,int Size) {
   int idx=0;
   while( Size-- ) {
      if( *Lst++ == Ip1 )
         return idx;
      ++idx;
   }

   return(-1);
}

int vgrid::Cvgd_print_desc(int sout, int convip) {
  int k, ip1, kind, my_int;
  char pres_S[] = " p,";
  if(! this->valid) {
      printf("In Cvgd_print_desc: vgrid structure is not valid\n");
      return(VGD_ERROR);
    }
    if(sout != -1 && sout != 6){
      printf("In Cvgd_print_desc : please implement sout option = %d\n",sout);
      return(VGD_ERROR);
    }
    if(convip == -1){
      strcpy(pres_S,"");
    }
    
    // Dump general descriptor information
    printf(" -- Vertical Grid Descriptor Information --\n");
    printf("   ip1 = %d\n   ip2 = %d\n", this->rec.ip1, this->rec.ip2);
    printf("-------------------------------------------------------\n");
    printf(" Vcode = %d\n",this->vcode);
    printf("   Descriptor Nomvar: %s\n",this->rec.nomvar);
    printf("   level kind = %d, level version = %d\n", this->kind, this->version);
    if( this->is_valid(ptop_8_valid) )
      printf("   ptop=%f Pa\n",this->ptop_8);
    if( this->is_valid(pref_8_valid) )
      printf("   pref=%f Pa\n",this->pref_8);
    if( this->is_valid(rcoef1_valid) )
      printf("   rcoef1=%f\n",this->rcoef1);
    if( this->is_valid(rcoef2_valid) )
      printf("   rcoef2=%f\n",this->rcoef2);
    if( this->is_valid( rcoef3_valid) && this->Cvgd_is_valid("ref_namel_valid" ) )
      printf("   rcoef3=%f\n",this->rcoef3);
    if( this->is_valid(rcoef4_valid) && this->Cvgd_is_valid("ref_namel_valid" ) )
      printf("   rcoef4=%f\n",this->rcoef4);
    if( this->Cvgd_is_valid("ref_name_valid") )
      printf("   Surface field nomvar %s\n",this->ref_name);
    if( this->Cvgd_is_valid("ref_namel_valid") )
      printf("   Surface field nomvar large scale %s\n",this->ref_namel);
    
    switch(this->vcode) {
    case 1:
      printf("   Number of height levels (momentum/Vertical-Velocity) %d\n",this->nk);
      printf("   Equation to compute heights z = A \n");
      break;
    case 1001:
      printf("   Number of sigma levels %d\n",this->nk);
      printf("   Equation to compute hydrostatic pressure (pi): pi = B * P0*100\n");
      break;
    case 1002:
      printf("   Number of eta levels %d\n", this->nl_m );
      break;
    case 2001:
      printf("   Number of pressure levels %d\n", this->nl_m );
      printf("   Equation to compute hydrostatic pressure (pi): pi = A\n");
      break;
    case 1003:
      printf("   Number of hybrid normalized levels %d\n", this->nl_m );
      printf("   Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100\n");
      break;
    case 4001:
      printf("   Number of heights levels %d (height with respect to ground level)\n", this->nl_m );
      printf("   Equation to compute heights (m): h = A\n");
      break;
    case 5001:
      printf("   Number of hybrid levels %d\n", this->nl_m );
      printf("   Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100\n");
      break;
    case 5999: 
      printf("   Number of hybrid unstaggered levels of unknown origin %d\n", this->nl_m );
      printf("   Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100\n");
      break;
    case 5002:
    case 5003:
    case 5004:
      printf("   Number of hybrid levels (momentum levels) %d\n", this->nl_m-1 );
      printf("   Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)\n");
      break;
    case 5005:
      printf("   Number of hybrid levels (momentum/thermo levels) %d\n", this->nl_m-2 );
      ip1=this->ip1_m[this->nl_m-1];
      printf("   Diagnostic momentum level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=this->ip1_t[this->nl_t-1];
      printf("   Diagnostic thermo   level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      printf("   Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)\n");
      break;
    case 5100:
      printf("   Number of hybrid levels (SLEVE momentum/thermo levels) %d\n", this->nl_m-2 );
      ip1=this->ip1_m[this->nl_m-1];
      printf("   Diagnostic momentum level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=this->ip1_t[this->nl_t-1];
      printf("   Diagnostic thermo   level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      printf("   Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref) + C * ln(P0LS*100/pref)\n");
      break;
    case 21001:
      if( this->Cvgd_is_valid("ref_namel_valid") ){
	printf("   Number of hybrid height levels (Gal-Chen) (SLEVE momentum/thermo levels) %d\n", this->nl_m-2 );
      } else {
	printf("   Number of hybrid height levels (Gal-Chen) (momentum/thermo levels) %d\n", this->nl_m-2 );
      }
      ip1=this->ip1_m[this->nl_m-1];
      printf("   Diagnostic momentum level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=this->ip1_t[this->nl_t-1];
      printf("   Diagnostic thermo   level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      if( this->Cvgd_is_valid("ref_namel_valid") ){
	printf("   Equation to compute heights z = A + B * ME + C * MELS\n");
      } else {
	printf("   Equation to compute heights z = A + B * ME\n");
      }
      break;
    case 21002:
      if( this->Cvgd_is_valid("ref_namel_valid") ){
	printf("   Number of hybrid height levels on Lorenz grid (SLEVE momentum-thermo/vertical-velocity levels) %d\n", this->nl_m-2 );
      } else {
	printf("   Number of hybrid height levels on Lorenz grid (momentum-thermo/vertical-velocity levels) %d\n", this->nl_m-2 );
      }
      ip1=this->ip1_m[this->nl_m-1];
      printf("   Diagnostic momentum level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=this->ip1_t[this->nl_t-1];
      printf("   Diagnostic thermo   level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=this->ip1_w[this->nl_w-1];
      printf("   Diagnostic Vertical-Velocity level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      if( this->Cvgd_is_valid("ref_namel_valid") ){
	printf("   Equation to compute heights z = A + B * ME + C * MELS\n");
      } else {
	printf("   Equation to compute heights z = A + B * ME\n");
      }
      break;
    default:
      printf("(Cvgd) ERROR in Cvgd_print_desc, invalid kind or version: kind=%d, version=%d\n",this->kind,this->version);
      return(VGD_ERROR);
    }

    if (this->is_valid(ip1_m_valid) ) {
      printf("   Momentum levels ip1,%s A, B",pres_S);
      if( this->is_valid( c_m_8_valid) && this->Cvgd_is_valid("ref_namel_valid" ) ) {
	printf(", C:\n");
      }else{
	printf(":\n");
      }
      for ( k = 0; k < this->nl_m; k++) {
	if(convip != -1){
	  printf("%12d %-# 25.15G %-# 25.15G %-# 25.15G",this->ip1_m[k],c_convip_IP2Level(this->ip1_m[k],&my_int),this->a_m_8[k],this->b_m_8[k]);
	} else {
	  printf("%12d %-# 25.15G %-# 25.15G",this->ip1_m[k],this->a_m_8[k],this->b_m_8[k]);
	}
	if( this->is_valid(c_m_8_valid) && this->Cvgd_is_valid("ref_namel_valid" ) ) {
	  printf(" %-# 25.15G\n",this->c_m_8[k]);
	}else{
	  printf("\n");
	}
      }
    }
    if (this->is_valid(ip1_t_valid) ) {
      printf("   Thermodynamic levels ip1,%s A, B",pres_S);
      if( this->is_valid(c_t_8_valid) && this->Cvgd_is_valid("ref_namel_valid" ) ) {
	printf(", C:\n");
      }else{
	printf(":\n");
      }
      for ( k = 0; k < this->nl_t; k++) {
	if(convip != -1){
	  printf("%12d %-# 25.15G %-# 25.15G %-# 25.15G",this->ip1_t[k],c_convip_IP2Level(this->ip1_t[k],&my_int),this->a_t_8[k],this->b_t_8[k]);
	} else {
	  printf("%12d %-# 25.15G %-# 25.15G",this->ip1_t[k],this->a_t_8[k],this->b_t_8[k]);
	}
	if( this->is_valid(c_t_8_valid) && this->Cvgd_is_valid("ref_namel_valid" ) ) {
	  printf(" %-# 25.15G\n",this->c_t_8[k]);
	}else{
	  printf("\n");
	}
      }
    }
    if (this->is_valid(ip1_w_valid) ) {
      printf("   Vertical-Velocity levels ip1,%s A, B",pres_S);
      if( this->is_valid(c_w_8_valid) && this->Cvgd_is_valid("ref_namel_valid") ) {
	printf(", C:\n");
      }else{
	printf(":\n");
      }
      for ( k = 0; k < this->nl_w; k++) {
	if(convip != -1){
	  printf("%12d %-# 25.15G %-# 25.15G %-# 25.15G",this->ip1_w[k],c_convip_IP2Level(this->ip1_w[k],&my_int),this->a_w_8[k],this->b_w_8[k]);
	} else {
	  printf("%12d %-# 25.15G %-# 25.15G",this->ip1_w[k],this->a_w_8[k],this->b_w_8[k]);
	}
	if( this->is_valid(c_w_8_valid) && this->Cvgd_is_valid("ref_namel_valid") ) {
	  printf(" %-# 25.15G\n",this->c_w_8[k]);
	}else{
	  printf("\n");
	}
      }
    }
    return(VGD_OK);
}

int vgrid::Cvgd_print_vcode_description(int vcode){
  
  // Create horizontal rule
  char *hr = {"-------------------------------------------------------"};

  if(vcode == 1001 || vcode == -1){
    printf("%s\nVcode 1001, kind 1, version 1\n",hr);
    printf("  Sigma levels\n");
  }
  if(vcode == 1002 || vcode == -1) {
    printf("%s\nVcode 1002, kind 1, version 2\n", hr);
    printf("   Eta levels\n");
  }
  if(vcode == 1003 || vcode == -1){
    printf("%s\nVcode 1003, kind 1, version 3\n", hr);
    printf("   Hybrid normalized levels\n");
  }
  if(vcode == 2001 || vcode==-1){
    printf("%s\nVcode 2001, kind 2, version 1\n", hr);
    printf("   Pressure levels\n");
  }
  if(vcode == 4001 || vcode == -1){
    printf("%s\nVcode 4001, kind 4, version 1\n", hr);
    printf("   Height with respect to ground level\n");
  }
  if(vcode == 5001 || vcode == -1){
    printf("%s\nVcode 5001, kind 5, version 1\n", hr);
    printf("   Hybrid levels, unstaggered\n");
  }
  if(vcode == 5002 || vcode==-1){
    printf("%s\nVcode 5002, kind 5, version 2\n", hr);
    printf("   Hybrid staggered levels, nk momentum levels, nk+1 thermo levels\n");
    printf("   First level at top is a thermo level\n");
  }
  if(vcode == 5003 || vcode==-1){
    printf("%s\nVcode 5003, kind 5, version 3\n", hr);
    printf("   Hybrid staggered levels, nk momentum levels, nk+1 thermo levels\n");
    printf("   First level at top is a thermo level\n");
    printf("   Last thermo level is unstaggered (tlift)\n");
  }
  if(vcode == 5004 || vcode == -1){
    printf("%s\nVcode 5004, kind 5, version 4\n", hr);
    printf("   Hybrid staggered levels, same number of momentum and themro levels\n");
    printf("   First level at top is a momentum level\n");
  }
  if(vcode == 5005 || vcode == -1){
      printf("%s\nVcode 5005, kind 5, version 5\n",  hr);
      printf("   Hybrid staggered levels, same number of momentum and themro levels\n");
      printf("   First level at top is a momentum level\n");
      printf("   Diag level heights (m AGL) encoded\n");
  }
  if(vcode == 5100 || vcode == -1){
      printf("%s\nVcode 5100, kind 5, version 100\n",  hr);
      printf("   Hybrid staggered SLEVE levels, same number of momentum and themro levels\n");
      printf("   The SLEVE coordinate needs surface reference fields:\n");
      printf("      P0   surface pressure\n");
      printf("      P0LS large scale surface pressure\n");
      printf("   First level at top is a momentum level\n");
      printf("   Diag level heights (m AGL) encoded\n");
  }
  if(vcode == 5999 || vcode == -1){
    printf("%s\nVcode 5999, kind 5, version 999\n",  hr);
    printf("   Hybrid unstaggered levels of unkown source\n");
    printf("   Can be used to encode any hybbrid unstaggered levels e.g. ECMWF\n");
  }
  if(vcode == 21001 || vcode == -1){
    printf("%s\nVcode 21001, kind 21, version 1\n",  hr);
    printf("   Hybrid height levels (Gal-Chen) (momentum/thermo levels) may be SLEVE\n");
    printf("   The coordinate needs surface reference fields:\n");
    printf("      ME   surface pressure\n");
    printf("     The SLEVE version also needs surface reference fields:\n");
    printf("      MELS large scale surface pressure\n");
    printf("   Diag level heights (m AGL) encoded\n");
  }
  if(vcode == 21002 || vcode == -1){
    printf("%s\nVcode 21002, kind 21, version 2\n",  hr);
    printf("   Hybrid height levels on Lorenz grid (momentum/thermo levels/vertical-velocity) may be SLEVE\n");
    printf("   The coordinate needs surface reference fields:\n");
    printf("      ME   surface pressure\n");
     printf("     The SLEVE version also needs surface reference fields:\n");
    printf("      MELS large scale surface pressure\n");
    printf("   Diag level heights (m AGL) encoded\n");
  }

  return(VGD_OK);

}

int vgrid::C_compute_heights_0001_8(int ni, int nj, int nk, int *ip1_list, double *levels) {
  char proc_name[] = "C_compute_heights_0001_8";
#define REAL_8 1
  double *aa_8;
  int ij, k, ijk, ind;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_w, this->nl_w) ) != -1 ) {
	aa_8[k] = this->a_w_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	return(VGD_ERROR);	
      }
    }
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
      levels[ijk] = aa_8[k];
#else
      levels[ijk] = (float) aa_8[k];
#endif
    }
  }

  free(aa_8);

  return(VGD_OK);
#undef REAL_8 
}

int vgrid::C_compute_heights_0001(int ni, int nj, int nk, int *ip1_list, float *levels) {
  char proc_name[] = "C_compute_heights_0001";
#undef REAL_8
  double *aa_8;
  int ij, k, ijk, ind;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_w, this->nl_w) ) != -1 ) {
	aa_8[k] = this->a_w_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	return(VGD_ERROR);	
      }
    }
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
      levels[ijk] = aa_8[k];
#else
      levels[ijk] = (float) aa_8[k];
#endif
    }
  }

  free(aa_8);

  return(VGD_OK);
}

int vgrid::C_compute_pressure_1001_1002_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log) {
  char proc_name[] = "C_compute_pressure_1001_1002_8";
#define REAL_8 1
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");

  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes  
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }    
  }
  
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]] + this->b_m_8[ind[k]] * sfc_field[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  return(VGD_OK);
#undef REAL_8 
}

int vgrid::C_compute_pressure_1001_1002(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log) {
  char proc_name[] = "C_compute_pressure_1001_1002";
#undef REAL_8
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");

  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes  
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }    
  }
  
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]] + this->b_m_8[ind[k]] * (double)sfc_field[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  return(VGD_OK);
}

int vgrid::C_compute_pressure_2001_8(int ni, int nj, int nk, int *ip1_list, double *levels, int in_log) {
  char proc_name[] = "C_compute_pressure_2001_8";
#define REAL_8 1
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");
  
if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }
  
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  return(VGD_OK);
#undef REAL_8
}
int vgrid::C_compute_pressure_2001(int ni, int nj, int nk, int *ip1_list, float *levels, int in_log) {
  char proc_name[] = "C_compute_pressure_2001";
#undef REAL_8
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");
  
if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }
  
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  return(VGD_OK);
}

int vgrid::C_compute_heights_4001_8(int ni, int nj, int nk, int *ip1_list, double *levels) {
  char proc_name[] = "C_compute_heights_4001_8";
#define REAL_8 1
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");
  
  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }
  
  // Compute heights
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]];
#if defined(REAL_8)
      levels[ijk] = lvl;
#else
      levels[ijk] = (float) lvl;
#endif
    }
  }
  free(ind);
  return(VGD_OK);
#undef REAL_8
}
int vgrid::C_compute_heights_4001(int ni, int nj, int nk, int *ip1_list, float *levels) {
  char proc_name[] = "C_compute_heights_4001";
#undef REAL_8
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");
  
  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }
  
  // Compute heights
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]];
#if defined(REAL_8)
      levels[ijk] = lvl;
#else
      levels[ijk] = (float) lvl;
#endif
    }
  }
  free(ind);
  return(VGD_OK);
}

int vgrid::C_compute_pressure_1003_5001_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log, int dpidpis ){
  char proc_name[] = "C_compute_pressure_1003_5001_8";
#define REAL_8 1
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");

  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }  
  if( dpidpis ){
    if(in_log){
      printf("(Cvgd) ERROR in %s, option in_log not allowed with option dpidpis\n", proc_name);
      return(VGD_ERROR);
    }
    for( k = 0, ijk=0; k < nk; ++k ){
      for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
#if defined(REAL_8)
	levels[ijk] = this->b_m_8[ind[k]];
#else
	levels[ijk] = (float) this->b_m_8[ind[k]];
#endif	
      }
    }    
    free(ind);
    return(VGD_OK);
  }
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]] + this->b_m_8[ind[k]] * sfc_field[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  
  return(VGD_OK);
#undef REAL_8
}

int vgrid::C_compute_pressure_1003_5001(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis ){
  char proc_name[] = "C_compute_pressure_1003_5001";
#undef REAL_8
  int k,*ind,ij,ijk;
  double lvl;
  char message[128];

  strcpy(message,"(Cvgd) ERROR in ");
  strcat(message,proc_name);
  strcat(message,", cannot allocate ind of int of size\n");

  if( my_alloc_int(&ind, nk, message) == VGD_ERROR )
    return(VGD_ERROR);
  
  // Find ip1 indexes
  for( k = 0; k < nk; ++k ){
    if( ( ind[k] = Cvgd_FindIp1Idx(ip1_list[k],this->ip1_m,this->nl_m)) == -1 ) {
      printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n",proc_name, ip1_list[k]);
      free(ind);
      return(VGD_ERROR);
    }
  }  
  if( dpidpis ){
    if(in_log){
      printf("(Cvgd) ERROR in %s, option in_log not allowed with option dpidpis\n", proc_name);
      return(VGD_ERROR);
    }
    for( k = 0, ijk=0; k < nk; ++k ){
      for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
#if defined(REAL_8)
	levels[ijk] = this->b_m_8[ind[k]];
#else
	levels[ijk] = (float) this->b_m_8[ind[k]];
#endif	
      }
    }    
    free(ind);
    return(VGD_OK);
  }
  // Compute pressure
  for( k = 0, ijk=0; k < nk; ++k ){
    for( ij = 0; ij < ni*nj; ++ij, ++ijk ){
      lvl = this->a_m_8[ind[k]] + this->b_m_8[ind[k]] * sfc_field[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? log(lvl) : lvl;
#else
      levels[ijk] = (float) (in_log ? log(lvl) : lvl);
#endif
    }
  }
  free(ind);
  
  return(VGD_OK);
}

int vgrid::C_compute_pressure_5002_5003_5004_5005_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log, int dpidpis) {
  char proc_name[] = "C_compute_pressure_5002_5003_5004_5005_8";
#define REAL_8 1
  double *aa_8, *bb_8, *s_8, lvl;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	free(bb_8);  	
	return(VGD_ERROR);	
      }
    }
  }
  s_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! s_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate s_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    return(VGD_ERROR);
  }
  for(ij=0; ij < ni*nj; ij++) {
    s_8[ij] = log(sfc_field[ij]/this->pref_8);
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
      lvl = aa_8[k] + bb_8[k]*s_8[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? lvl : exp(lvl);
#else
      levels[ijk] = (float) (in_log ? lvl : exp(lvl));
#endif
    }
  }
  //Force surface pressure to be equal to sfc_field
  //Needed by assimilation section.  
  if(! in_log) {
    for(k=0; k < nk; k++) {
      hyb = c_convip_IP2Level(ip1_list[k],&kind);
      if(fabs(hyb - 1.) < .000001 && kind == 5) {
  	ijk=k*ni*nj;
  	for(ij=0; ij < ni*nj; ij++, ijk++) {
  	  levels[ijk] = sfc_field[ij];
  	}
      }
    }
  }

  if( dpidpis ){
    if( in_log ){
      printf("(Cvgd) ERROR: in %s, cannot get dpidpis in log\n", proc_name);
      free(s_8);
      free(aa_8);
      free(bb_8);
      return(VGD_ERROR);
    }
    for(k=0, ijk=0; k < nk; k++) {
      for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
  	levels[ijk] = bb_8[k]*levels[ijk]/sfc_field[ij];
#else
	levels[ijk] = (float) bb_8[k]*levels[ijk]/sfc_field[ij];
#endif	
      }
    }
  }
  
  free(s_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
#undef REAL_8
}

int vgrid::C_compute_pressure_5002_5003_5004_5005(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis) {
  char proc_name[] = "C_compute_pressure_5002_5003_5004_5005";
#undef REAL_8
  double *aa_8, *bb_8, *s_8, lvl;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	free(bb_8);  	
	return(VGD_ERROR);	
      }
    }
  }
  s_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! s_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate s_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    return(VGD_ERROR);
  }
  for(ij=0; ij < ni*nj; ij++) {
    s_8[ij] = log(sfc_field[ij]/this->pref_8);
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
      lvl = aa_8[k] + bb_8[k]*s_8[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? lvl : exp(lvl);
#else
      levels[ijk] = (float) (in_log ? lvl : exp(lvl));
#endif
    }
  }
  //Force surface pressure to be equal to sfc_field
  //Needed by assimilation section.  
  if(! in_log) {
    for(k=0; k < nk; k++) {
      hyb = c_convip_IP2Level(ip1_list[k],&kind);
      if(fabs(hyb - 1.) < .000001 && kind == 5) {
  	ijk=k*ni*nj;
  	for(ij=0; ij < ni*nj; ij++, ijk++) {
  	  levels[ijk] = sfc_field[ij];
  	}
      }
    }
  }

  if( dpidpis ){
    if( in_log ){
      printf("(Cvgd) ERROR: in %s, cannot get dpidpis in log\n", proc_name);
      free(s_8);
      free(aa_8);
      free(bb_8);
      return(VGD_ERROR);
    }
    for(k=0, ijk=0; k < nk; k++) {
      for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
  	levels[ijk] = bb_8[k]*levels[ijk]/sfc_field[ij];
#else
	levels[ijk] = (float) bb_8[k]*levels[ijk]/sfc_field[ij];
#endif	
      }
    }
  }
  
  free(s_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
}

int vgrid::C_compute_pressure_5100_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, double *sfc_field_ls, int in_log, int dpidpis) {
  char proc_name[] = "C_compute_pressure_5100_8";
#define REAL_8 1
  double *aa_8, *bb_8, *cc_8, *s_8, *sl_8, lvl;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }
  cc_8 = (double*)malloc(nk*sizeof(double));
  if(! cc_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate cc_8 of bouble of size %d\n", proc_name, nk);
    free(cc_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
      cc_8[k] = this->c_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
	cc_8[k] = this->c_t_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	free(bb_8);  	
	free(cc_8);  	
	return(VGD_ERROR);	
      }
    }
  }
  s_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! s_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate s_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    free(cc_8);
    return(VGD_ERROR);
  }
  for(ij=0; ij < ni*nj; ij++) {
    s_8[ij] = log(sfc_field[ij]/this->pref_8);
  }
  sl_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! sl_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate sl_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    free(cc_8);
    free(s_8);
    return(VGD_ERROR);
  }
  if(dpidpis){
    for(ij=0; ij < ni*nj; ij++) {
      sl_8[ij] = 0.;
    }
  } else {
    for(ij=0; ij < ni*nj; ij++) {
      sl_8[ij] = log(sfc_field_ls[ij]/this->pref_8);
    }
  }
  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
      lvl = aa_8[k] + bb_8[k]*s_8[ij] + cc_8[k]*sl_8[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? lvl : exp(lvl);
#else
      levels[ijk] = (float) (in_log ? lvl : exp(lvl));
#endif
    }
  }
  //Force surface pressure to be equal to sfc_field
  //Needed by assimilation section.  
  if(! in_log) {
    for(k=0; k < nk; k++) {
      hyb = c_convip_IP2Level(ip1_list[k],&kind);
      if(fabs(hyb - 1.) < .000001 && kind == 5) {
  	ijk=k*ni*nj;
  	for(ij=0; ij < ni*nj; ij++, ijk++) {
  	  levels[ijk] = sfc_field[ij];
  	}
      }
    }
  }

  if( dpidpis ){
    if( in_log ){
      printf("(Cvgd) ERROR: in %s, cannot get dpidpis in log\n", proc_name);
      free(s_8);
      free(aa_8);
      free(bb_8);
      return(VGD_ERROR);
    }
    for(k=0, ijk=0; k < nk; k++) {
      for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
  	levels[ijk] = bb_8[k]*levels[ijk]/sfc_field[ij];
#else
	levels[ijk] = (float) bb_8[k]*levels[ijk]/sfc_field[ij];
#endif	
      }
    }
  }
  
  free(s_8);
  free(sl_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
#undef REAL_8
}

int vgrid::C_compute_pressure_5100(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log, int dpidpis) {
  char proc_name[] = "C_compute_pressure_5100";
#undef REAL_8
  double *aa_8, *bb_8, *cc_8, *s_8, *sl_8, lvl;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }
  cc_8 = (double*)malloc(nk*sizeof(double));
  if(! cc_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate cc_8 of bouble of size %d\n", proc_name, nk);
    free(cc_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
      cc_8[k] = this->c_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
	cc_8[k] = this->c_t_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	free(bb_8);  	
	free(cc_8);  	
	return(VGD_ERROR);	
      }
    }
  }
  s_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! s_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate s_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    free(cc_8);
    return(VGD_ERROR);
  }
  for(ij=0; ij < ni*nj; ij++) {
    s_8[ij] = log(sfc_field[ij]/this->pref_8);
  }
  sl_8 = (double*)malloc(ni*nj*sizeof(double));
  if(! sl_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate sl_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    free(cc_8);
    free(s_8);
    return(VGD_ERROR);
  }
  if(dpidpis){
    for(ij=0; ij < ni*nj; ij++) {
      sl_8[ij] = 0.;
    }
  } else {
    for(ij=0; ij < ni*nj; ij++) {
      sl_8[ij] = log(sfc_field_ls[ij]/this->pref_8);
    }
  }
  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
      lvl = aa_8[k] + bb_8[k]*s_8[ij] + cc_8[k]*sl_8[ij];
#if defined(REAL_8)
      levels[ijk] = in_log ? lvl : exp(lvl);
#else
      levels[ijk] = (float) (in_log ? lvl : exp(lvl));
#endif
    }
  }
  //Force surface pressure to be equal to sfc_field
  //Needed by assimilation section.  
  if(! in_log) {
    for(k=0; k < nk; k++) {
      hyb = c_convip_IP2Level(ip1_list[k],&kind);
      if(fabs(hyb - 1.) < .000001 && kind == 5) {
  	ijk=k*ni*nj;
  	for(ij=0; ij < ni*nj; ij++, ijk++) {
  	  levels[ijk] = sfc_field[ij];
  	}
      }
    }
  }

  if( dpidpis ){
    if( in_log ){
      printf("(Cvgd) ERROR: in %s, cannot get dpidpis in log\n", proc_name);
      free(s_8);
      free(aa_8);
      free(bb_8);
      return(VGD_ERROR);
    }
    for(k=0, ijk=0; k < nk; k++) {
      for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
  	levels[ijk] = bb_8[k]*levels[ijk]/sfc_field[ij];
#else
	levels[ijk] = (float) bb_8[k]*levels[ijk]/sfc_field[ij];
#endif	
      }
    }
  }
  
  free(s_8);
  free(sl_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
}

int vgrid::C_compute_heights_21001_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, double *sfc_field_ls) {
  char proc_name[] = "C_compute_heights_21001_8";
  double *my_sfc_field_ls;
#define REAL_8 1
  double *aa_8, *bb_8, *cc_8;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }
  cc_8 = (double*)malloc(nk*sizeof(double));
  if(! cc_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate cc_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    free(bb_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
      cc_8[k] = this->c_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
	cc_8[k] = this->c_t_8[ind];
      } else {
	if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_w, this->nl_w) ) != -1 ) {
	  aa_8[k] = this->a_w_8[ind];
	  bb_8[k] = this->b_w_8[ind];
	  cc_8[k] = this->c_w_8[ind];
	} else {
	  printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	  free(aa_8);
	  free(bb_8);
	  free(cc_8);
	  return(VGD_ERROR);	
	}
      }
    }
  }

  if( strcmp(this->ref_namel, VGD_NO_REF_NOMVAR) == 0 ){
    my_sfc_field_ls = sfc_field;
  } else {
    my_sfc_field_ls = sfc_field_ls;
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
      levels[ijk] = aa_8[k] + bb_8[k]*sfc_field[ij] + cc_8[k]*my_sfc_field_ls[ij];
#else
      levels[ijk] = (float) ( aa_8[k] + bb_8[k]*sfc_field[ij] + cc_8[k]*my_sfc_field_ls[ij] );
#endif
    }
  }
  //Force surface heights to be equal to sfc_field
  //Needed by assimilation section.  
  for(k=0; k < nk; k++) {
    hyb = c_convip_IP2Level(ip1_list[k],&kind);
    if(fabs(hyb) < .000001 && kind == 21) {
      ijk=k*ni*nj;
      for(ij=0; ij < ni*nj; ij++, ijk++) {
        levels[ijk] = sfc_field[ij];
      }
    }
  }

  free(aa_8);
  free(bb_8);
  free(cc_8);

  return(VGD_OK);
#undef REAL_8
}

int vgrid::C_compute_heights_21001(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls) {
  char proc_name[] = "C_compute_heights_21001";
  float *my_sfc_field_ls;
#undef REAL_8
  double *aa_8, *bb_8, *cc_8;
  int ij, k, ijk, ind, kind;
  float hyb;

  aa_8 = (double*)malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = (double*)malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }
  cc_8 = (double*)malloc(nk*sizeof(double));
  if(! cc_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate cc_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    free(bb_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_m, this->nl_m) ) != -1 ) {
      aa_8[k] = this->a_m_8[ind];
      bb_8[k] = this->b_m_8[ind];
      cc_8[k] = this->c_m_8[ind];
    } else {
      if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_t, this->nl_t) ) != -1 ) {
	aa_8[k] = this->a_t_8[ind];
	bb_8[k] = this->b_t_8[ind];
	cc_8[k] = this->c_t_8[ind];
      } else {
	if( (ind = Cvgd_FindIp1Idx( ip1_list[k], this->ip1_w, this->nl_w) ) != -1 ) {
	  aa_8[k] = this->a_w_8[ind];
	  bb_8[k] = this->b_w_8[ind];
	  cc_8[k] = this->c_w_8[ind];
	} else {
	  printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	  free(aa_8);
	  free(bb_8);
	  free(cc_8);
	  return(VGD_ERROR);	
	}
      }
    }
  }

  if( strcmp(this->ref_namel, VGD_NO_REF_NOMVAR) == 0 ){
    my_sfc_field_ls = sfc_field;
  } else {
    my_sfc_field_ls = sfc_field_ls;
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
#if defined(REAL_8)
      levels[ijk] = aa_8[k] + bb_8[k]*sfc_field[ij] + cc_8[k]*my_sfc_field_ls[ij];
#else
      levels[ijk] = (float) ( aa_8[k] + bb_8[k]*(double)sfc_field[ij] + cc_8[k]*(double)my_sfc_field_ls[ij] );
#endif
    }
  }
  //Force surface heights to be equal to sfc_field
  //Needed by assimilation section.  
  for(k=0; k < nk; k++) {
    hyb = c_convip_IP2Level(ip1_list[k],&kind);
    if(fabs(hyb) < .000001 && kind == 21) {
      ijk=k*ni*nj;
      for(ij=0; ij < ni*nj; ij++, ijk++) {
        levels[ijk] = sfc_field[ij];
      }
    }
  }

  free(aa_8);
  free(bb_8);
  free(cc_8);

  return(VGD_OK);
}

int vgrid::Cvgd_levels_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, int in_log) {
  if(this->Cvgd_diag_withref_2ref_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, NULL, in_log, 0) == VGD_ERROR )
    return(VGD_ERROR);
  return(VGD_OK);
}

int vgrid::Cvgd_levels(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log) {
  if(this->Cvgd_diag_withref_2ref(ni, nj, nk, ip1_list, levels, sfc_field, NULL, in_log, 0) == VGD_ERROR )
    return(VGD_ERROR);
  return(VGD_OK);
}

int vgrid::Cvgd_levels_2ref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8, int in_log) {
  if(this->Cvgd_diag_withref_2ref_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, sfc_field_ls_8, in_log, 0) == VGD_ERROR )
    return(VGD_ERROR);
  return(VGD_OK);
}

int vgrid::Cvgd_levels_2ref(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log) {
  if(this->Cvgd_diag_withref_2ref(ni, nj, nk, ip1_list, levels, sfc_field, sfc_field_ls, in_log, 0) == VGD_ERROR )
    return(VGD_ERROR);
  return(VGD_OK);
}

int vgrid::Cvgd_diag_withref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8,int in_log, int dpidpis) {
  if( this->Cvgd_diag_withref_2ref_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, NULL, in_log, dpidpis) ){
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int vgrid::Cvgd_diag_withref(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis) {
  if( this->Cvgd_diag_withref_2ref(ni, nj, nk, ip1_list, levels, sfc_field, NULL, in_log, dpidpis) ){
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int vgrid::Cvgd_diag_withref_2ref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8,int in_log, int dpidpis) {
  char proc_name[] = "Cvgd_diag_withref_2ref_8";
  char double_interface = 1;
  // The following pointers will never be used but they are needed to compile
  float *levels = NULL, *sfc_field = NULL, *sfc_field_ls = NULL;
#define REAL_8 1
  if(! this->Cvgd_is_valid("SELF")){
    printf("(Cvgd) ERROR in %s, invalid vgrid.\n",proc_name);
    return(VGD_ERROR);
  }
  
  switch(this->vcode) {
  case 1:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not supported for vertical coordinate 1\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_heights_0001_8(ni, nj, nk, ip1_list, levels_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_0001(ni, nj, nk, ip1_list, levels) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;    
  case 1001:
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 1002:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 1002\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 2001:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 2001\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_pressure_2001_8(ni, nj, nk, ip1_list, levels_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_2001(ni, nj, nk, ip1_list, levels, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 4001:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 4001\n");
      return(VGD_ERROR);
    }
    if( in_log ){
      printf("(Cvgd) ERROR: option in_log not supported for vertical coordinate 4001\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_heights_4001_8(ni, nj, nk, ip1_list, levels_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_4001(ni, nj, nk, ip1_list, levels) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 1003:
  case 5001:
    if(double_interface){
      if( C_compute_pressure_1003_5001_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log, dpidpis) == VGD_ERROR )
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1003_5001(ni, nj, nk, ip1_list, levels, sfc_field, in_log, dpidpis) == VGD_ERROR )
	return(VGD_ERROR);
    }
    break;
  case 5002:
  case 5003:
  case 5004:
  case 5005:
    if(double_interface){
      if( C_compute_pressure_5002_5003_5004_5005_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_5002_5003_5004_5005(ni, nj, nk, ip1_list, levels, sfc_field, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 5100:
    if(double_interface){
      if( C_compute_pressure_5100_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, sfc_field_ls_8, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_5100(ni, nj, nk, ip1_list, levels, sfc_field, sfc_field_ls, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 5999:
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 21001:
  case 21002:
    if(double_interface){
      if( C_compute_heights_21001_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, sfc_field_ls_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_21001(ni, nj, nk, ip1_list, levels, sfc_field, sfc_field_ls) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  default:
    printf("(Cvgd) ERROR in %s, invalid kind or version: kind = %d, version = %d\n", proc_name, this->kind, this->version);
    return(VGD_ERROR);
  }
  
  return(VGD_OK);
#undef REAL_8
}

int vgrid::Cvgd_diag_withref_2ref(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log, int dpidpis) {
  char proc_name[] = "Cvgd_diag_withref_2ref";
  char double_interface = 0;
  // The following pointers will never be used but they are needed to compile
  double *levels_8 = NULL, *sfc_field_8 = NULL, *sfc_field_ls_8 = NULL;
#undef REAL_8
  if(! this->Cvgd_is_valid("SELF")){
    printf("(Cvgd) ERROR in %s, invalid vgrid.\n",proc_name);
    return(VGD_ERROR);
  }
  
  switch(this->vcode) {
  case 1:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not supported for vertical coordinate 1\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_heights_0001_8(ni, nj, nk, ip1_list, levels_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_0001(ni, nj, nk, ip1_list, levels) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;    
  case 1001:
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 1002:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 1002\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 2001:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 2001\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_pressure_2001_8(ni, nj, nk, ip1_list, levels_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_2001(ni, nj, nk, ip1_list, levels, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 4001:
    if( dpidpis ){
      printf("(Cvgd) ERROR: dpidpis not implemented for vertical coordinate 4001\n");
      return(VGD_ERROR);
    }
    if( in_log ){
      printf("(Cvgd) ERROR: option in_log not supported for vertical coordinate 4001\n");
      return(VGD_ERROR);
    }
    if(double_interface){
      if( C_compute_heights_4001_8(ni, nj, nk, ip1_list, levels_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_4001(ni, nj, nk, ip1_list, levels) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 1003:
  case 5001:
    if(double_interface){
      if( C_compute_pressure_1003_5001_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log, dpidpis) == VGD_ERROR )
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1003_5001(ni, nj, nk, ip1_list, levels, sfc_field, in_log, dpidpis) == VGD_ERROR )
	return(VGD_ERROR);
    }
    break;
  case 5002:
  case 5003:
  case 5004:
  case 5005:
    if(double_interface){
      if( C_compute_pressure_5002_5003_5004_5005_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_5002_5003_5004_5005(ni, nj, nk, ip1_list, levels, sfc_field, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 5100:
    if(double_interface){
      if( C_compute_pressure_5100_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, sfc_field_ls_8, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_5100(ni, nj, nk, ip1_list, levels, sfc_field, sfc_field_ls, in_log, dpidpis) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 5999:
    if(double_interface){
      if( C_compute_pressure_1001_1002_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_pressure_1001_1002(ni, nj, nk, ip1_list, levels, sfc_field, in_log) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  case 21001:
  case 21002:
    if(double_interface){
      if( C_compute_heights_21001_8(ni, nj, nk, ip1_list, levels_8, sfc_field_8, sfc_field_ls_8) == VGD_ERROR)
	return(VGD_ERROR);
    } else {
      if( C_compute_heights_21001(ni, nj, nk, ip1_list, levels, sfc_field, sfc_field_ls) == VGD_ERROR)
	return(VGD_ERROR);
    }
    break;
  default:
    printf("(Cvgd) ERROR in %s, invalid kind or version: kind = %d, version = %d, vcode=%d\n", proc_name, this->kind, this->version, this->vcode);
    return(VGD_ERROR);
  }
  
  return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <c_vgd_construct>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Initialise et retourne une structure de type vgrid_descriptor
 *
 * Parametres :
 *
 * Retour   : Une structure initialisée de type vgrid_descriptor
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */

vgrid::vgrid(int ip1, int ip2)
{
  rec.ip1    = (int) fmax(0,ip1);
  rec.ip2    = (int) fmax(0,ip2);

  ptop_8        = VGD_MISSING;
  pref_8        = VGD_MISSING;  
  table         = NULL;
  table_ni      = 0;
  table_nj      = 0;
  table_nk      = 1;
  a_m_8         = NULL;
  b_m_8         = NULL;
  c_m_8         = NULL;
  a_t_8         = NULL;
  b_t_8         = NULL;
  c_t_8         = NULL;
  a_w_8         = NULL;
  b_w_8         = NULL;
  c_w_8         = NULL;
  ip1_m         = NULL;
  ip1_t         = NULL;  
  ip1_w         = NULL;  
  nl_m          = 0;
  nl_t          = 0;
  nl_w          = 0;
  dhm           = VGD_MISSING;
  dht           = VGD_MISSING;
  dhw           = VGD_MISSING;
  
  ref_name      = strdup(VGD_NO_REF_NOMVAR);
  ref_namel     = strdup(VGD_NO_REF_NOMVAR);
  rcoef1        = VGD_MISSING;
  rcoef2        = VGD_MISSING;
  rcoef3        = VGD_MISSING;
  rcoef4        = VGD_MISSING;
  nk            = 0;
  ip1           = 0;
  ip2           = 0;
  unit          = 0;
  vcode         = 0;
  kind          = 0;
  version       = 0;
  match_ipig    = 0;
  valid         = 0;
  skip          = 0;
  k_plus_top    = 0;

  rec.fstd_initialized = 0;
  rec.dateo = 0;
  rec.deet = 0;
  rec.npas = 0;
  rec.nbits = -64;
  rec.datyp = 0;
  rec.ip1 = 0;
  rec.ip2 = 0;
  rec.ip3 = 0;
  rec.ig1 = 0;
  rec.ig2 = 0;
  rec.ig3 = 0;
  rec.ig4 = 0;
  strcpy(rec.typvar,"  ");
  strcpy(rec.nomvar,"    ");
  strcpy(rec.etiket,"            ");
  strcpy(rec.grtyp," ");
}

int vgrid::allocate_table(int nk)
{
  int table_size;

  set_table_nj(nk);

  //if( this->table )
  //  free( this->table );
  table_size = this->table_ni * this->table_nj * this->table_nk;
  this->table = (double*)malloc ( table_size * sizeof(double) );
  if(! this->table )
  {
    printf("(Cvgd) ERROR in allocate_table, cannot allocate table of double of size %d\n",table_size );
    return(VGD_ERROR);
  }

  return(VGD_OK);
}

void vgrid::c_vgd_free_abci() {
    // Thermo pointers may be pointing to momentum or velocity for certain Vcode, only nullify them if this is the case.    
    if( this->a_t_8 == this->a_m_8 || this->a_t_8 == this->a_w_8 ) {
      this->a_t_8 = NULL;
    } else {
      FREE(this->a_t_8);
    }
    if( this->b_t_8 == this->b_m_8 || this->b_t_8 == this->b_w_8 ) {
      this->b_t_8 = NULL;
    } else {
      FREE(this->b_t_8);
    }
    if( this->c_t_8 == this->c_m_8 || this->c_t_8 == this->c_w_8 ) {
      this->c_t_8 = NULL;
    } else {
      FREE(this->c_t_8);
    }
    if( this->ip1_t == this->ip1_m || this->ip1_t == this->ip1_w ) {
      this->ip1_t = NULL;
    } else {
      FREE(this->ip1_t);
    }

    // Velocity pointers may be pointing to momentum or thermo for certain Vcode, only nullify them if this is the case.    
    if( this->a_w_8 == this->a_m_8 || this->a_w_8 == this->a_t_8 ) {
      this->a_w_8 = NULL;
    } else {
      FREE(this->a_w_8);
    }
    if( this->b_w_8 == this->b_m_8 || this->b_w_8 == this->b_t_8 ) {
      this->b_w_8 = NULL;
    } else {
      FREE(this->b_w_8);
    }
    if( this->c_w_8 == this->c_m_8 || this->c_w_8 == this->c_t_8 ) {
      this->c_w_8 = NULL;
    } else {
      FREE(this->c_w_8);
    }
    if( this->ip1_w == this->ip1_m || this->ip1_w == this->ip1_t ) {
      this->ip1_t = NULL;
    } else {
      FREE(this->ip1_w);
    }
    
    FREE(this->a_m_8);
    FREE(this->b_m_8);
    FREE(this->ip1_m);
    FREE(this->c_m_8);
}



int vgrid::allocate_and_fill_table(int nk)
{
  // Fill in the table (encode the vertical co-ordinate)
  if(this->allocate_table(nk) == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in allocate_and_fill_table, problem with allocate_table for vcode=%d\n",this->vcode);
    return(VGD_ERROR);
  }
  this->set_refnames();
  if(this->c_encode_vert() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in allocate_and_fill_table, problem with c_encode_vert for vcode=%d\n",this->vcode);
    return(VGD_ERROR);
  }


  this->valid = 1;
  if(this->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in allocate_and_fill_table, problem with fstd_init\n");
  }

  return(VGD_OK);
}

int vgrid::Cvgd_build_from_table(double *table, int ni, int nj, int nk)
{
  // N.B.:  'this' must be a SUBCLASS of vgrid

  int table_size, i;
  double *ltable;

  // Coordinate constructor - build vertical descriptor from table input
  // Set internal vcode (if all above was successful)

  this->valid = 0;

  // V V V V V   THIS SHOULD NO LONGER BE NECESSARY   V V V V V
  // Since table passed in argument may be the this->table, we take a copy before the call to free
  table_size = ni * nj * nk;
  ltable = (double*)malloc ( table_size * sizeof(double) );
  if(! ltable ) {
    printf("(Cvgd) ERROR in Cvgd_build_from_table, cannot allocate ltable of bouble of size %d\n", table_size);
    return(VGD_ERROR);
  }
  my_copy_double(table, &ltable, table_size);
  // ^ ^ ^ ^ ^  THIS SHOULD NO LONGER BE NECESSARY   ^ ^ ^ ^ ^


  this->table_ni = ni;
  this->table_nj = nj;
  this->table_nk = nk;

  this->table = (double*)malloc ( ni * nj * nk * sizeof(double) );
  if(! this->table ) {
    printf("(Cvgd) ERROR in Cvgd_build_from_table, cannot allocate table of double of size %d\n",table_size );
    return(VGD_ERROR);
  }
  for(i = 0; i < table_size; i++) {
    this->table[i] = ltable[i];
  }
  free(ltable);

  // Fill remainder of structure
  if( this->c_decode_vert() == VGD_ERROR )
  {
    printf("(Cvgd) in Cvgd_build_from_table, problem decoding table with vcode %d\n", this->vcode);
    return(VGD_ERROR);
  }

  this->valid = 1;
  if(this->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_build_from_table, problem creating record information\n");
  }

  return(VGD_OK);
}

void vgrid::Cvgd_free() {
// Avoid crashing:  don't free
//    if( *self ) {
//       FREE((*self)->table);
//       this->c_vgd_free_abci(self);
//       FREE((*self)->ref_name);      
//       FREE((*self)->ref_namel);      
//       free(*self);
//       *self = NULL;
//    }
}

/*----------------------------------------------------------------------------
 * Nom      : <Cvgd_set_vcode_i>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Set and check the vertical code
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *    <Kind>   : Kind of the vertical coord
 *    <Version>: Version of the vertical coord
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int vgrid::Cvgd_set_vcode_i(int Kind,int Version) {

   if( Kind>MAX_VKIND || Kind<0 || Version>999 || Version<0 ) {
      fprintf(stderr,"(Cvgd) ERROR in Cvgd_set_vcode_i, invalid kind or version kind=%d, version=%d\n",Kind,Version);
      return(VGD_ERROR);
   }
   this->vcode = Kind*1000 + Version;
   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Cvgd_set_vcode>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Set and check the vertical code
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int vgrid::Cvgd_set_vcode() {

   if( !this->table ) {
      fprintf(stderr,"(Cvgd) ERROR: Cvgd_set_vcode called before constructor\n");
      return(VGD_ERROR);
   }

   return this->Cvgd_set_vcode_i(this->kind,this->version);
}

/*----------------------------------------------------------------------------
 * Nom      : <fstd_init>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Initialize common elements of the fstd record
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int vgrid::fstd_init()
{
   VGD_TFSTD *h = &this->rec;

   if( h->fstd_initialized )
      return(VGD_OK);

   h->ig2 = h->ig3 = h->ig4 = 0;

   fstd_subinit();  // subclass-specific assignments

   strcpy(h->nomvar,"!!");
   strcpy(h->typvar,"X");
   strcpy(h->grtyp,"X");

   h->dateo       = 0;
   h->deet        = 0;
   h->npas        = 0;
   h->datyp       = 5;
   h->nbits       = 64;
   h->ip3         = 0;
   h->ig1         = this->vcode;
   h->fstd_initialized = 1;

   return(VGD_OK);
}

void vgrid::set_refnames()
{
  // This method allows a subclass to set the values for this->ref_name and
  // this->ref_namel during Cvgd_create_from_ab()
}

int vgrid::Cvgd_getopt_int(char *key, int *value, int quiet)
{
  if(! value){
    printf("(Cvgd) ERROR in Cvgd_getopt_int, value is a NULL pointer\n");
    return(VGD_ERROR);
  }
  if (strcmp(key, "ALLOW_SIGMA") == 0){
      *value = ALLOW_SIGMA;
  } else {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_getopt_int, invalid key %s\n",key);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }  
  
  return(VGD_OK);
}

int vgrid::Cvgd_get_int(char *key, int *value, int quiet)
{  
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_int, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if(! value){
    printf("(Cvgd) ERROR in Cvgd_get_int, value is a NULL pointer\n");
    return(VGD_ERROR);
  }
  *value = VGD_MISSING;
  if (strcmp(key, "NL_M") == 0){
    *value = this->nl_m;
  } else if (strcmp(key, "NL_T") == 0){
    *value = this->nl_t;
  } else if (strcmp(key, "NL_W") == 0){
    *value = this->nl_w;
  } else if (strcmp(key, "KIND") == 0){
    *value = this->kind;
  } else if (strcmp(key, "VERS") == 0){
    *value = this->version;
  } else if (strcmp(key, "VCOD") == 0){
    *value = this->vcode;
  } else if( strcmp(key, "DATE") == 0){
    *value = this->rec.dateo;
  } else if (strcmp(key, "IG_1") == 0){
    *value = this->rec.ig1;
  } else if (strcmp(key, "IG_2") == 0){
    *value = this->rec.ig2;
  } else if (strcmp(key, "IG_3") == 0){
    *value = this->rec.ig3;
  } else if (strcmp(key, "IG_4") == 0){
    *value = this->rec.ig4;
  } else if (strcmp(key, "IP_1") == 0){
    *value = this->rec.ip1;
  } else if (strcmp(key, "IP_2") == 0){
    *value = this->rec.ip2;
  } else if (strcmp(key, "IP_3") == 0){
    *value = this->rec.ip3;
  } else if (strcmp(key, "DIPM") == 0){
    if( ! this->Cvgd_is_valid("dhm_valid") ){      
      if(! quiet) {
	printf("(Cvgd) ERROR in Cvgd_get_int, cannot get key %s\n",key);
	fflush(stdout);
      }
      return(VGD_ERROR);
    }
    *value = this->ip1_m[this->nl_m -1];
  } else if (strcmp(key, "DIPT") == 0){
    if( ! this->Cvgd_is_valid("dht_valid") ){      
      if(! quiet) {
	printf("(Cvgd) ERROR in Cvgd_get_int, cannot get key %s\n",key);
	fflush(stdout);
      }
      return(VGD_ERROR);
    }
    *value = this->ip1_t[this->nl_t -1];
  } else if (strcmp(key, "DIPW") == 0){
    if( ! this->Cvgd_is_valid("dhw_valid") ){      
      if(! quiet) {
	printf("(Cvgd) ERROR in Cvgd_get_int, cannot get key %s\n",key);
	fflush(stdout);
      }
      return(VGD_ERROR);
    }
    *value = this->ip1_w[this->nl_w -1];
  } else if (strcmp(key, "MIPG") == 0){
    *value = this->match_ipig;
  } else if (strcmp(key, "LOGP") == 0){
    *value = this->is_valid(is_in_logp);
  } else {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_int, invalid key %s\n",key);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }
  
  return(VGD_OK);

}

int vgrid::Cvgd_get_int_1d(char *key, int **value, int *nk, int quiet)
{
  int OK = 1;
  if(nk) *nk = -1;
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_int_1d, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  // ====
  // VIP1
  // ----
  if(strcmp(key, "VIP1") == 0 ){
    if( this->is_valid(ip1_m_valid) ){
      printf("(Cvgd) ERROR in Cvgd_get_int_1d, depricated key '%s' use VIPM instead.\n", key);
      fflush(stdout);
      return(VGD_ERROR);
    } else {
      OK = 0;
    }
  }
  if( strcmp(key, "VIPM") == 0 ){
    // ====
    // VIPM
    // ----
    if( this->is_valid(ip1_m_valid) ){
      if(! *value){
        (*value) = (int*)malloc(this->nl_m * sizeof(int));
	if(! *value){
	  printf("(Cvgd) ERROR in Cvgd_get_int_1d, problem allocating %d int\n",this->nl_m);
	  return(VGD_ERROR);
	}
      }
      my_copy_int(this->ip1_m, value, this->nl_m);
      if(nk) *nk = this->nl_m;
    } else {
      OK = 0;
    }    
  } else if( strcmp(key, "VIPT") == 0 ){
    // ====
    // VIPT
    // ----  
    if(! *value){
      (*value) = (int*)malloc(this->nl_t * sizeof(int));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_int_1d, problem allocating %d int\n",this->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_int(this->ip1_t, value, this->nl_t);
    if(nk) *nk = this->nl_t;
  } else if( strcmp(key, "VIPW") == 0 ){
    // ====
    // VIPW
    // ----
    if(! *value){
      (*value) = (int*)malloc(this->nl_w * sizeof(int));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_int_1d, problem allocating %d int\n",this->nl_w);
	return(VGD_ERROR);
      }
    }
    my_copy_int(this->ip1_w, value, this->nl_w);
    if(nk) *nk = this->nl_w;
  }else{
    OK = 0;
  }
  if(! OK) {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_int_1d, invalid key '%s' for Vcode %d\n",key, this->vcode);
      fflush(stdout);
    }
    return(VGD_ERROR);    
  }
  return(VGD_OK);

}

int vgrid::Cvgd_get_float(char *key, float *value, int quiet) {

  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_float, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if(! value){
    printf("(Cvgd) ERROR in Cvgd_get_float, value is a NULL pointer\n");
    return(VGD_ERROR);
  }  

  if( strcmp(key, "RC_1" ) == 0 ){    
    if( this->is_valid(rcoef1_valid) ){
      *value = this->rcoef1;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "RC_2" ) == 0 ){
    if( this->is_valid(rcoef2_valid) ){
      *value = this->rcoef2;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "RC_3" ) == 0 ){
    if( this->is_valid(rcoef3_valid) ){
      *value = this->rcoef3;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "RC_4" ) == 0 ){
    if( this->is_valid(rcoef4_valid) ){
      *value = this->rcoef4;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "DHM " ) == 0 ){
    if( this->is_valid(dhm_valid) ){
      *value = this->dhm;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "DHT " ) == 0 ){
    if( this->is_valid(dht_valid) ){
      *value = this->dht;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else  if( strcmp(key, "DHW " ) == 0 ){
    if( this->is_valid(dhw_valid) ){
      *value = this->dhw;
    } else {
      *value = (float) c_get_error(key,quiet);
    }
  } else {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_float, invalid key '%s'\n",key);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }
  return(VGD_OK);

}

int vgrid::Cvgd_get_float_1d(char *key, float **value, int *nk, int quiet)
{
  char key2[5];
  int *vip1=NULL, kind, k, OK = 1;
  if(nk) *nk = -1;
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_float_1d, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "VCDM") == 0 ){
    if (this->is_valid(ip1_m_valid)) {
      if(! *value){
        (*value) = (float*)malloc(this->nl_m * sizeof(float));
	if(! *value){
	  printf("(Cvgd) ERROR in Cvgd_get_float_1d, problem allocating %d double\n",this->nl_m);
	  return(VGD_ERROR);
	}
      }    
      strcpy(key2,"VIPM");
      this->Cvgd_get_int_1d(key2, &vip1, NULL, quiet);
      for(k = 0; k < this->nl_m; k++){
	(*value)[k] = c_convip_IP2Level(vip1[k], &kind);
      }
      free(vip1);
      if(nk) *nk = this->nl_m;
    } else {
      OK = 0;
    }
  } else if( strcmp(key, "VCDT") == 0 ){
    if(! *value){
      (*value) = (float*)malloc(this->nl_t * sizeof(float));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_float_1d, problem allocating %d double\n",this->nl_t);
	return(VGD_ERROR);
      }
    }  
    strcpy(key2,"VIPT");
    this->Cvgd_get_int_1d(key2, &vip1, NULL, quiet);
    for(k = 0; k < this->nl_t; k++){
      (*value)[k] = c_convip_IP2Level(vip1[k], &kind);
    }    
    free(vip1);
    if(nk) *nk = this->nl_t;
  } else if( strcmp(key, "VCDW") == 0 ){
    if(! *value){
      (*value) = (float*)malloc(this->nl_w * sizeof(float));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_float_1d, problem allocating %d double\n",this->nl_w);
	return(VGD_ERROR);
      }
    }  
    strcpy(key2,"VIPW");
    this->Cvgd_get_int_1d(key2, &vip1, NULL, quiet);
    for(k = 0; k < this->nl_w; k++){
      (*value)[k] = c_convip_IP2Level(vip1[k], &kind);
    }    
    free(vip1);
    if(nk) *nk = this->nl_w;
  } else {
    OK = 0;
  }
  if(! OK){
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_float_1d, invalid key '%s' for vcode %d.\n",key, this->vcode);
      fflush(stdout);
    }
    return(VGD_ERROR);    
  }
  return(VGD_OK);
}

int vgrid::Cvgd_get_double(char *key, double *value_get, int quiet) {
  int OK = 1;
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_double, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "PTOP") == 0 ) {
    if(! this->is_valid(ptop_8_valid)) OK = 0;
    *value_get = this->ptop_8;
  } else if ( strcmp(key, "PREF") == 0 ) {
    if(! this->is_valid(pref_8_valid)) OK = 0;
    *value_get = this->pref_8;
  } else if ( strcmp(key, "RC_1") == 0 ) {
    if(! this->is_valid(rcoef1_valid)) OK = 0;
    *value_get = this->rcoef1;
  } else if ( strcmp(key, "RC_2") == 0 ) {
    if(! this->is_valid(rcoef2_valid)) OK = 0;
    *value_get = this->rcoef2;
  } else if ( strcmp(key, "RC_3") == 0 ) {
    if(! this->is_valid(rcoef3_valid)) OK = 0;
    *value_get = this->rcoef3;
  } else if ( strcmp(key, "RC_4") == 0 ) {
    if(! this->is_valid(rcoef4_valid)) OK = 0;
    *value_get = this->rcoef4;
  } else {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_double, invalid key '%s'\n", key);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }
  
  if(! OK) {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_double, %s cannot get for Vcode %d\n", key, this->vcode);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }    
  
  return(VGD_OK);
}

int vgrid::Cvgd_get_double_1d(char *key, double **value, int *nk, int quiet)
{
  int OK = 1;
  if(nk) *nk = -1;
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_double_1d, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "CA_M") == 0 || strcmp(key, "COFA") == 0 ){
    //=====
    // CA_M
    //-----
    if( this->is_valid(a_m_8_valid) ) {
      if(! *value){
	(*value) = (double*)malloc(this->nl_m * sizeof(double));
	if(! *value){
	  printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CA_M\n",this->nl_m);
	  return(VGD_ERROR);
	}
      }
      my_copy_double(this->a_m_8, value, this->nl_m);
      if(nk) *nk = this->nl_m;
    } else {
      OK = 0;
    }
  } else if( strcmp(key, "CB_M") == 0 || strcmp(key, "COFB") == 0 ) {
    //=====
    // CB_M
    //-----
    if( this->is_valid(b_m_8_valid) ) {
      if(! *value){
	(*value) = (double*)malloc(this->nl_m * sizeof(double));
	if(! *value){
	  printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CB_M\n",this->nl_m);
	  return(VGD_ERROR);
	}
      }
      my_copy_double(this->b_m_8, value, this->nl_m);
      if(nk) *nk = this->nl_m;
    } else {
      OK = 0;
    }
  } else if( strcmp(key, "CC_M") == 0 ) {
    //=====
    // CC_M
    //-----
    if( this->is_valid(c_m_8_valid) ) {
      if(! *value){
	(*value) = (double*)malloc(this->nl_m * sizeof(double));
	if(! *value){
	  printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CC_M\n",this->nl_m);
	  return(VGD_ERROR);
	}
      }
      my_copy_double(this->c_m_8, value, this->nl_m);
      if(nk) *nk = this->nl_m;
    } else {
      OK = 0;
    }
  } else if( strcmp(key, "CA_T") == 0 ){
    //=====
    // CA_T
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_t * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CA_T\n",this->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->a_t_8, value, this->nl_t);
    if(nk) *nk = this->nl_t;
  } else if( strcmp(key, "CB_T") == 0 ){
    //=====
    // CB_T
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_t * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CB_T\n",this->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->b_t_8, value, this->nl_t);
    if(nk) *nk = this->nl_t;
  } else if( strcmp(key, "CC_T") == 0 ){
    //=====
    // CC_T
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_t * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CC_T\n",this->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->c_t_8, value, this->nl_t);
    if(nk) *nk = this->nl_t;
  } else if( strcmp(key, "CA_W") == 0 ){
    //=====
    // CA_W
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_w * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CA_W\n",this->nl_w);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->a_w_8, value, this->nl_w);
    if(nk) *nk = this->nl_w;
  } else if( strcmp(key, "CB_W") == 0 ){
    //=====
    // CB_W
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_w * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CB_W\n",this->nl_w);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->b_w_8, value, this->nl_w);
    if(nk) *nk = this->nl_w;
   } else if( strcmp(key, "CC_W") == 0 ){
    //=====
    // CC_W
    //-----
    if(! *value){
      (*value) = (double*)malloc(this->nl_w * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_1d, problem allocating %d double for CC_W\n",this->nl_w);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->c_w_8, value, this->nl_w);
    if(nk) *nk = this->nl_w;
  } else {
    //============
    // Invalid key
    //------------
    OK = 0;
  }    
  if( ! OK) {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_double_1d, invalid key '%s' for vcode %d\n", key, this->vcode);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }

  return(VGD_OK);

}

int vgrid::Cvgd_get_double_3d(char *key, double **value, int *ni, int *nj, int *nk, int quiet)
{
  if(ni) *ni = -1;
  if(nj) *nj = -1;
  if(nk) *nk = -1;    
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_double_3d, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  int table_size = this->table_ni * this->table_nj * this->table_nk;
  if( strcmp(key, "VTBL") == 0 ){
    if(! *value){
      (*value) = (double*)malloc( table_size * sizeof(double));
      if(! *value){
	printf("(Cvgd) ERROR in Cvgd_get_double_3d, problem allocating %d double.\n",table_size);
	return(VGD_ERROR);
      }
    }
    my_copy_double(this->table, value, table_size);
    if(ni) *ni = this->table_ni;
    if(nj) *nj = this->table_nj;
    if(nk) *nk = this->table_nk;
  } else {
    if(! quiet) {
      printf("(Cvgd) ERROR in Cvgd_get_double_3d, invalid key '%s'\n",key);
      fflush(stdout);
    }
    return(VGD_ERROR);
  }

  return(VGD_OK);
}

int vgrid::Cvgd_get_char(char *key, char out[], int quiet) {
  char ok = 1;
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_get_char, invalid vgrid structure.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "ETIK") == 0 ){
    strcpy(out,this->rec.etiket);
  } else if( strcmp(key, "NAME") == 0 ){
    strcpy(out,this->rec.nomvar);
  } else if( strcmp(key, "RFLD") == 0 ){
    if( this->Cvgd_is_valid("ref_name_valid") ){
      strcpy(out,this->ref_name);
    } else {
      ok = 0;
      strcpy(out,VGD_NO_REF_NOMVAR);
    }
  } else if( strcmp(key, "RFLS") == 0 ){
    if( this->Cvgd_is_valid("ref_namel_valid") ){
      strcpy(out,this->ref_namel);
    } else {
      ok = 0;
      strcpy(out,VGD_NO_REF_NOMVAR);
    }
  } else {
    ok = 0;
  }
  if(! ok ){
    if(! quiet){
      printf("(Cvgd) ERROR in Cvgd_get_char, invalid key -> '%s'\n",key);
    }
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int vgrid::Cvgd_put_char(char *key, char *value) {
  // if(! this->Cvgd_is_valid("SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_put_char, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "ETIK") == 0 ){
    strcpy(this->rec.etiket,value);
  } else {
    printf("(Cvgd) ERROR in Cvgd_put_char, invalid key -> '%s'\n",key);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int vgrid::Cvgd_putopt_int(char *key, int value) {
  if( strcmp(key, "ALLOW_SIGMA") == 0 ) {
    ALLOW_SIGMA = value;
  } else {
    printf("(Cvgd) ERROR in Cvgd_putopt_int, invalid key %s\n", key);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
    
int vgrid::Cvgd_put_int(char *key, int value) {
  int kind;
  
  // if(! this->Cvgd_is_valid(,"SELF")){
  //   printf("(Cvgd) ERROR in Cvgd_put_int, invalid vgrid.\n");
  //   return(VGD_ERROR);
  // }
  if( strcmp(key, "DATE") == 0 ) {
    this->rec.dateo = value;
  } else if( strcmp(key, "IG_1") == 0 ) {
    this->rec.ig1 = value;
  } else if( strcmp(key, "IG_2") == 0 ) {
    this->rec.ig2 = value;
  } else if( strcmp(key, "IG_3") == 0 ) {
    this->rec.ig3 = value;
  } else if( strcmp(key, "IG_4") == 0 ) {
    this->rec.ig4 = value;
  } else if( strcmp(key, "IP_1") == 0 ) {
    this->rec.ip1 = value;
  } else if( strcmp(key, "IP_2") == 0 ) {
    this->rec.ip2 = value;
  } else if( strcmp(key, "IP_3") == 0 ) {
    this->rec.ip3 = value;
  } else if( strcmp(key, "DIPM") == 0 ) {
    if ( this->is_valid(dhm_valid)) {
      this->ip1_m[this->nl_m -1 ] = value;
      if ( this->is_valid(pref_8_valid)) {
	this->a_m_8[this->nl_m -1 ] = c_comp_diag_a_ip1(this->pref_8, value);
      } else {
	// Height coordinate
	this->a_m_8[this->nl_m -1 ] = c_convip_IP2Level(value, &kind);
      }
      if( this->c_encode_vert() == VGD_ERROR) {
	printf("(Cvgd) ERROR in Cvgd_put_int, problem with c_encode_vert for key %s\n",key);
	return(VGD_ERROR);
      }
    } else {
      printf("(Cvgd) ERROR in Cvgd_put_int, DIPM cannot be put for Vcode %d\n", this->vcode);
      return(VGD_ERROR);
    }
  } else if( strcmp(key, "DIPT") == 0 ) {
    if ( this->is_valid(dht_valid)) {
      this->ip1_t[this->nl_t - 1] = value;
      if ( this->is_valid(pref_8_valid)) {
	this->a_t_8[this->nl_t - 1] = c_comp_diag_a_ip1(this->pref_8, value);
      } else {
	// Height coordinate
	this->a_t_8[this->nl_t -1 ] =  c_convip_IP2Level(value, &kind);
      }
      if( this->c_encode_vert() == VGD_ERROR) {
	printf("(Cvgd) ERROR in Cvgd_put_int, problem with c_encode_vert for key %s\n", key);
	return(VGD_ERROR);
      }
    } else {
      printf("(Cvgd) ERROR in Cvgd_put_int, DIPT cannot be put for Vcode %d\n", this->vcode);
      return(VGD_ERROR);
    }
  } else if( strcmp(key, "DIPW") == 0 ) {
    if ( this->is_valid(dhw_valid)) {
      this->ip1_w[this->nl_w - 1] = value;
      if ( this->is_valid(pref_8_valid)) {
	this->a_w_8[this->nl_w - 1] = c_comp_diag_a_ip1(this->pref_8, value);
      } else {
	// Height coordinate
	this->a_w_8[this->nl_w -1 ] =  c_convip_IP2Level(value, &kind);
      }
      if( this->c_encode_vert() == VGD_ERROR) {
	printf("(Cvgd) ERROR in Cvgd_put_int, problem with c_encode_vert for key %s\n", key);
	return(VGD_ERROR);
      }
    } else {
      printf("(Cvgd) ERROR in Cvgd_put_int, DIPW cannot be put for Vcode %d\n", this->vcode);
      return(VGD_ERROR);
    }
  } else {
    printf("(Cvgd) ERROR in Cvgd_put_int, invalid key %s\n", key);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int vgrid::C_get_consistent_pt_e1(int iun, float *val, char *nomvar ){
  int error, ni, nj, nk, nmax=1000, infon, k;
  int liste[nmax];
  float *work;
  VGD_TFSTD_ext var;

  error = c_fstinl(iun, &ni, &nj, &nk, -1, " ", -1, -1, -1, " ", nomvar, liste, &infon, nmax);
  if (error < 0) {
    printf("(Cvgd) ERROR in C_get_consistent_pt_e1, with fstinl\n");
    return(VGD_ERROR);
  }
  
  if( infon > 1 ){
    printf("(Cvgd)  More than one %s checking consistency ...\n",nomvar);
  }

  if( my_alloc_float(&work, ni*nj, "(Cvgd) ERROR in C_get_consistent_pt_e1, unable to allocate work") == VGD_ERROR )
    return(VGD_ERROR);

  for( k = 0; k < infon; k++ ){
    if( my_fstprm(liste[k], &var) == VGD_ERROR ){
      goto bomb;
    }
    if ( var.ni != ni && var.nj != nj && var.nk != nk ){
	printf("(Cvgd) ERROR: in C_get_consistent_pt_e1, dim misatch for %s, expected (%d,%d,%d), got (%d,%d,%d)\n", nomvar, ni, nj, nk, var.ni, var.nj, var.nk);
      goto bomb;
    }
    if( c_fstluk(work,liste[k],&ni,&nj,&nk) < 0 ){
      printf("(Cvgd) ERROR: in C_get_consistent_pt_e1, with c_fstluk");
    }
    if( k == 0 ){
      *val = work[0];
    } else {
      if( memcmp( &(work[0]), val, sizeof(float)/sizeof(char)) ){
	printf("(Cvgd) ERROR: in C_get_consistent_pt_e1, inconsistent %s, %f v %f\n", nomvar, work[0], *val);
	goto bomb;
      }
    }
  }
  printf("(Cvgd)   All %s consistent\n", nomvar);
  free(work);
  return(VGD_OK);
 bomb:
  free(work);
  return(VGD_ERROR);
}

int vgrid::C_get_consistent_hy(int iun, VGD_TFSTD_ext var, VGD_TFSTD_ext *va2, char *nomvar ){
  int error, ni, nj, nk, nmax=1000, infon, ind;
  int liste[nmax];
  VGD_TFSTD_ext va3;

  // Note: HY may have dateo or datev
  // Try dateo first
  error = c_fstinl(iun, &ni, &nj, &nk, var.dateo, var.etiket, -1, -1, -1, " ", nomvar, liste, &infon, nmax);
  if (error < 0) {
    printf("(Cvgd) ERROR in C_get_consistent_hy, with fstinl on dateo\n");
    return(VGD_ERROR);
  }  
  if( infon == 0 ){
    // No dateo, check datev
    error = c_fstinl(iun, &ni, &nj, &nk, var.datev, var.etiket, -1, -1, -1, " ", nomvar, liste, &infon, nmax);
    if (error < 0) {
      printf("(Cvgd) ERROR in C_get_consistent_hy, with fstinl on datev\n");
      return(VGD_ERROR);
    }
    if( infon == 0 ){
      printf("(Cvgd)  ERROR in C_get_consistent_hy, no record of nomvar = %s, (dateo = %d or datev = %d), etiket = %s found\n", nomvar, var.dateo, var.datev, var.etiket);
      return(VGD_ERROR);
    }
  }
  for( ind = 0; ind < infon; ind++ ){
    if( ind == 0 ){
      if( my_fstprm(liste[ind], va2) == VGD_ERROR ){
	return(VGD_ERROR);
      }
      printf("(Cvgd)   Found matching HY\n");
    } else {
      printf("(Cvgd)   More than one %s, checking consistency ...\n",nomvar);
      if( my_fstprm(liste[ind], &va3) == VGD_ERROR ){
	return(VGD_ERROR);
      }
      if ( va3.ni != ni && va3.nj != nj && va3.nk != nk ){
	printf("(Cvgd) ERROR: in C_get_consistent_hy, dim misatch for %s, expected (%d,%d,%d), got (%d,%d,%d)\n", nomvar, ni, nj, nk, va3.ni, va3.nj, va3.nk);
	return(VGD_ERROR);
      }
      if ( va3.ig1 != va2->ig1 && va3.ig2 != va2->ig2 && va3.ig3 != va2->ig3 && va3.ig4 != va2->ig4 ){
	printf("(Cvgd) ERROR: in C_get_consistent_hy, igs misatch for %s, expected (%d,%d,%d,%d), got (%d,%d,%d,%d)\n", nomvar, va2->ig1, va2->ig2, va2->ig3, va2->ig4, va3.ig1, va3.ig2, va3.ig3, va3.ig4);
	return(VGD_ERROR);
      } 
    }
  }
  if( infon > 1 )
    printf("(Cvgd)   All %s consistent\n", nomvar);
  return(VGD_OK);
}

int vgrid::Cvgd_write_desc (int unit) {
  int ip1, ip2;
  float work[1];

  if(! this->valid) {
    printf("(Cvgd) ERROR in Cvgd_write_desc, vgrid structure is not valid %d\n", this->valid);    
    return(VGD_ERROR);
  }
  ip1=this->rec.ip1;
  if(this->rec.ip1 < 0) ip1=0;
  ip2=this->rec.ip2;
  if(this->rec.ip2 < 0) ip2=0;
  
  if( c_fstecr( this->table,      work,            -this->rec.nbits, unit, 
		this->rec.dateo,  this->rec.deet,   this->rec.npas, 
		this->table_ni,   this->table_nj,   this->table_nk, 
		ip1,              ip2,              this->rec.ip3,
		this->rec.typvar, this->rec.nomvar, this->rec.etiket, 
		this->rec.grtyp,  this->rec.ig1,    this->rec.ig2,    this->rec.ig3, this->rec.ig4,
		this->rec.datyp, 1) , 0 ) {
    printf("(Cvgd) ERROR in Cvgd_write_desc, problem with fstecr\n");
    return(VGD_ERROR);
  }

  return(VGD_OK);

}

int vgrid::Cvgd_stda76_temp(int *i_val, int nl, float *temp){
  int vcode;
  float *pres;
  pres = (float*)malloc( nl * sizeof(float) );
  if(! pres){
    printf("(Cvgd) ERROR in Cvgd_stda76_temp, problem allocating pres\n");
    return(VGD_ERROR);
  }  
  if(! temp){
    printf("(Cvgd) ERROR in Cvgd_stda76_temp, temp not allocated\n");
    return(VGD_ERROR);
  }
  
  if( this->Cvgd_get_int("VCOD", &vcode, 1) == VGD_ERROR ){
    return(VGD_ERROR);
  };
  if(! strcmp(this->ref_name,"ME  ") || vcode == 4001 ){
    if( this->c_stda76_temp_pres_from_heights(i_val, nl, temp, pres, NULL, NULL) == VGD_ERROR ){
      return(VGD_ERROR);
    }
  } else {
    if( this->c_stda76_temp_from_press(i_val, nl, temp) == VGD_ERROR ){
      return(VGD_ERROR);
    }
  }
  free(pres);
  return(VGD_OK);
}

int vgrid::Cvgd_stda76_pres(int *i_val, int nl, float *pres, float *sfc_temp, float *sfc_pres){

  float *temp;
  temp = (float*)malloc( nl * sizeof(float) );
  if(! temp){
    printf("(Cvgd) ERROR in Cvgd_stda76_pres, problem allocating temp of size %d \n",nl);
    return(VGD_ERROR);
  }
  if(! pres){
    printf("(Cvgd) ERROR in Cvgd_stda76_pres, pres not allocated\n");
    return(VGD_ERROR);
  }
  if(! strcmp(this->ref_name,"ME  ")){
    if( this->c_stda76_temp_pres_from_heights(i_val, nl, temp, pres, sfc_temp, sfc_pres) == VGD_ERROR ){
      return(VGD_ERROR);
    }
  } else {
    printf("ERROR: please contact the vgrid developpers to add c_stda76_pres_from_pres\n");
    return(VGD_ERROR);
    //if( c_stda76_pres_from_press(i_val, nl, pres) == VGD_ERROR ){
    //  return(VGD_ERROR);
    //}
  }
  free(temp);
  return(VGD_OK);
}

int vgrid::Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres,
						      int nb){

  int i, k, zero_lapse_rate[STDA76_N_LAYER];
  float Tk[STDA76_N_LAYER+1], pk[STDA76_N_LAYER+1], zk[STDA76_N_LAYER+1];
  float gammaT[STDA76_N_LAYER];

  if(c_get_stda76(Tk, pk, zk, gammaT, zero_lapse_rate) == VGD_ERROR){
    return(VGD_ERROR);
  }
  for(i=0; i<nb; i++){
    if( pres[i] <= pk[STDA76_N_LAYER]){
      printf("Pressure %f Pa in list is out of the standard atmophere pressure upper bound which is %f Pa\n", pres[i], pk[STDA76_N_LAYER]);
      return(VGD_ERROR);
    }
    if(pres[i] >= pk[0]){
      // Integrate downward from surface pres and temp with gammaT[0]
      hgts[i] = (float) (zk[0] + Tk[0]/gammaT[0] * ( exp(-(VGD_RGASD*gammaT[0])/VGD_GRAV * log(pres[i]/pk[0] )) - 1.f ));
    } else {
      for(k=0; k<STDA76_N_LAYER; k++){
	if(pres[i] > pk[k+1]){
	  // compute height up to pressure value
	  if( zero_lapse_rate[k] ){
	    hgts[i] = (float) (zk[k] - (VGD_RGASD*Tk[k])/VGD_GRAV 
			       * log(pres[i]/pk[k]));
	  } else {
	    hgts[i] = (float) (zk[k] + Tk[k]/gammaT[k]
			       * ( exp(-(VGD_RGASD*gammaT[k])/VGD_GRAV
				       * log(pres[i]/pk[k] )) - 1.f ));
	  }
	  break;
	}
      }
    }
    //printf("pres[i] = %f, hgts[i] = %f\n", pres[i], hgts[i]);
  }
  
  return(VGD_OK);
}

int vgrid::Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts,
						      int nb){

  int i, k, zero_lapse_rate[STDA76_N_LAYER];
  float Tk[STDA76_N_LAYER+1], pk[STDA76_N_LAYER+1], zk[STDA76_N_LAYER+1];
  float gammaT[STDA76_N_LAYER];

  if(c_get_stda76(Tk, pk, zk, gammaT, zero_lapse_rate) == VGD_ERROR){
    return(VGD_ERROR);
  }
  for(i=0; i<nb; i++){
    if( hgts[i] > zk[STDA76_N_LAYER]){
      printf("Height %f m in list is out of the standard atmophere height upper bound which is %f m\n", hgts[i], zk[STDA76_N_LAYER]);
      return(VGD_ERROR);
    }
    if(hgts[i] <= zk[0]){
      pres[i] = (float) ( pk[0] * exp(-VGD_GRAV/(VGD_RGASD*gammaT[0])
				      * log(gammaT[0]*(hgts[i]-zk[0])/Tk[0]
					    + 1.f)));
			    
    } else {
      for(k=0; k<STDA76_N_LAYER; k++){
	if(hgts[i] < zk[k+1]){
	  if( zero_lapse_rate[k] ){
	    pres[i] = (float) (pk[k]
			       * exp(-VGD_GRAV/(VGD_RGASD*Tk[k])
				     *(hgts[i]-zk[k])));
	  } else {
	    pres[i] = (float) (pk[k] * exp(-VGD_GRAV/(VGD_RGASD*gammaT[k])
					   * log(gammaT[k]*(hgts[i]-zk[k])
						 /Tk[k] + 1.f)));
	  }
	  break;
	}
      }
    }
    //printf("pres[i] = %f, hgts[i] = %f\n", pres[i], hgts[i]);
  }

  return(VGD_OK);
}


// Construct a vgrid from key
void vgrid::build_vgrid_from_key(int key)
{
  double *table;
  VGD_TFSTD_ext var;

  // Read all the description information, var, for the key
  if( my_fstprm(key, &var) == VGD_ERROR ) {
    printf("(Cvgd) ERROR in vgrid::build_vgrid_from_key, with my_fstprm on key %d\n",key);
    throw vgrid_exception();
  }

  // Enter var data into this; and read the field into this->table
  if( this->C_load_toctoc(var, key) == VGD_ERROR )
  {
    printf("(Cvgd) ERROR in vgrid::build_vgrid_from_key, cannot load !!\n");
  }

  this->kind    = (int) this->table[0];
  this->version = (int) this->table[1];

  if( this->Cvgd_set_vcode() == VGD_ERROR )
  {
    printf("(Cvgd) ERROR in Cvgd_set_vcode, cannot set vcode\n");
    throw vgrid_exception();
  }

  if( this->c_decode_vert() == VGD_ERROR )
  {
    printf("(Cvgd) in Cvgd_set_vcode, problem decoding table with vcode %d\n", this->vcode);
    throw vgrid_exception();
  }
  this->valid = 1;
  if(this->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_set_vcode, problem creating record information\n");
  }

  return;
}
// end of class vgrid
