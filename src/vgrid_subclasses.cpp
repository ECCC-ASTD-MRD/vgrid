// libdescrip - Vertical grid descriptor library for FORTRAN programming
// Copyright (C) 2016  Direction du developpement des previsions nationales
//                     Centre meteorologique canadien
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


#include "vgrid_subclasses.hpp"
#include <stdlib.h>
#include <stdio.h>


// ########## class 0001 ##########
vgrid_0001::vgrid_0001() : vgrid()
{
  this->kind = 0;
  this->version = 1;
  this->vcode = 0001;
}
vgrid_0001::vgrid_0001(int key) : vgrid(key)
{
}

int vgrid_0001::c_decode_vert()
{
  int skip, nk, k, ind;

  skip          = (int) this->table[2];
  ind = 3;
  
  nk = (this->table_nj - skip)/2;
  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();
  // Allocate and assign level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_0001::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){      
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t =    (int*)malloc( nk * sizeof(int) );
  this->a_t_8 = (double*)malloc( nk * sizeof(double) );
  this->b_t_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_t || !this->a_t_8 || !this->b_t_8 ){
    printf("(Cvgd) ERROR in vgrid_0001::c_decode_vert_0001, cannot allocate,  ip1_t, a_t_8 and b_t_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){      
    this->ip1_t[k] = (int) this->table[ind  ];
    this->a_t_8[k] =       this->table[ind+1];
    this->b_t_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;
  return(VGD_OK);
}


// ########## class 1001 ##########
vgrid_1001::vgrid_1001() : vgrid()
{
  this->kind = 1;
  this->version = 1;
  this->vcode = 1001;
}
vgrid_1001::vgrid_1001(int key) : vgrid(key)
{
}

int vgrid_1001::c_decode_vert()
{
  int skip, nk, k, ind;

  skip = (int) this->table[2];
  flip_transfer_d2c(this->ref_name,this->table[3]);
  // The next two values in table are not used, so we continue with ind = 6
  ind = 6;
  
  nk = this->table_nj - skip;
  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();
  // Allocate and assign level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_1001::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){      
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }  
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;
  return(VGD_OK);
}

int vgrid_1001::C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m)
{
  char ok = 1;
  int k,ip1, kind;
  float f_one=1.f;

  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_1001::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_1001::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_1001::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);
  
  if( memcmp( &(hyb[nk-1]), &f_one, sizeof(float)/sizeof(char)) ){
    printf("WRONG SPECIFICATION OF SIGMA VERTICAL LEVELS: SIGMA(NK) MUST BE 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(hyb[k] <= hyb[k-1])
    {
      printf("WRONG SPECIFICATION OF SIGMA VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", hyb[k]);
    }
    return(VGD_ERROR);
  }

  for ( k = 0; k < nk; k++)
  {
    (*a_m_8)[k]=0.;
    // Go back and forth to ip1 in order to make sure hyb value is encodable.
    ip1 = c_convip_Level2IP_old_style(hyb[k],1);
    (*b_m_8)[k] = (double) c_convip_IP2Level(ip1,&kind);
    (*ip1_m)[k] = ip1;
  }

  return(VGD_OK);
}


// ########## class 1002 ##########
vgrid_1002::vgrid_1002() : vgrid()
{
  this->kind = 1;
  this->version = 2;
  this->vcode = 1002;
}
vgrid_1002::vgrid_1002(int key) : vgrid(key)
{
}

int vgrid_1002::c_decode_vert()
{
  int skip, nk, k, ind;

  skip          = (int) this->table[2];
  this->ptop_8  =       this->table[3];
  flip_transfer_d2c(this->ref_name,this->table[4]);
  // The next value in table is not used, so we continue with ind = 6
  ind = 6;
  nk = this->table_nj - skip;
  
  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();
  // Allocate and assign level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_1002::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
   for ( k = 0; k < nk; k++){      
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;

  return(VGD_OK);
}

int vgrid_1002::C_genab(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m)
{
  char ok=1;
  int k;

  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_1002::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_1002::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_1002::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);

  // For eta, relax the test on etauser[nk-1] != 1. to allow legacy construction of
  // partial atmospheric levels. Some users just wanted the top half of the atmosphere and
  // this test was making their call to vgrid to bomb. Since there is no more model
  // using eta this is safe.

  if(etauser[nk-1] > 1.)
  {
    printf("WRONG SPECIFICATION OF ETA VERTICAL LEVELS: ETA(NK-1) MUST BE LESS OR EQUAL TO 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(etauser[k] <= etauser[k-1])
    {
      printf(" WRONG SPECIFICATION OF ETA VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", etauser[k]);
    }
    return(VGD_ERROR);
  }

  if( *ptop_8 <= 0.)
  {
    printf("(Cvgd) ERROR in vgrid_1002::C_genab: ptop = %f must be greater than zero\n", *ptop_8);
    return(VGD_ERROR);
  }

  int ip1, kind;
  float eta;
  for ( k = 0; k < nk; k++)
  {
    ip1 = c_convip_Level2IP_old_style(etauser[k],1);
    eta = c_convip_IP2Level(ip1,&kind);
    (*ip1_m)[k] = ip1;
    (*a_m_8)[k] = (1. - eta) * (*ptop_8);
    (*b_m_8)[k] = eta;
  }

  return(VGD_OK);
}

// ########## common to classes vgrid_1003, vgrid_5001 ##########
vgrid_1003_5001::vgrid_1003_5001(int key) : vgrid(key)
{
}

int vgrid_1003_5001::c_decode_vert()
{
  int skip, k, ind, nk;

  skip            = (int) this->table[2];

  this->ptop_8  =         this->table[3];
  this->pref_8  =         this->table[4];
  this->rcoef1  = (float) this->table[5];
 
  flip_transfer_d2c(this->ref_name,this->table[6]);
  // The next two values in table are not used, so we continue with ind = 9
  ind = 9;
  nk = this->table_nj - skip;

  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // Allocate and assign momentum level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 )
  {
    printf("(Cvgd) ERROR in vgrid_1003_5001::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++)
  {    
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;
  
  return(VGD_OK);
}


// ########## class 1003 ##########
vgrid_1003::vgrid_1003() : vgrid()
{
  this->kind = 1;
  this->version = 3;
  this->vcode = 1003;
}
vgrid_1003::vgrid_1003(int key) : vgrid_1003_5001(key)
{
}

int vgrid_1003::C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m)
{
  char ok = 1;
  int k;
  int complete, ip1, kind;
  float f_one=1.f, f_zero=0.f, pres;
  double hybtop = ptop_8 / pref_8;
  double hyb;
   
  ok = 1;
  if( ptop_8 <= 0.)
  {
    printf("(Cvgd) ERROR in C_genab_1003: ptop must be greater than zero, got %f\n", ptop_8);
    return(VGD_ERROR);
  }
  if( memcmp( &(hybuser[nk-1]), &f_one, sizeof(float)/sizeof(char)) )
  {
    printf("(Cvgd) ERROR in C_genab_1003: WRONG SPECIFICATION OF HYB VERTICAL LEVELS: HYB(NK) MUST BE 1.0, got %f\n",hybuser[nk-1]);
    return(VGD_ERROR);
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf("(Cvgd) ERROR in C_genab_1003: WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok = 0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_1003::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_1003::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_1003::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);

  for( k=0; k < nk; k++)
  {
    // Denormalised hyb to compute A and B
    ip1 = c_convip_Level2IP_old_style( (float) hybuser[k], 1);
    pres = c_convip_IP2Level(ip1, &kind);
    (*ip1_m)[k] = ip1;
    hyb = pres + ( 1. - pres ) * ptop_8/pref_8;
    if( k == 0)
    {
      if( memcmp( &(hybuser[k]), &f_zero, sizeof(float)/sizeof(char)) )
      {
	// Complete set
	complete = 1;
	hybtop = hyb;
      } 
      else 
      {
	// Incomplete set
	complete = 0;
	hybtop = ptop_8/pref_8;
      }
    }
    if( complete && k == 0 )
    {
      // Make sure B top is zero and  A is ptop
      (*b_m_8)[k] = 0.;
      (*a_m_8)[k] = ptop_8;
    }
    else
    {
      (*b_m_8)[k] = pow(( hyb - hybtop )/( 1. - hybtop ), rcoef);
      (*a_m_8)[k] = pref_8 * ( hyb - (*b_m_8)[k] );
    }
  }

  return(VGD_OK);
}


// ########## class 2001 ##########
vgrid_2001::vgrid_2001() : vgrid()
{
  this->kind = 2;
  this->version = 1;
  this->vcode = 2001;
}
vgrid_2001::vgrid_2001(int key)
{
}

int vgrid_2001::c_decode_vert()
{
  int skip, nk, k, ind;

  skip = (int) this->table[2];
  ind = 3;

  nk = this->table_nj - skip;

  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();
  // Allocate and assign level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_2001::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){      
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;
  return(VGD_OK);
}

int vgrid_2001::C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m)
{
  char ok = 1;
  int k;
  
  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_2001::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_2001::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_2001::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);
  
  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(pres[k] <= pres[k-1])
    {
      printf("WRONG SPECIFICATION OF PRESSURE VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", pres[k]);
    }
    return(VGD_ERROR);
  }

  for ( k = 0; k < nk; k++)
  {
    (*a_m_8)[k] = pres[k] * 100.;
    (*b_m_8)[k] = 0.;
    // Go back and forth to ip1 in order to make sure pres value is encodable.
    (*ip1_m)[k] = c_convip_Level2IP(pres[k],2);
  }

  return(VGD_OK);
}


// ########## class 4001 ##########
vgrid_4001::vgrid_4001() : vgrid()
{
  this->kind = 4;
  this->version = 1;
  this->vcode = 4001;
}
vgrid_4001::vgrid_4001(int key)
{
}

int vgrid_4001::c_decode_vert()
{
  int skip, nk, k, ind;

  skip = (int) this->table[2];
  ind = 3;

  nk = this->table_nj - skip;

  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();
  // Allocate and assign level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_4001::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){      
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->valid = 1;
  return(VGD_OK);
}

int vgrid_4001::C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m)
{

  // Andre Plante May 2018.
  char ok = 1;
  int k;
  
  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_4001::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_4001::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_4001::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);
  
  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(hgts[k] <= hgts[k-1])
    {
      printf("WRONG SPECIFICATION OF HEIGHTS VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if ( hgts[0] < 0. )
  {
    printf("WRONG SPECIFICATION OF HEIGHTS VERTICAL LEVELS: LEVELS must be positive\n");
    ok=0;
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", hgts[k]);
    }
    return(VGD_ERROR);
  }

  for ( k = 0; k < nk; k++)
  {
    (*a_m_8)[k] = hgts[k];
    (*b_m_8)[k] = 0.;
    (*ip1_m)[k] = c_convip_Level2IP(hgts[k],4);
  }

  return(VGD_OK);
}


// ########## class 5001 ##########
vgrid_5001::vgrid_5001() : vgrid()
{
  this->kind = 5;
  this->version = 1;
  this->vcode = 5001;
}
vgrid_5001::vgrid_5001(int key) : vgrid_1003_5001(key)
{
}

int vgrid_5001::C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m)
{
  char ok = 1;
  int k;
  int complet, ip1, kind;
  float epsilon=1.0e-6, f_one=1.f;
  double hybtop = ptop_8 / pref_8;
  double hyb, pr1;
   
  if( my_alloc_double(a_m_8, nk, "(Cvgd) ERROR in vgrid_5001::C_genab, malloc error with a_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_double(b_m_8, nk, "(Cvgd) ERROR in vgrid_5001::C_genab, malloc error with b_m_8") == VGD_ERROR )
    return(VGD_ERROR);
  if( my_alloc_int   (ip1_m, nk, "(Cvgd) ERROR in vgrid_5001::C_genab, malloc error with ip1_m") == VGD_ERROR )
    return(VGD_ERROR);

  if( memcmp( &(hybuser[nk-1]), &f_one, sizeof(float)/sizeof(char)) )
  {
    printf("WRONG SPECIFICATION OF HYB VERTICAL LEVELS: HYB(NK) MUST BE 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(hybuser[k] <= hybuser[k-1])
    {
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  if( ptop_8 <= 0.)
  {
    printf("(Cvgd) ERROR in C_genab_5001: ptop = %f must be greater than zero\n", ptop_8);
    return(VGD_ERROR);
  }

  if( ( ptop_8 - (double)hybuser[0] * pref_8 ) / ptop_8 > epsilon )
  {
    printf("(Cvgd) ERROR in C_genab_5001: ptop = %f is lower than first hyb level = %f\n", ptop_8, (double)hybuser[0]*pref_8);
    return(VGD_ERROR);
  }

  ip1 = c_convip_Level2IP( (float) hybtop, 5);
  hybtop = (double) c_convip_IP2Level(ip1,&kind);
  pr1 = 1./(1.-hybtop);
  
  // Find out if first level is at top
  
  if( fabs( ptop_8 - (double)hybuser[0] * pref_8 ) / ptop_8 < epsilon)
  {
    complet = 1;
  } else{
    printf("(Cvgd) NOTE: First hyb level is not at model top\n");
    complet = 0;
  }

  for ( k = 0; k < nk; k++)
  {
    ip1 = c_convip_Level2IP(hybuser[k],5);
    hyb = (double)c_convip_IP2Level(ip1,&kind);
    (*ip1_m)[k] = ip1;
    (*b_m_8)[k] = pow( (hyb - hybtop) * pr1, rcoef);
    (*a_m_8)[k] = pref_8 * ( hyb - (*b_m_8)[k] );
  }
  if(complet)
  {
    (*b_m_8)[0] = 0.;
    (*a_m_8)[0] = ptop_8;
  }
    
  return(VGD_OK);
}


// ########## common to classes vgrid_5002, vgrid_5003, vgrid_5004, vgrid_5005 ##########
vgrid_5002_5003_5004_5005::vgrid_5002_5003_5004_5005(int key, int k_plus_top_value) : vgrid(key)
{
  k_plus_top = k_plus_top_value;
}

int vgrid_5002_5003_5004_5005::c_decode_vert()
{
  int skip, k, ind, k_plus_diag, nk, nb, kind;

  this->kind    =   (int) this->table[0];
  this->version =   (int) this->table[1];
  skip          =   (int) this->table[2];
  this->ptop_8  =         this->table[3];
  this->pref_8  =         this->table[4];
  this->rcoef1  = (float) this->table[5];
  this->rcoef2  = (float) this->table[6];

  flip_transfer_d2c(this->ref_name,this->table[7]);
  if( this->Cvgd_set_vcode_i(this->kind, this->version) == VGD_ERROR )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::c_decode_vert, cannot set vcode\n");
    return(VGD_ERROR);
  }

  k_plus_diag = 0;
  if(this->is_valid(dhm_valid))
  {
    k_plus_diag=1;
  }

  // The next value in table is not used, so we continue with ind = 9
  ind = 9;
  // nk is the number of momentum level without hyb=1.0 and the diag level in m
  nk = ( this->table_nj - k_plus_top - skip ) / 2 -1 -k_plus_diag;

  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // Allocate and assign momentum level data, there are nb of them nk + hyb=1 and possibly the diag in m
  nb = nk + 1 + k_plus_diag;
  this->nl_m = nb;
  this->ip1_m = (int*)malloc( nb * sizeof(int) );
  this->a_m_8 = (double*)malloc( nb * sizeof(double) );
  this->b_m_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++)
  {
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  if(this->is_valid(dhm_valid)) this->dhm = c_convip_IP2Level( this->ip1_m[nb-1], &kind );

  // Allocate and assign thermodynamic level data
  nb = nb + k_plus_top;
  this->nl_t = nb;
  this->nl_w = nb;
  this->ip1_t = (int*)malloc( nb * sizeof(int) );
  this->a_t_8 = (double*)malloc( nb * sizeof(double) );
  this->b_t_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_t || !this->a_t_8 || !this->b_t_8 )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::c_decode_vert, cannot allocate,  ip1_t, a_t_8 and b_t_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++)
  {
    this->ip1_t[k] = (int) this->table[ind  ];
    this->a_t_8[k] =       this->table[ind+1];
    this->b_t_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  if(this->is_valid(dht_valid)) this->dht= c_convip_IP2Level( this->ip1_t[nb-1], &kind );
  this->ip1_w = this->ip1_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  this->valid = 1;
  return(VGD_OK);  
}


int vgrid_5002_5003_5004_5005::C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift)
{  
  // Processing option
  if( ! ( tlift == 0 || tlift == 1 ) )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, wrong value given to tlift, expecting 0 (for false) or 1 (for true), got %d\n",tlift);
    fflush(stdout);
    return(VGD_ERROR);
  }

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok = 1;
  int k;
  float hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1;  
  
  *nl_m = nk + 1;
  *nl_t = nk + 2;

  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t)
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);
  if ( ptop_8 <= 0. ) {
    printf("(Cvgd) ERROR in C_genab_5002_5003: ptop_8 must be > 0, got %f\n", ptop_8);
    fflush(stdout);
    return(VGD_ERROR);
  }
  ztop_8  = log(ptop_8);

  // Checking vertical layering

  //    Check range
  hybtop = (float) (ptop_8 / pref_8);
  if( hybuser[nk-1] >= 1. )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    fflush(stdout);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop )
  {
    printf("(Cvgd) ERROR in vgrid_5002_5003_5004_5005::C_genab_5002_5003: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
    fflush(stdout);
    return(VGD_ERROR);
  }

  //Check monotonicity
  for ( k = 1; k < nk; k++)
  {
    if(hybuser[k] <= hybuser[k-1])
    {
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok)
  {
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++)
    {
      printf("   %f\n", hybuser[k]);
    }
    fflush(stdout);
    return(VGD_ERROR);
  }

  // Momentum levels
  pr1 = 1. / (zsrf_8 - ztop_8);
  for( k = 0; k < nk; k++ )
  {
    zeta_8  = zsrf_8 + log((double)hybuser[k]);
    lamba_8  = ( zeta_8 - ztop_8 ) * pr1;
    rcoef  = (float) (rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8);
    b_m_8[k] = pow(lamba_8, rcoef);
    a_m_8[k] = zeta_8;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;

  // Thermodynamic levels    
  for( k = 1; k < nk; k++ )
  {
    b_t_8[k] = 0.5 * ( b_m_8[k] + b_m_8[k-1] );
    a_t_8[k] = 0.5 * ( a_m_8[k] + a_m_8[k-1] );
  }
  // Special thermo levels
  b_t_8[0]    = 0.5 * ( b_m_8[0]    + 0.    );
  b_t_8[nk]   = 0.5 * ( b_m_8[nk-1] + 1.    );
  b_t_8[nk+1] = 1.;
  a_t_8[0]    = 0.5 * ( a_m_8[0]    + ztop_8);
  a_t_8[nk]   = 0.5 * ( a_m_8[nk-1] + zsrf_8);
  a_t_8[nk+1] = zsrf_8;

  if( tlift )
  {
    a_t_8[nk]   = a_m_8[nk-1];
    b_t_8[nk]   = b_m_8[nk-1];
  }

  // Compute ip1 values
  for(k = 0; k < nk; k++ )
  {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);
  
  ip1_t[0]    = c_convip_Level2IP( sqrtf( hybtop     * hybuser[0]   ), 5 );
  for(k = 1; k < nk; k++ )
  {
    ip1_t[k]  = c_convip_Level2IP( sqrtf( hybuser[k] * hybuser[k-1] ), 5 );
  }
  if( tlift )
  {
    ip1_t[nk]   = c_convip_Level2IP( hybuser[nk-1] , 5 );
  }
  else
  {
    ip1_t[nk]   = c_convip_Level2IP( sqrtf( hybuser[nk-1]*1.0f ), 5 );
  }
  ip1_t[nk+1] = c_convip_Level2IP(1.,5);
  
  return(VGD_OK);
}

// ########## class 5002 ##########
vgrid_5002::vgrid_5002() : vgrid()
{
  this->kind = 5;
  this->version = 2;
  this->vcode = 5002;
}
vgrid_5002::vgrid_5002(int key) : vgrid_5002_5003_5004_5005(key, 1)
{
}

// ########## class 5003 ##########
vgrid_5003::vgrid_5003() : vgrid()
{
  this->kind = 5;
  this->version = 3;
  this->vcode = 5003;
}
vgrid_5003::vgrid_5003(int key) : vgrid_5002_5003_5004_5005(key, 1)
{
}

// ########## class 5004 ##########
vgrid_5004::vgrid_5004() : vgrid()
{
  this->kind = 5;
  this->version = 4;
  this->vcode = 5004;
}
vgrid_5004::vgrid_5004(int key) : vgrid_5002_5003_5004_5005(key, 0)
{
}

int vgrid_5004::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t)
{
  // Processing option

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok = 1;
  int k;
  float hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1, zetau_8, zeta2_8, l_ptop_8;  
  
  *nl_m = nk + 1;
  *nl_t = nk + 1;

  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("(Cvgd) ERROR in vgrid_5004::C_genab, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);

  if ( lrint(ptop_8) == -2 || lrint(ptop_8) == -1 ) {
    // Auto compute ptop and make B(1) = 0
    zetau_8 = zsrf_8 + log((double)hybuser[0]);
    zeta2_8 = zsrf_8 + log((double)hybuser[1]);
    ztop_8  = 0.5 * ( 3. * zetau_8 - zeta2_8);
    l_ptop_8 = exp(ztop_8);
    if( lrint(ptop_8) == -1 ) {
      // Compute B(1) from ztop, B(1) != 0
      zetau_8 = ztop_8;
    }
  } else if (ptop_8 <= 0.) {
    printf("(Cvgd) ERROR in vgrid_5004::C_genab: ptop_8 must be > 0, got %f\n",ptop_8);
    return(VGD_ERROR);
  } else {
    // Take B(1) from user's ztop
    l_ptop_8 = ptop_8;
    ztop_8  = log(ptop_8);
    zetau_8 = ztop_8;
  }

  // Checking vertical layering

  //    Check range
  hybtop = (float) (l_ptop_8 / pref_8);
  if( hybuser[nk-1] >= 1. ) {
    printf("(Cvgd) ERROR in vgrid_5004::C_genab: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("(Cvgd) ERROR in vgrid_5004::C_genab: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
    return(VGD_ERROR);
  }

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  // Momentum levels
  pr1 = 1. / (zsrf_8 - zetau_8);
  for( k = 0; k < nk; k++ ) {
    zeta_8  = zsrf_8 + log((double)hybuser[k]);
    lamba_8  = ( zeta_8 - zetau_8 ) * pr1;
    rcoef  = (float) (rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8);
    b_m_8[k] = pow(lamba_8, rcoef);
    a_m_8[k] = zeta_8;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;

  // Thermodynamic levels    
  for( k = 0; k < nk; k++ ) {
    b_t_8[k] = 0.5 * ( b_m_8[k+1] + b_m_8[k] );
    a_t_8[k] = 0.5 * ( a_m_8[k+1] + a_m_8[k] );
  }
  // Special thermo levels
  b_t_8[nk] = 1.;
  a_t_8[nk] = zsrf_8;

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);    
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);

  for(k = 0; k < nk-1; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrtf( hybuser[k+1] * hybuser[k] ), 5 );
  }
  ip1_t[nk-1] = c_convip_Level2IP( sqrtf( 1.f * hybuser[nk-1] ), 5 );
  ip1_t[nk]   = c_convip_Level2IP(1.,5);
  
  return(VGD_OK);
}

// ########## class 5005 ##########
vgrid_5005::vgrid_5005() : vgrid()
{
  this->kind = 5;
  this->version = 5;
  this->vcode = 5005;
}
vgrid_5005::vgrid_5005(int key) : vgrid_5002_5003_5004_5005(key, 0)
{
}


// ########## class 5100 ##########
vgrid_5100::vgrid_5100() : vgrid()
{
  this->kind = 5;
  this->version = 100;
  this->vcode = 5100;
}
vgrid_5100::vgrid_5100(int key)
{
}

int vgrid_5100::c_decode_vert()
{
  int skip, k, ind, nb, kind;

  skip          =   (int) this->table[2];
  this->ptop_8  =         this->table[3];
  this->pref_8  =         this->table[4];
  this->rcoef1  = (float) this->table[5];
  this->rcoef2  = (float) this->table[6];
  this->rcoef3  = (float) this->table[7];
  this->rcoef4  = (float) this->table[8];
  flip_transfer_d2c(this->ref_name ,this->table[9]);
  flip_transfer_d2c(this->ref_namel,this->table[10]);

  if( this->Cvgd_set_vcode_i(this->kind, this->version) == VGD_ERROR ) {
    printf("(Cvgd) ERROR in vgrid_5100::c_decode_vert, cannot set vcode\n");
    return(VGD_ERROR);
  }

  // The next value in table is not used, so we continue with ind = 12
  ind = 12;
  nb = ( this->table_nj - skip ) / 2;

  // Free A, B, C and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // Allocate and assign momentum level data, there are nb of them nk + hyb=1 and possibly the diag in m
  this->nl_m = nb;
  this->ip1_m =    (int*)malloc( nb * sizeof(int) );
  this->a_m_8 = (double*)malloc( nb * sizeof(double) );
  this->b_m_8 = (double*)malloc( nb * sizeof(double) );
  this->c_m_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 || !this->c_m_8){
    printf("(Cvgd) ERROR in vgrid_5100::c_decode_vert, cannot allocate,  ip1_m, a_m_8, b_m_8 and c_m_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++){
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    this->c_m_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dhm = c_convip_IP2Level( this->ip1_m[nb-1], &kind );

  // Allocate and assign thermodynamic level data
  this->nl_t = nb;
  this->nl_w = nb;
  this->ip1_t =    (int*)malloc( nb * sizeof(int) );
  this->a_t_8 = (double*)malloc( nb * sizeof(double) );
  this->b_t_8 = (double*)malloc( nb * sizeof(double) );
  this->c_t_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_t || !this->a_t_8 || !this->b_t_8 || !this->c_t_8 ){
    printf("(Cvgd) ERROR in vgrid_5100::c_decode_vert, cannot allocate,  ip1_t, a_t_8, b_t_8 and c_t_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++){
    this->ip1_t[k] = (int) this->table[ind  ];
    this->a_t_8[k] =       this->table[ind+1];
    this->b_t_8[k] =       this->table[ind+2];
    this->c_t_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dht= c_convip_IP2Level( this->ip1_t[nb-1], &kind );
  this->ip1_w = this->ip1_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  this->valid = 1;

  return(VGD_OK);
}


// ########## class 5999 ##########
vgrid_5999::vgrid_5999() : vgrid()
{
  this->kind = 5;
  this->version = 999;
  this->vcode = 5999;
}
vgrid_5999::vgrid_5999(int key)
{
}

int vgrid_5999::c_decode_vert()
{
  int skip, k, ind, nk;

  skip = (int) this->table[2];
  flip_transfer_d2c(this->ref_name,this->table[3]);

  // The next two values in table are not used, so we continue with ind = 6
  ind = 6;
  nk = this->table_nj - skip;

  // Free A, B and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // Allocate and assign momentum level data, there are nk of them
  this->nl_m = nk;
  this->nl_t = nk;
  this->nl_w = nk;
  this->ip1_m =    (int*)malloc( nk * sizeof(int) );
  this->a_m_8 = (double*)malloc( nk * sizeof(double) );
  this->b_m_8 = (double*)malloc( nk * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 ){
    printf("(Cvgd) ERROR in vgrid_5999::c_decode_vert, cannot allocate,  ip1_m, a_m_8 and b_m_8 of size %d\n", nk);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nk; k++){    
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    ind = ind + 3;
  }
  this->ip1_t = this->ip1_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->ip1_w = this->ip1_m;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  
  return(VGD_OK);
}


// ########## class 21001 ##########
vgrid_21001::vgrid_21001() : vgrid()
{
  this->kind = 21;
  this->version = 1;
  this->vcode = 21001;
}
vgrid_21001::vgrid_21001(int key)
{
}

int vgrid_21001::c_decode_vert()
{
  int skip, k, ind, nb, kind;

  skip          =   (int) this->table[2];
  this->rcoef1  = (float) this->table[3];
  this->rcoef2  = (float) this->table[4];
  this->rcoef3  = (float) this->table[5];
  this->rcoef4  = (float) this->table[6];
  flip_transfer_d2c(this->ref_name, this->table[7]);
  flip_transfer_d2c(this->ref_namel,this->table[8]);
  if( this->Cvgd_set_vcode_i(this->kind, this->version) == VGD_ERROR ) {
    printf("(Cvgd) ERROR in vgrid_21001::c_decode_vert, cannot set vcode\n");
    return(VGD_ERROR);
  }
  // Free A, B, C and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // nb is the number of momentum level with hyb=1.0 and the diag level
  nb = ( this->table_nj - skip ) / 2;
  this->nl_m = nb;
  this->ip1_m =    (int*)malloc( nb * sizeof(int) );
  this->a_m_8 = (double*)malloc( nb * sizeof(double) );
  this->b_m_8 = (double*)malloc( nb * sizeof(double) );
  this->c_m_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 || !this->c_m_8 ){
    printf("(Cvgd) ERROR in vgrid_21001::c_decode_vert, cannot allocate,  ip1_m, a_m_8, b_m_8 and c_m_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  ind = 12;
  for ( k = 0; k < nb; k++){
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    this->c_m_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dhm = c_convip_IP2Level( this->ip1_m[nb-1], &kind );

  // Allocate and assign thermodynamic level data
  this->nl_t = nb;
  this->nl_w = nb;
  this->ip1_t =    (int*)malloc( nb * sizeof(int) );
  this->a_t_8 = (double*)malloc( nb * sizeof(double) );
  this->b_t_8 = (double*)malloc( nb * sizeof(double) );
  this->c_t_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_t || !this->a_t_8 || !this->b_t_8 || !this->c_t_8 ){
    printf("(Cvgd) ERROR in vgrid_21001::c_decode_vert, cannot allocate,  ip1_t, a_t_8, b_t_8 and c_t_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++){
    this->ip1_t[k] = (int) this->table[ind  ];
    this->a_t_8[k] =       this->table[ind+1];
    this->b_t_8[k] =       this->table[ind+2];
    this->c_t_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dht= c_convip_IP2Level( this->ip1_t[nb-1], &kind );
  this->valid = 1;
  this->ip1_w = this->ip1_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  return(VGD_OK);
}


// ########## class 21002 ##########
vgrid_21002::vgrid_21002() : vgrid()
{
  this->kind = 21;
  this->version = 2;
  this->vcode = 21002;
}
vgrid_21002::vgrid_21001(int key)
{
}

int vgrid_21002::c_decode_vert()
{
  int skip, k, ind, nb, kind;

  skip          =   (int) this->table[2];
  this->rcoef1  = (float) this->table[3];
  this->rcoef2  = (float) this->table[4];
  this->rcoef3  = (float) this->table[5];
  this->rcoef4  = (float) this->table[6];
  flip_transfer_d2c(this->ref_name,this->table[7]);
  flip_transfer_d2c(this->ref_namel,this->table[8]);
  if( this->Cvgd_set_vcode_i(this->kind, this->version) == VGD_ERROR ) {
    printf("(Cvgd) ERROR in vgrid_21002::c_decode_vert, cannot set vcode\n");
    return(VGD_ERROR);
  }
  // Free A, B, C and Ip1 vectors for momentum and thermo.
  this->c_vgd_free_abci();

  // nb is the number of momentum level with hyb=1.0 and the diag level
  nb = ( this->table_nj - skip - 1 ) / 2;
  // Allocate and assign momentum arrays
  this->nl_m = nb;
  this->ip1_m =    (int*)malloc( nb * sizeof(int) );
  this->a_m_8 = (double*)malloc( nb * sizeof(double) );
  this->b_m_8 = (double*)malloc( nb * sizeof(double) );
  this->c_m_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_m || !this->a_m_8 || !this->b_m_8 || !this->c_m_8 ){
    printf("(Cvgd) ERROR in vgrid_21002::c_decode_vert, cannot allocate,  ip1_m, a_m_8, b_m_8 and c_m_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  ind = 12;
  for ( k = 0; k < nb; k++){
    this->ip1_m[k] = (int) this->table[ind  ];
    this->a_m_8[k] =       this->table[ind+1];
    this->b_m_8[k] =       this->table[ind+2];
    this->c_m_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dhm = c_convip_IP2Level( this->ip1_m[nb-1], &kind );

  // Allocate and assign Vertical-Velocity arrays
  this->nl_w = nb;
  this->ip1_w =    (int*)malloc( nb * sizeof(int) );
  this->a_w_8 = (double*)malloc( nb * sizeof(double) );
  this->b_w_8 = (double*)malloc( nb * sizeof(double) );
  this->c_w_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_w || !this->a_w_8 || !this->b_w_8 || !this->c_w_8 ){
    printf("(Cvgd) ERROR in vgrid_21002::c_decode_vert, cannot allocate,  ip1_w, a_w_8, b_w_8 and c_w_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  for ( k = 0; k < nb; k++){
    this->ip1_w[k] = (int) this->table[ind  ];
    this->a_w_8[k] =       this->table[ind+1];
    this->b_w_8[k] =       this->table[ind+2];
    this->c_w_8[k] =       this->table[ind+3];
    ind = ind + 4;
  }
  this->dhw = c_convip_IP2Level( this->ip1_w[nb-1], &kind );

  // Allocate and assign thermo arrays
  this->nl_t = nb;
  this->ip1_t =    (int*)malloc( nb * sizeof(int) );
  this->a_t_8 = (double*)malloc( nb * sizeof(double) );
  this->b_t_8 = (double*)malloc( nb * sizeof(double) );
  this->c_t_8 = (double*)malloc( nb * sizeof(double) );
  if( !this->ip1_t || !this->a_t_8 || !this->b_t_8 || !this->c_t_8 ){
    printf("(Cvgd) ERROR in vgrid_21002::c_decode_vert, cannot allocate,  ip1_t, a_t_8, b_t_8 and c_t_8 of size %d\n", nb);
    return(VGD_ERROR);
  }
  // For Lorenz, thermo is momentum, except for diag level (see below)
  // This is why we must allocate arrays and not just point to momentum like the other Vcode do.
  for ( k = 0; k < nb-1; k++){
    this->ip1_t[k] = this->ip1_m[k];
    this->a_t_8[k] = this->a_m_8[k];
    this->b_t_8[k] = this->b_m_8[k];
    this->c_t_8[k] = this->c_m_8[k];
  }
  // Diag level is specific to thermo
  this->ip1_t[nb-1] = (int) this->table[ind  ];
  this->a_t_8[nb-1] = this->table[ind+1];
  this->b_t_8[nb-1] = this->table[ind+2];
  this->c_t_8[nb-1] = this->table[ind+3];
  this->dht = c_convip_IP2Level( this->ip1_t[nb-1], &kind );

  return(VGD_OK);
}
