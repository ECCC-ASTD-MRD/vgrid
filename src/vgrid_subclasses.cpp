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
  this->kind    = 0;
  this->version = 1;
  this->vcode   = 0001;
  this->skip    = 1;  // Could be changed by c_decode_vert
  this->table_ni = 3;
}

vgrid_0001(int key) : vgrid_0001() : vgrid(key)
{
};

void vgrid_0001::set_table_nj(int nk)
{
  table_nj = 2*nk+skip;
}

int vgrid_0001::c_decode_vert()
{
  int nk, k, ind;

  this->skip = (int) this->table[2];
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

int vgrid_0001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  
  int k, ind = 3;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = 0;
    ind = ind + 3;
  }
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_w[k];
    this->table[ind+1] = this->a_w_8[k];
    this->table[ind+2] = 0;
    ind = ind + 3;
  }
  this->nl_w = this->nl_w;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;

  this->valid = 1;

  return(VGD_OK);
}

void vgrid_0001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"HEIGHT_OCEAN");
};


// ########## class 1001 ##########
vgrid_1001::vgrid_1001() : vgrid()
{
  this->kind    = 1;
  this->version = 1;
  this->vcode   = 1001;
  this->skip    = 2; // Could be changed by c_decode_vert
  this->table_ni = 3;
  strcpy(this->ref_name,"P0  ");
}

vgrid_1001(int key) : vgrid_1001() : vgrid(key)
{
};

void vgrid_1001::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_1001::c_decode_vert()
{
  int nk, k, ind;

  this->skip = (int) this->table[2];
  flip_transfer_d2c(this->ref_name,this->table[3]);
  // The next two values in table are not used, so we continue with ind = 6
  ind = 6;
  
  nk = this->table_nj - this->skip;
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

int vgrid_1001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  flip_transfer_c2d(this->ref_name, &(this->table[3]));

  this->table[4] = 0.;
  this->table[5] = 0.;
  

  int k, ind = 6;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;

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

void vgrid_1001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"ETA_GEMV3");
};


// ########## class 1002 ##########
vgrid_1002::vgrid_1002() : vgrid()
{
  this->kind    = 1;
  this->version = 2;
  this->vcode   = 1002;
  this->skip    = 2; // Could be changed by c_decode_vert
  this->table_ni = 3;
  strcpy(this->ref_name,"P0  ");
}

vgrid_1002(int key) : vgrid_1002() : vgrid(key)
{
};

void vgrid_1002::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_1002::c_decode_vert()
{
  int nk, k, ind;

  this->skip    = (int) this->table[2];
  this->ptop_8  =       this->table[3];
  flip_transfer_d2c(this->ref_name,this->table[4]);
  // The next value in table is not used, so we continue with ind = 6
  ind = 6;
  nk = this->table_nj - this->skip;
  
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

int vgrid_1002::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  this->table[3] = this->ptop_8;
  flip_transfer_c2d(this->ref_name, &(this->table[4]));
  this->table[5] = 0.;
  
  int k, ind = 6;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;

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
int vgrid_1003_5001::c_decode_vert()
{
  int k, ind, nk;

  this->skip    =   (int) this->table[2];

  this->ptop_8  =         this->table[3];
  this->pref_8  =         this->table[4];
  this->rcoef1  = (float) this->table[5];
 
  flip_transfer_d2c(this->ref_name,this->table[6]);
  // The next two values in table are not used, so we continue with ind = 9
  ind = 9;
  nk = this->table_nj - this->skip;

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

void vgrid_1002::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"ETA_GEMV3");
  h->ig2=(int)round(this->ptop_8*10.0);
};


// ########## class 1003 ##########
vgrid_1003::vgrid_1003() : vgrid()
{
  this->kind    = 1;
  this->version = 3;
  this->vcode   = 1003;
  this->skip    = 0; // Could be change by c_decode_vert
  strcpy(this->ref_name,"P0  ");
}

vgrid_1003(int key) : vgrid_1003() : vgrid(key)
{
};

void vgrid_1003::set_table_nj(int nk)
{
  table_nj = nk+skip;
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

void vgrid_1003::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"HYBNORM_GEM3");
  h->ig2=(int)round(this->ptop_8*10.0);
  h->ig3=(int)roundf(this->rcoef1*100.0f);
};


// ########## class 2001 ##########
vgrid_2001::vgrid_2001() : vgrid()
{
  this->kind    = 2;
  this->version = 1;
  this->vcode   = 2001;
  this->skip    = 1;
  this->table_ni = 3;
}

vgrid_2001(int key) : vgrid_2001() : vgrid(key)
{
};

void vgrid_2001::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_2001::c_decode_vert()
{
  int nk, k, ind;

  this->skip = (int) this->table[2];
  ind = 3;

  nk = this->table_nj - this->skip;

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

int vgrid_2001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  
  int k, ind = 3;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;

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

void vgrid_2001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"PRESSURE");
};


// ########## class 4001 ##########
vgrid_4001::vgrid_4001() : vgrid()
{
  this->kind = 4;
  this->version = 1;
  this->vcode = 4001;
  this->skip    = 1; // Could be changed by c_decode_vert
  this->table_ni = 3;
}

vgrid_4001(int key) : vgrid_4001() : vgrid(key)
{
};

void vgrid_4001::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_4001::c_decode_vert()
{
  int nk, k, ind;

  this->skip = (int) this->table[2];
  ind = 3;

  nk = this->table_nj - this->skip;

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

int vgrid_4001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  
  int k, ind = 3;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = 0.;
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;

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

void vgrid_4001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"M_ABOVE_SFC");
};


// ########## class 5001 ##########
vgrid_5001::vgrid_5001() : vgrid()
{
  this->kind    = 5;
  this->version = 1;
  this->vcode   = 5001;
  this->skip    = 3;
  this->table_ni = 3;
  strcpy(this->ref_name,"P0  ");
}

vgrid_5001(int key) : vgrid_5001() : vgrid(key)
{
};

void vgrid_5001::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_5001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;

  this->table[3] = this->ptop_8;
  this->table[4] = this->pref_8;
  this->table[5] = this->rcoef1;
  
  flip_transfer_c2d(this->ref_name, &(this->table[6]));
  this->table[7] = 0.;
  this->table[8] = 0.;

  int k, ind = 9;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;
  
  this->valid = 1;

  return(VGD_OK);
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
void vgrid_5002_5003_5004_5005::set_table_nj(int nk)
{
  table_nj = this->nl_m + this->nl_t + skip;
}

int vgrid_5002_5003_5004_5005::c_decode_vert()
{
  int k, ind, k_plus_diag, nk, nb, kind;

  this->kind    =   (int) this->table[0];
  this->version =   (int) this->table[1];
  this->skip    =   (int) this->table[2];
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

int vgrid_5002_5003_5004_5005::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  this->table[3] = this->ptop_8;
  this->table[4] = this->pref_8;
  this->table[5] = this->rcoef1;  
  this->table[6] = this->rcoef2;
  flip_transfer_c2d(this->ref_name, &(this->table[7]));
  this->table[8] = 0.;

  int k, ind = 9;
  for ( k = 0; k < this->nl_m; k++)
  {
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  for ( k = 0; k < this->nl_t; k++)
  {
    this->table[ind  ] = this->ip1_t[k];
    this->table[ind+1] = this->a_t_8[k];
    this->table[ind+2] = this->b_t_8[k];
    ind = ind + 3;
  }
  this->nl_w = this->nl_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  this->c_w_8 = this->c_t_8;
  this->ip1_w = this->ip1_t;

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

void vgrid_5001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"HYB_GEMV3");
  h->ig2=(int)round(this->ptop_8*10.0);
  h->ig3=(int)roundf(this->rcoef1*100.0f);
};

// ########## class 5002 ##########
vgrid_5002::vgrid_5002() : vgrid()
{
  this->kind    = 5;
  this->version = 2;
  this->vcode   = 5002;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->k_plus_top = 1;
  this->table_ni = 3;
}

vgrid_5002(int key) : vgrid_5002() : vgrid(key)
{
};

void vgrid_5002::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"STG_CP_GEMV4");
  h->ig2=(int)round(this->ptop_8*10.0);
  h->ig3=(int)roundf(this->rcoef1*100.0f);
  h->ig4=(int)roundf(this->rcoef2*100.0f);
};

// ########## class 5003 ##########
vgrid_5003::vgrid_5003() : vgrid()
{
  this->kind    = 5;
  this->version = 3;
  this->vcode   = 5003;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->k_plus_top = 1;
  this->table_ni = 3;
}

vgrid_5003(int key) : vgrid_5003() : vgrid(key)
{
};

void vgrid_5003::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"STG_CP_GEMV4");
  h->ig2=(int)round(this->ptop_8*10.0);
  h->ig3=(int)roundf(this->rcoef1*100.0f);
  h->ig4=(int)roundf(this->rcoef2*100.0f);
};

// ########## class 5004 ##########
vgrid_5004::vgrid_5004() : vgrid()
{
  this->kind    = 5;
  this->version = 4;
  this->vcode   = 5004;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->table_ni = 3;
}

vgrid_5004(int key) : vgrid_5004() : vgrid(key)
{
};

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

void vgrid_5004::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"STG_CP_GEMV4");
  h->ig2=(int)round(this->ptop_8*10.0);
  h->ig3=(int)roundf(this->rcoef1*100.0f);
  h->ig4=(int)roundf(this->rcoef2*100.0f);
};

// ########## class 5005 ##########
vgrid_5005::vgrid_5005() : vgrid()
{
  this->kind    = 5;
  this->version = 5;
  this->vcode   = 5005;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->table_ni = 3;
  strcpy(this->ref_name,"P0  ");
}

vgrid_5005(int key) : vgrid_5005() : vgrid(key)
{
};

int vgrid_5005::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht)

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok = 1;
  int k;
  float hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1, zetau_8, zeta2_8;
  
  *nl_m = nk + 2;
  *nl_t = nk + 2;

  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);
  
  // Auto compute ptop and make B(0) = 0
  zetau_8 = zsrf_8 + log((double)hybuser[0]);
  zeta2_8 = zsrf_8 + log((double)hybuser[1]);
  ztop_8  = 0.5 * ( 3. * zetau_8 - zeta2_8);
  (**ptop_out_8) = exp(ztop_8);

  // Checking vertical layering

  //    Check range
  hybtop = (float) ( (**ptop_out_8) / pref_8 );
  if( hybuser[nk-1] >= 1. ) {
    printf("(Cvgd) ERROR in vgrid_5005::C_genab: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("(Cvgd) ERROR in vgrid_5005::C_genab: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
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
  // Integrating the hydrostatic eq with T=0C
  // ln[p(z=dhm)] = ln(ps) - g/(Rd*T)*dhm
  // s = ln(ps) - ln(pref)
  // ln[p(z=dhm)] = ln(pref) - g/(Rd*T)*dhm + s
  // => B=1, A = ln(pref) - g/(Rd*T)*dhm
  // We take T at 0C
  a_m_8[nk+1] = c_comp_diag_a_height(pref_8,dhm);
  b_m_8[nk+1] = 1.;

  // Thermodynamic levels    
  for( k = 0; k < nk; k++ ) {
    b_t_8[k] = 0.5 * ( b_m_8[k+1] + b_m_8[k] );
    a_t_8[k] = 0.5 * ( a_m_8[k+1] + a_m_8[k] );
  }
  // Special thermo levels
  b_t_8[nk]   = 1.;
  a_t_8[nk]   = zsrf_8;
  a_t_8[nk+1] = c_comp_diag_a_height(pref_8,dht);
  b_t_8[nk+1] = 1.;

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);    
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_m[nk+1] = c_convip_Level2IP(dhm,4);

  for(k = 0; k < nk-1; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrtf( hybuser[k+1] * hybuser[k] ), 5 );
  }
  ip1_t[nk-1] = c_convip_Level2IP( sqrtf( 1.f * hybuser[nk-1] ), 5 );
  ip1_t[nk]   = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_t[nk+1] = c_convip_Level2IP(dht,4);
  
  return(VGD_OK);
}

void vgrid_5005::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"STG_CP_GEMV4");
  h->ig2=0;
  h->ig3=(int)roundf(this->rcoef1*100.0f);
  h->ig4=(int)roundf(this->rcoef2*100.0f);
};


// ########## class 5100 ##########
vgrid_5100::vgrid_5100() : vgrid()
{
  this->kind    = 5;
  this->version = 100;
  this->vcode   = 5100;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->table_ni = 4;
  strcpy(this->ref_name,"P0  ");
  strcpy(this->ref_namel,"P0LS");
}

vgrid_5100(int key) : vgrid_5100() : vgrid(key)
{
};

int vgrid_5100::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht, int avg)

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *c_m_8, *a_t_8, *b_t_8, *c_t_8;
  int *ip1_m, *ip1_t;
    
  char ok = 1;
  int k;
  float hybtop, rcoef, rcoefL;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1, zeta1_8, zeta2_8, zetaN_8;
  
  *nl_m = nk + 2;
  *nl_t = nk + 2;

  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_c_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_c_m_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_c_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_c_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_c_t_8){
    printf("(Cvgd) ERROR in vgrid_5100::C_genab, malloc error with *PP_c_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("(Cvgd) ERROR in vgrid_5005::C_genab, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  c_m_8 = *PP_c_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  c_t_8 = *PP_c_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);
  
  // Auto compute ptop and make B(0) = 0
  zeta1_8 = zsrf_8 + log((double)hybuser[0]);
  zeta2_8 = zsrf_8 + log((double)hybuser[1]);
  zetaN_8 = zsrf_8 + log((double)hybuser[nk-1]);
  ztop_8  = 0.5 * ( 3. * zeta1_8 - zeta2_8);
  (**ptop_out_8) = exp(ztop_8);

  // Checking vertical layering

  //    Check range
  hybtop = (float) ( (**ptop_out_8) / pref_8 );
  if( hybuser[nk-1] >= 1. ) {
    printf("(Cvgd) ERROR in vgrid_5100::C_genab: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("(Cvgd) ERROR in vgrid_5100::C_genab: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
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
  if(avg){
    pr1 = 1. / (zsrf_8 - zeta1_8);
  }else{
    pr1 = 1. / (zetaN_8 - zeta1_8);
  }   
  for( k = 0; k < nk; k++ ) {
    zeta_8  = zsrf_8 + log((double)hybuser[k]);
    lamba_8 = fmin(1.0, ( zeta_8 - zeta1_8 ) * pr1);
    rcoefL  = (float) (rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8);
    rcoef   = (float) (rcoef4 - ( rcoef4 - rcoef3 ) * lamba_8);
    a_m_8[k] = zeta_8;
    b_m_8[k] = pow(lamba_8, rcoef);
    c_m_8[k] = pow(lamba_8, rcoefL) - b_m_8[k];
    // Since rcoef* may be big we limit B and C to avoid floating point overflow
    if(b_m_8[k] < 1.e-16)b_m_8[k] = 0.;
    if(c_m_8[k] < 1.e-16)c_m_8[k] = 0.;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;
  c_m_8[nk] = 0.;
  // Integrating the hydrostatic eq with T=0C
  // ln[p(z=dhm)] = ln(ps) - g/(Rd*T)*dhm
  // s = ln(ps) - ln(pref)
  // ln[p(z=dhm)] = ln(pref) - g/(Rd*T)*dhm + s
  // => B=1, A = ln(pref) - g/(Rd*T)*dhm
  // We take T at 0C
  a_m_8[nk+1] = c_comp_diag_a_height(pref_8,dhm);
  b_m_8[nk+1] = 1.;
  c_m_8[nk+1] = 0.;

  // Thermodynamic levels    
  for( k = 0; k < nk; k++ ) {
    a_t_8[k] = 0.5 * ( a_m_8[k+1] + a_m_8[k] );
    zeta_8  = a_t_8[k];
    if(avg){
      b_t_8[k] = 0.5 * ( b_m_8[k+1] + b_m_8[k] );
      c_t_8[k] = 0.5 * ( c_m_8[k+1] + c_m_8[k] );      
    }else{
      lamba_8 = fmin(1., fmax(0.,(zeta_8- zeta1_8)*pr1));
      rcoefL  = (float) (rcoef2-(rcoef2-rcoef1)*lamba_8);
      rcoef   = (float) (rcoef4-(rcoef4-rcoef3)*lamba_8);
      b_t_8[k] = pow(lamba_8, rcoef);
      c_t_8[k] = pow(lamba_8, rcoefL) - b_t_8[k];
    }
    // Since rcoef* may be big we limit B and C to avoid floating point overflow         
    if(b_t_8[k] < 1.e-16) b_t_8[k] = 0.;
    if(c_t_8[k] < 1.e-16) c_t_8[k] = 0.;
  }
  // Special thermo levels
  a_t_8[nk]   = zsrf_8;
  b_t_8[nk]   = 1.;
  c_t_8[nk]   = 0.;
  a_t_8[nk+1] = c_comp_diag_a_height(pref_8,dht);
  b_t_8[nk+1] = 1.;
  c_t_8[nk+1] = 0.;

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);    
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_m[nk+1] = c_convip_Level2IP(dhm,4);

  for(k = 0; k < nk-1; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrtf( hybuser[k+1] * hybuser[k] ), 5 );
  }
  ip1_t[nk-1] = c_convip_Level2IP( sqrtf( 1.f * hybuser[nk-1] ), 5 );
  ip1_t[nk]   = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_t[nk+1] = c_convip_Level2IP(dht,4);
  
  return(VGD_OK);
}

void vgrid_5100::set_table_nj(int nk)
{
  table_nj = this->nl_m + this->nl_t + skip;
}

int vgrid_5100::c_decode_vert()
{
  int k, ind, nb, kind;

  this->skip    =   (int) this->table[2];
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

int vgrid_5100::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  this->table[3] = this->ptop_8;
  this->table[4] = this->pref_8;
  this->table[5] = this->rcoef1;  
  this->table[6] = this->rcoef2;
  this->table[7] = this->rcoef3;
  this->table[8] = this->rcoef4;
  flip_transfer_c2d(this->ref_name, &(this->table[9]));
  flip_transfer_c2d(this->ref_namel, &(this->table[10]));
  this->table[11] = 0.;

  int k, ind = 12;
  for ( k = 0; k < this->nl_m; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    this->table[ind+3] = this->c_m_8[k];
    ind = ind + 4;
  }
  for ( k = 0; k < this->nl_t; k++){
    this->table[ind  ] = this->ip1_t[k];
    this->table[ind+1] = this->a_t_8[k];
    this->table[ind+2] = this->b_t_8[k];
    this->table[ind+3] = this->c_t_8[k];
    ind = ind + 4;
  }
  this->nl_w = this->nl_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  this->c_w_8 = this->c_t_8;
  this->ip1_w = this->ip1_t;

  this->valid = 1;

  return(VGD_OK);
}

void vgrid_5100::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"STG_CP_SLEVE");
  h->ig2=0;
  h->ig3=0;
  h->ig4=0;
};


// ########## class 5999 ##########
vgrid_5999::vgrid_5999() : vgrid()
{
  this->kind    = 5;
  this->version = 999;
  this->vcode   = 5999;
  this->skip    = 2; // Could be changed by c_decode_vert
  this->table_ni = 3;
  strcpy(this->ref_name,"P0  ");
}

vgrid_5999(int key) : vgrid_5999() : vgrid(key)
{
};

void vgrid_5100::set_table_nj(int nk)
{
  table_nj = nk+skip;
}

int vgrid_5999::c_decode_vert()
{
  int k, ind, nk;

  this->skip = (int) this->table[2];
  flip_transfer_d2c(this->ref_name,this->table[3]);

  // The next two values in table are not used, so we continue with ind = 6
  ind = 6;
  nk = this->table_nj - this->skip;

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

int vgrid_5999::c_encode_vert()
{
  int i, k, kind;
  float hyb;

  // Check ip1 validity
  for( k=0; k < nk; k++){
    hyb = c_convip_IP2Level(this->ip1_m[k],&kind);
    hyb = hyb*2.f; // To silence the compiler warning
    // Even if hyb is kind 5, kind 4 may be present due to diag level in m AGL
    if( kind != 5 && kind != 4 ) {
      printf("Error in vgrid_5999::c_encode_vert, ip1 kind must be 5 or 4 but got %d, for ip1 = %d\n", kind, this->ip1_m[k]);
      return(VGD_ERROR);
    }
    for( i=k+1; i < nk; i++) {
      if( this->ip1_m[i] == this->ip1_m[k]) {
	printf("Error in encode_vert_5999, repetition present in ip1 list for at least ip1 = %d\n", this->ip1_m[i]);
	return(VGD_ERROR);
      }
    }
  }

  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  flip_transfer_c2d(this->ref_name, &(this->table[3]));
  this->table[4] = 0.;
  this->table[5] = 0.;

  int ind = 6;
  for ( k = 0; k < nk; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    ind = ind + 3;
  }
  this->nl_t = this->nl_m;
  this->nl_w = this->nl_m;
  this->a_t_8 = this->a_m_8;
  this->b_t_8 = this->b_m_8;
  this->c_t_8 = this->c_m_8;
  this->a_w_8 = this->a_m_8;
  this->b_w_8 = this->b_m_8;
  this->c_w_8 = this->c_m_8;
  this->ip1_t = this->ip1_m;
  this->ip1_w = this->ip1_m;

  this->valid = 1;

  return(VGD_OK);
}

void vgrid_5999::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"UNSTAG_OTHER");
  h->ig2=0;
  h->ig3=0;
  h->ig4=0;
};


// ########## class 21001 ##########
vgrid_21001::vgrid_21001() : vgrid()
{
  this->kind    = 21;
  this->version = 1;
  this->vcode   = 21001;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->table_ni = 4;
  strcpy(this->ref_name,"ME  ");
  strcpy(this->ref_namel,"MELS");
  if (   fabs( this->rcoef3 - this->rcoef1 ) < 1.0e-6
      && fabs( this->rcoef4 - this->rcoef2 ) < 1.0e-6)
  {
    strcpy(this->ref_namel,VGD_NO_REF_NOMVAR);
  }
  if ( this->rcoef3 < 0. || this->rcoef4 < 0. )
  {
    strcpy(this->ref_namel,VGD_NO_REF_NOMVAR);
  }
}

vgrid_21001(int key) : vgrid_21001() : vgrid(key)
{
};


int vgrid_21001::C__genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht)
{
  // Andre Plante Nov 2017.
  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *c_m_8, *a_t_8, *b_t_8, *c_t_8;
  int *ip1_m, *ip1_t;
  char ok = 1;
  int k;
  float rcoef, my_rcoef3, my_rcoef4, hybm[nk+2], hybt[nk+2];
  double lamda_8, pr1;

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] >= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY DECREASING\n");
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

  *nl_m = nk + 2;
  *nl_t = nk + 2;
  
  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_c_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_c_m_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_c_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_c_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_c_t_8){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_c_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("(Cvgd) ERROR in vgrid_21001::C_genab, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  c_m_8 = *PP_c_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  c_t_8 = *PP_c_t_8;
  ip1_t = *PP_ip1_t;
  
  if ( rcoef3 < 0. ){
    if( rcoef4 >= 0. ){
      printf("(Cvgd) ERROR in vgrid_21001::C_genab, rcoef4 should not bet set since rcoef3 is not set\n");
      return(VGD_ERROR);
    }
  }
  if ( rcoef4 < 0. ){
    if( rcoef3 >= 0. ){
      printf("(Cvgd) ERROR in vgrid_21001::C_genab, rcoef3 should not bet set since rcoef4 is not set\n");
      return(VGD_ERROR);
    }
  }
  if ( rcoef3 < 0. ){
    my_rcoef3 = rcoef1;
    my_rcoef4 = rcoef2;
  } else {
    my_rcoef3 = rcoef3;
    my_rcoef4 = rcoef4;
  }
  //Momentum levels
  // Note: hybuser at surface in not in the list but we know it is zero
  pr1 = 1. / (double)hybuser[0];
  for( k = 0; k < nk; k++ ){
    lamda_8 = (double)( hybuser[0] - hybuser[k] ) * pr1;
    rcoef   = (float) (my_rcoef4 - ( my_rcoef4 - my_rcoef3 ) * lamda_8);
    a_m_8[k] = (double)hybuser[k];
    b_m_8[k] = pow(lamda_8, rcoef);
    rcoef   = (float) (rcoef2 - ( rcoef2 - rcoef1 ) * lamda_8);
    c_m_8[k] = pow(lamda_8, rcoef) - b_m_8[k];
    //Since rcoef* may be big we limit B and C to avoid floating point overflow
    if(b_m_8[k] < 1.e-16){
      b_m_8[k] = 0.;
    }
    if(c_m_8[k] < 1.e-16){
      c_m_8[k] = 0.;
    }
  }
  a_m_8[nk]   = 0.;
  b_m_8[nk]   = 1.;
  c_m_8[nk]   = 0.;
  a_m_8[nk+1] = dhm;
  b_m_8[nk+1] = 1.;
  c_m_8[nk+1] = 0.;
  
  //Thermodynamic levels
  for( k = 0; k < nk; k++ ){
    a_t_8[k] = 0.5*( a_m_8[k] + a_m_8[k+1] );
    b_t_8[k] = 0.5*( b_m_8[k] + b_m_8[k+1] );
    c_t_8[k] = 0.5*( c_m_8[k] + c_m_8[k+1] );
    // Since rcoef* may be big we limit B and C to avoid floating point overflow         
    if( b_t_8[k] < 1.e-16){
      b_t_8[k] = 0.;
    }
    if( c_t_8[k] < 1.e-16){
      c_t_8[k] = 0.;
    }
  }
  a_t_8[nk]   = 0.;
  c_t_8[nk]   = 0.;
  b_t_8[nk]   = 1.;
  a_t_8[nk+1] = dht;
  b_t_8[nk+1] = 1.;
  c_t_8[nk+1] = 0.;
  
  for( k = 0; k < nk; k++ ){
    hybm[k] = hybuser[k];
  }
  for( k = 0; k < nk-1; k++ ){
    hybt[k] = 0.5f * ( hybm[k] + hybm[k+1] );
  } 
  hybt[nk-1] = 0.5f * hybm[nk-1];
  
  hybt[nk] = 0.;
  hybm[nk] = 0.;

  //Compute ip1 values
  for( k = 0; k < nk+1; k++ ){
    ip1_m[k] = c_convip_Level2IP( hybm[k], 21 );
    ip1_t[k] = c_convip_Level2IP( hybt[k], 21 );
  }
  
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_m[nk+1] = c_convip_Level2IP( dhm, 4 );
  ip1_t[nk+1] = c_convip_Level2IP( dht, 4 );
  
  return(VGD_OK);
}

void vgrid_21001::set_table_nj(int nk)
{
  table_nj = this->nl_m + this->nl_t + skip;
}

int vgrid_21001::c_decode_vert()
{
  int k, ind, nb, kind;

  this->skip    =   (int) this->table[2];
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

int vgrid_21001::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  this->table[3] = this->rcoef1;  
  this->table[4] = this->rcoef2;
  this->table[5] = this->rcoef3;  
  this->table[6] = this->rcoef4;
  flip_transfer_c2d(this->ref_name,  &(this->table[7]));
  flip_transfer_c2d(this->ref_namel, &(this->table[8]));
  this->table[9] = 0.; 
  this->table[10]= 0.; 
  this->table[11]= 0.; 

  int k, ind = 12;
  for ( k = 0; k < this->nl_m; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    this->table[ind+3] = this->c_m_8[k];
    ind = ind + 4;
  }
  for ( k = 0; k < this->nl_t; k++){
    this->table[ind  ] = this->ip1_t[k];
    this->table[ind+1] = this->a_t_8[k];
    this->table[ind+2] = this->b_t_8[k];
    this->table[ind+3] = this->c_t_8[k];
    ind = ind + 4;
  }
  this->nl_w = this->nl_t;
  this->a_w_8 = this->a_t_8;
  this->b_w_8 = this->b_t_8;
  this->c_w_8 = this->c_t_8;
  this->ip1_w = this->ip1_t;

  this->valid = 1;

  return(VGD_OK);
}

void vgrid_21001::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  if( this->Cvgd_is_valid("ref_namel_valid" ) )
  {
    strcpy(h->etiket,"HYB_H_CP_SLV");
  }
  else
  {
    strcpy(h->etiket,"HYB_H_CP");
  }
  h->ig2=0;
  h->ig3=0;
  h->ig4=0;
};


// ########## class 21002 ##########
vgrid_21002::vgrid_21002() : vgrid()
{
  this->kind    = 21;
  this->version = 2;
  this->vcode   = 21002;
  this->skip    = 3; // Could be changed by c_decode_vert
  this->table_ni = 4;

  // To have a complete set of levels parameters, we need all momentum and Vertical-Velocity
  // levels, the thermo levels set only differs for the diag level. Therefor we only write
  // Momentum, Vertical-Velocity and the thermo diag level
  strcpy(this->ref_name,"ME  ");
  strcpy(this->ref_namel,"MELS");
  if (   fabs( this->rcoef3 - this->rcoef1 ) < 1.0e-6
      && fabs( this->rcoef4 - this->rcoef2 ) < 1.0e-6)
  {
    strcpy(this->ref_namel,VGD_NO_REF_NOMVAR);
  }
  if ( this->rcoef3 < 0. || this->rcoef4 < 0. )
  {
    strcpy(this->ref_namel,VGD_NO_REF_NOMVAR);
  }
}

vgrid_21002(int key) : vgrid_21002() : vgrid(key)
{
};

int vgrid_21002::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, int *nl_w, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, double **PP_a_w_8, double **PP_b_w_8, double **PP_c_w_8, int **PP_ip1_w, float dhm, float dht, float dhw)
{
  // Note: momentum and thermo levels are the same except for the diag level. We define a complete set for both for simplicity.
  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *c_m_8, *a_t_8, *b_t_8, *c_t_8, *a_w_8, *b_w_8, *c_w_8;
  int *ip1_m, *ip1_t, *ip1_w;
  char ok = 1;
  int k;
  float rcoef, my_rcoef3, my_rcoef4, hybm[nk+2], hybw[nk+2];
  double lamda_8, pr1;

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] >= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY DECREASING\n");
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

  *nl_m = nk + 2;
  *nl_t = nk + 2;
  *nl_w = nk + 2;
  
  *PP_a_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_c_m_8 = (double*)malloc( (*nl_m)*sizeof(double) );
  if(! *PP_c_m_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_c_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = (int*)malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_c_t_8 = (double*)malloc( (*nl_t)*sizeof(double) );
  if(! *PP_c_t_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_c_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = (int*)malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }
  *PP_a_w_8 = (double*)malloc( (*nl_w)*sizeof(double) );
  if(! *PP_a_w_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_a_w_8\n");
    return(VGD_ERROR);
  }
  *PP_b_w_8 = (double*)malloc( (*nl_w)*sizeof(double) );
  if(! *PP_b_w_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_b_w_8\n");
    return(VGD_ERROR);
  }
  *PP_c_w_8 = (double*)malloc( (*nl_w)*sizeof(double) );
  if(! *PP_c_w_8){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_c_w_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_w = (int*)malloc( (*nl_w)*sizeof(int) );
  if(! *PP_ip1_w){
    printf("(Cvgd) ERROR in vgrid_21002::C_genab, malloc error with *PP_ip1_w\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  c_m_8 = *PP_c_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  c_t_8 = *PP_c_t_8;
  ip1_t = *PP_ip1_t;
  a_w_8 = *PP_a_w_8;
  b_w_8 = *PP_b_w_8;
  c_w_8 = *PP_c_w_8;
  ip1_w = *PP_ip1_w;

  if ( rcoef3 < 0. ){
    if( rcoef4 >= 0. ){
      printf("(Cvgd) ERROR in vgrid_21002::C_genab, rcoef4 should not bet set since rcoef3 is not set\n");
      return(VGD_ERROR);
    }
  }
  if ( rcoef4 < 0. ){
    if( rcoef3 >= 0. ){
      printf("(Cvgd) ERROR in vgrid_21002::C_genab, rcoef3 should not bet set since rcoef4 is not set\n");
      return(VGD_ERROR);
    }
  }
  if ( rcoef3 < 0. ){
    my_rcoef3 = rcoef1;
    my_rcoef4 = rcoef2;
  } else {
    my_rcoef3 = rcoef3;
    my_rcoef4 = rcoef4;
  }

  //Momentum levels
  // Note: hybuser at surface in not in the list but we know it is zero
  pr1 = 1. / hybuser[0];
  for( k = 0; k < nk; k++ ){
    lamda_8 = ( hybuser[0] - hybuser[k] ) * pr1;
    rcoef   = (float) (my_rcoef4 - ( my_rcoef4 - my_rcoef3 ) * lamda_8);
    a_m_8[k] = (double)hybuser[k];
    b_m_8[k] = pow(lamda_8, rcoef);
    rcoef   = (float) ( rcoef2 - ( rcoef2 - rcoef1 ) * lamda_8);
    c_m_8[k] = pow(lamda_8, rcoef) - b_m_8[k];
    //Since rcoef* may be big we limit B and C to avoid floating point overflow
    if(b_m_8[k] < 1.e-16){
      b_m_8[k] = 0.;
    }
    if(c_m_8[k] < 1.e-16){
      c_m_8[k] = 0.;
    }
  }
  a_m_8[nk]   = 0.;
  b_m_8[nk]   = 1.;
  c_m_8[nk]   = 0.;
  a_m_8[nk+1] = dhm;
  b_m_8[nk+1] = 1.;
  c_m_8[nk+1] = 0.;
  
  //Vertical-Velocity levels
  for( k = 0; k < nk-1; k++ ){
    a_w_8[k] = 0.5*( a_m_8[k] + a_m_8[k+1] );
    b_w_8[k] = 0.5*( b_m_8[k] + b_m_8[k+1] );
    c_w_8[k] = 0.5*( c_m_8[k] + c_m_8[k+1] );
    // Since rcoef* may be big we limit B and C to avoid floating point overflow         
    if( b_w_8[k] < 1.e-16){
      b_w_8[k] = 0.;
    }
    if( c_w_8[k] < 1.e-16){
      c_w_8[k] = 0.;
    }
  }
  // In order to have the same number of level as momentum, we put two levels at the surface
  // Level at surface
  a_w_8[nk-1]   = 0.;
  b_w_8[nk-1]   = 1.;
  c_w_8[nk-1]   = 0.;
  a_w_8[nk]   = 0.;
  b_w_8[nk]   = 1.;
  c_w_8[nk]   = 0.;
  // Diag level
  a_w_8[nk+1] = dhw;
  b_w_8[nk+1] = 1.;
  c_w_8[nk+1] = 0.;
  
  //Thermo levels
  for( k = 0; k <= nk+1; k++ ){
    a_t_8[k] = a_m_8[k];
    b_t_8[k] = b_m_8[k];
    c_t_8[k] = c_m_8[k];
  }
  a_t_8[nk+1] = dht;

  for( k = 0; k < nk; k++ ){
    hybm[k] = hybuser[k];
  }
  for( k = 0; k < nk-1; k++ ){
    hybw[k] = 0.5f * ( hybm[k] + hybm[k+1] );
  } 
  hybw[nk-1] = 0.;
  hybw[nk] = 0.;
  hybm[nk] = 0.;

  //Compute ip1 values
  for( k = 0; k <= nk; k++ ){
    ip1_m[k] = c_convip_Level2IP( hybm[k], 21 );
    ip1_w[k] = c_convip_Level2IP( hybw[k], 21 );
    ip1_t[k] = ip1_m[k];
  }

  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_m[nk+1] = c_convip_Level2IP( dhm, 4 );
  ip1_t[nk+1] = c_convip_Level2IP( dht, 4 );
  ip1_w[nk+1] = c_convip_Level2IP( dhw, 4 );

  return(VGD_OK);
}

void vgrid_21002::set_table_nj(int nk)
{
  table_nj = this->nl_m + this->nl_w + 1 + skip;
}

int vgrid_21002::c_decode_vert()
{
  int k, ind, nb, kind;

  this->skip    =   (int) this->table[2];
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

int vgrid_21002::c_encode_vert()
{
  //Fill header
  this->table[0] = this->kind;
  this->table[1] = this->version;
  this->table[2] = this->skip;
  this->table[3] = this->rcoef1;  
  this->table[4] = this->rcoef2;
  this->table[5] = this->rcoef3;  
  this->table[6] = this->rcoef4;
  flip_transfer_c2d(this->ref_name, &(this->table[7]));
  flip_transfer_c2d(this->ref_namel, &(this->table[8]));
  this->table[9] = 0.; 
  this->table[10]= 0.; 
  this->table[11]= 0.; 

  int k, ind = 12;
  for ( k = 0; k < this->nl_m; k++){
    this->table[ind  ] = this->ip1_m[k];
    this->table[ind+1] = this->a_m_8[k];
    this->table[ind+2] = this->b_m_8[k];
    this->table[ind+3] = this->c_m_8[k];
    ind = ind + 4;
  }
  for ( k = 0; k < this->nl_w; k++){
    this->table[ind  ] = this->ip1_w[k];
    this->table[ind+1] = this->a_w_8[k];
    this->table[ind+2] = this->b_w_8[k];
    this->table[ind+3] = this->c_w_8[k];
    ind = ind + 4;
  }
  // Thermo diag level
  this->table[ind  ] = this->ip1_t[this->nl_t-1];
  this->table[ind+1] = this->a_t_8[this->nl_t-1];
  this->table[ind+2] = this->b_t_8[this->nl_t-1];
  this->table[ind+3] = this->c_t_8[this->nl_t-1];
  
  this->valid = 1;

  return(VGD_OK);
}

void vgrid_21002::fstd_subinit()
{
  VGD_TFSTD *h = &this->rec;
  strcpy(h->etiket,"HYB_H_LORENZ");
  h->ig2=0;
  h->ig3=0;
  h->ig4=0;
};
