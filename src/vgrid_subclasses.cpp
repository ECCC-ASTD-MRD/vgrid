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


// ########## class 1002 ##########
vgrid_1001::vgrid_1002(int key) : vgrid(key)
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
vgrid_1003::vgrid_1003(int key) : vgrid_1003_5001(key)
{
}


// ########## class 2001 ##########
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


// ########## class 4001 ##########
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


// ########## class 5001 ##########
vgrid_5001::vgrid_5001(int key) : vgrid_1003_5001(key)
{
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

// ########## class 5002 ##########
vgrid_5002::vgrid_5002(int key) : vgrid_5002_5003_5004_5005(key, 1)
{
}

// ########## class 5003 ##########
vgrid_5003::vgrid_5003(int key) : vgrid_5002_5003_5004_5005(key, 1)
{
}

// ########## class 5004 ##########
vgrid_5004::vgrid_5004(int key) : vgrid_5002_5003_5004_5005(key, 0)
{
}

// ########## class 5005 ##########
vgrid_5005::vgrid_5005(int key) : vgrid_5002_5003_5004_5005(key, 0)
{
}


// ########## class 5100 ##########
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
