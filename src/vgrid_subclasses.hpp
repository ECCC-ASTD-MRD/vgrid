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

#ifndef VGRID_SUBCLASSES_H
#define VGRID_SUBCLASSES_H

#include "vgrid.hpp"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

class vgrid_0001 : public vgrid
{
public:
  vgrid_0001(int ip1=-1, int ip2=-1);
  vgrid_0001(int key);
  vgrid_0001(const vgrid_0001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(double *a_m_8, double *b_m_8,
                          double *a_w_8, double *b_w_8, int *ip1_m, int *ip1_w,
                          int nl_m, int nl_w);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001(int ip1=-1, int ip2=-1);
  vgrid_1001(int key);
  vgrid_1001(const vgrid_1001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_create_from_ab(double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002(int ip1=-1, int ip2=-1);
  vgrid_1002(int key);
  vgrid_1002(const vgrid_1002 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *etauser, int nk, double ptop_8, double **a_m_8, double **b_m_8,
	      int **ip1_m);
  int Cvgd_create_from_ab(double ptop_8, double *a_m_8, double *b_m_8, int *ip1_m,
			  int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, double ptop_8);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1003_5001 : public vgrid
{
public:
  vgrid_1003_5001(int ip1=-1, int ip2=-1);
  vgrid_1003_5001(const vgrid_1003_5001 *original);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(double ptop_8, double pref_8, float rcoef1,
			 double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
private:
  void set_table_nj(int nk);
};

class vgrid_1003 : public vgrid_1003_5001
{
public:
  vgrid_1003(int ip1=-1, int ip2=-1);
  vgrid_1003(int key);
  vgrid_1003(const vgrid_1003 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   double ptop_8, double pref_8);
private:
  void fstd_subinit();
};

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001(int ip1=-1, int ip2=-1);
  vgrid_2001(int key);
  vgrid_2001(const vgrid_2001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_create_from_ab(double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001(int ip1=-1, int ip2=-1);
  vgrid_4001(int key);
  vgrid_4001(const vgrid_4001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_create_from_ab(double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001(int ip1=-1, int ip2=-1);
  vgrid_5001(int key);
  vgrid_5001(const vgrid_5001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   double ptop_8, double pref_8);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  vgrid_5002_5003_5004_5005(int ip1=-1, int ip2=-1);
  vgrid_5002_5003_5004_5005(const vgrid_5002_5003_5004_5005 *original);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift);
private:
  void set_table_nj(int nk);
  void set_refnames();
};

class vgrid_5002 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5002(int ip1=-1, int ip2=-1);
  vgrid_5002(int key);
  vgrid_5002(const vgrid_5002 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int Cvgd_create_from_ab(double ptop_8, double pref_8,
			 float rcoef1, float rcoef2, double *a_m_8,
			 double *b_m_8, double *a_t_8, double *b_t_8,
			 int *ip1_m, int *ip1_t,
			 int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, double ptop_8, double pref_8);
private:
  void fstd_subinit();
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003(int ip1=-1, int ip2=-1);
  vgrid_5003(int key);
  vgrid_5003(const vgrid_5003 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int Cvgd_create_from_ab(double ptop_8, double pref_8,
			 float rcoef1, float rcoef2, double *a_m_8,
			 double *b_m_8, double *a_t_8, double *b_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, double ptop_8, double pref_8);
private:
  void fstd_subinit();
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004(int ip1=-1, int ip2=-1);
  vgrid_5004(int key);
  vgrid_5004(const vgrid_5004 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
  int Cvgd_create_from_ab(double ptop_8, double pref_8, float rcoef1,
			 float rcoef2, double *a_m_8, double *b_m_8,
			 double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t,
			 int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, double ptop_8, double pref_8);
private:
  void fstd_subinit();
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005(int ip1=-1, int ip2=-1);
  vgrid_5005(int key);
  vgrid_5005(const vgrid_5005 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht);
  int Cvgd_create_from_ab(double pref_8, float rcoef1, float rcoef2,
                         double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, double pref_8,
			   double *ptop_out_8, float *dhm, float *dht);
private:
  void fstd_subinit();
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100(int ip1=-1, int ip2=-1);
  vgrid_5100(int key);
  vgrid_5100(const vgrid_5100 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht, int avg);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(double pref_8, float rcoef1,
			 float rcoef2, float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, float rcoef3, float rcoef4,
			   double pref_8, double *ptop_out_8,
			   float *dhm, float *dht, int avg);
  int C_compute_pressures_5100_float(int ni, int nj, int nk, int *ip1_list,
				     float *levels,
				     float *sfc_field,
				     float *sfc_field_ls,
				     int in_log);
  int C_compute_pressures_5100_double(int ni, int nj, int nk, int *ip1_list,
				      double *levels,
				      double *sfc_field,
				      double *sfc_field_ls,
				      int in_log);

public:
  template<class FloatPrecision>
  int C_compute_pressures_dpidpis_5100(int ni, int nj, int nk, int *ip1_list,
			               FloatPrecision *levels,
			               FloatPrecision *sfc_field,
			               FloatPrecision *sfc_field_ls,
			               int in_log, int dpidpis);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999(int ip1=-1, int ip2=-1);
  vgrid_5999(int key);
  vgrid_5999(const vgrid_5999 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hyb, int size_hyb, double **a_m_8, double **b_m_8, int **ip1_m);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(double *a_m_8, double *b_m_8,
			 int *ip1_m, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001(int ip1=-1, int ip2=-1);
  vgrid_21001(int key);
  vgrid_21001(const vgrid_21001 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
			   float rcoef2, float *dhm,
			   float *dht, float rcoef3, float rcoef4);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002(int ip1=-1, int ip2=-1);
  vgrid_21002(int key);
  vgrid_21002(const vgrid_21002 *original);
  vgrid *clone();     // returns the clone as a pointer to the base class
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, int *nl_w, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, double **PP_a_w_8, double **PP_b_w_8, double **PP_c_w_8, int **PP_ip1_w, float dhm, float dht, float dhw);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_create_from_ab(float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 double *a_w_8, double *b_w_8, double *c_w_8,
			 int *ip1_m, int *ip1_t, int *ip1_w, int nl_m);
  int Cvgd_create_from_hyb(float *hyb, int size_hyb, float rcoef1,
				float rcoef2, float *dhm,
				float *dht, float *dhw, float rcoef3=-1,
				float rcoef4=-1);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};


// DEFINITIONS OF TEMPLATE FUNCTIONS
// Can these go in the *.cpp, since they are private?

template<class FloatPrecision>
int vgrid_5100::C_compute_pressures_dpidpis_5100(int ni, int nj, int nk, int *ip1_list,
					         FloatPrecision *levels,
					         FloatPrecision *sfc_field,
					         FloatPrecision *sfc_field_ls,
					         int in_log=0, int dpidpis=0)
{
  char proc_name[] = "C_compute_pressure_5100";
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
      levels[ijk] = (FloatPrecision) (in_log ? lvl : exp(lvl));
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
	levels[ijk] = (FloatPrecision) bb_8[k]*levels[ijk]/sfc_field[ij];
      }
    }
  }
  
  free(s_8);
  free(sl_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
}

#endif // VGRID_SUBCLASSES_H
