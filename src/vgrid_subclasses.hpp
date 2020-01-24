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

class vgrid_0001 : public vgrid
{
public:
  vgrid_0001();
  vgrid_0001(int key);
  int c_decode_vert();
  int c_encode_vert();
//  int Cvgd_build_from_ab(int ip1, int ip2, double *a_m_8, double *b_m_8,
//                         double *a_w_8, double *b_w_8, int *ip1_m, int *ip1_w,
//                         int nl_m, int nl_t, int nl_w)
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001();
  vgrid_1001(int key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_build_from_ab(int ip1, int ip2, double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, int ip1, int ip2);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002();
  vgrid_1002(int key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *etauser, int nk, double ptop_8, double **a_m_8, double **b_m_8,
	      int **ip1_m);
  int Cvgd_build_from_ab(int ip1, int ip2, double ptop_8, double *a_m_8,
			 double *b_m_8, int *ip1_m, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, double ptop_8,
				int ip1, int ip2);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_1003_5001 : public vgrid
{
public:
  vgrid_1003_5001();
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_build_from_ab(int ip1, int ip2, double ptop_8, double pref_8, float rcoef1,
			 double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
private:
  void set_table_nj(int nk);
};

class vgrid_1003 : public vgrid_1003_5001
{
public:
  vgrid_1003();
  vgrid_1003(int key);
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
private:
  void fstd_subinit();
};

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001();
  vgrid_2001(int key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_build_from_ab(int ip1, int ip2, double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, int ip1, int ip2);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001();
  vgrid_4001(int key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int Cvgd_build_from_ab(int ip1, int ip2, double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001();
  vgrid_5001(int key);
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  vgrid_5002_5003_5004_5005();
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
  vgrid_5002();
  vgrid_5002(int key);
  int Cvgd_build_from_ab(int ip1, int ip2, double ptop_8, double pref_8,
			 float rcoef1, float rcoef2, double *a_m_8,
			 double *b_m_8, double *a_t_8, double *b_t_8,
			 int *ip1_m, int *ip1_t,
			 int nl_m, int nl_t);
private:
  void fstd_subinit();
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003();
  vgrid_5003(int key);
private:
  void fstd_subinit();
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004();
  vgrid_5004(int key);
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
private:
  void fstd_subinit();
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005();
  vgrid_5005(int key);
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht);
  int Cvgd_build_from_ab(int ip1, int ip2, double pref_8, float rcoef1, float rcoef2,
                         double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
private:
  void fstd_subinit();
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100();
  vgrid_5100(int key);
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht, int avg);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_build_from_ab(int ip1, int ip2, double pref_8, float rcoef1,
			 float rcoef2, float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, float rcoef1,
				float rcoef2, float rcoef3, float rcoef4,
				double pref_8, double *ptop_out_8, int ip1,
				int ip2, float *dhm, float *dht, int avg);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999();
  vgrid_5999(int key);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_build_from_ab(int ip1, int ip2, double *a_m_8, double *b_m_8,
			 int *ip1_m, int nl_m);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001();
  vgrid_21001(int key);
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_build_from_ab(int ip1, int ip2, float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, float rcoef1,
				float rcoef2, int ip1, int ip2, float *dhm,
				float *dht, float rcoef3,
				float rcoef4);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002();
  vgrid_21002(int key);
  int C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, int *nl_w, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, double **PP_a_w_8, double **PP_b_w_8, double **PP_c_w_8, int **PP_ip1_w, float dhm, float dht, float dhw);
  int c_decode_vert();
  int c_encode_vert();
  int Cvgd_build_from_ab(int ip1, int ip2, float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 double *a_w_8, double *b_w_8, double *c_w_8,
			 int *ip1_m, int *ip1_t, int *ip1_w, int nl_m);
  int Cvgd_build_vgrid_from_hyb(float *hyb, int size_hyb, float rcoef1,
				float rcoef2, int ip1, int ip2, float *dhm,
				float *dht, float *dhw, float rcoef3=-1,
				float rcoef4=-1);
private:
  void fstd_subinit();
  void set_table_nj(int nk);
  void set_refnames();
};

#endif // VGRID_SUBCLASSES_H
