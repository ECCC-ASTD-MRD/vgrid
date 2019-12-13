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

#ifndef VGRID_5005_H
#define VGRID_5005_H

#include "vgrid.hpp"

class vgrid_0001 : public vgrid
{
public:
  vgrid_0001();
  vgrid_0001(int key) : vgrid_0001() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  void fstd_subinit();
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001();
  vgrid_1001(int key) : vgrid_1001() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  void fstd_subinit();
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002();
  vgrid_1002(int key) : vgrid_1002() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m);
  void fstd_subinit();
};

class vgrid_1003_5001 : public vgrid
{
public:
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_1003 : public vgrid_1003_5001
{
public:
  vgrid_1003();
  vgrid_1003(int key) : vgrid_1003() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};
  void fstd_subinit();

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001();
  vgrid_2001(int key) : vgrid_2001() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  void fstd_subinit();
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001();
  vgrid_4001(int key) : vgrid_4001() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  void fstd_subinit();
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001();
  vgrid_5001(int key) : vgrid_5001() : vgrid(key);
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};
  void fstd_subinit();

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  int c_decode_vert();
  int c_encode_vert();
protected:
  int C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift);
};

class vgrid_5002 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5002();
  vgrid_5002(int key) : vgrid_5002() : vgrid(key);
  void fstd_subinit();
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003();
  vgrid_5003(int key) : vgrid_5003() : vgrid(key);
  void fstd_subinit();
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004();
  vgrid_5004(int key) : vgrid_5004() : vgrid(key);
  int vgrid_5004::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
  void fstd_subinit();
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005();
  vgrid_5005(int key) : vgrid_5005() : vgrid(key);
  int vgrid_5005::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht);
  void fstd_subinit();
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100();
  vgrid_5100(int key) : vgrid_5100() : vgrid(key);
  int vgrid_5100::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht, int avg);
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999();
  vgrid_5999(int key) : vgrid_5999() : vgrid(key);
  int c_decode_vert();
  int c_encode_vert();
  void fstd_subinit();
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001();
  vgrid_21001(int key) : vgrid_21001() : vgrid(key);
  int vgrid_21001::C__genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht);
  int c_decode_vert();
  int c_encode_vert();
  void fstd_subinit();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002();
  vgrid_21002(int key) : vgrid_21002() : vgrid(key);
  int vgrid_21002::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, int *nl_w, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, double **PP_a_w_8, double **PP_b_w_8, double **PP_c_w_8, int **PP_ip1_w, float dhm, float dht, float dhw);
  int c_decode_vert();
  int c_encode_vert();
  void fstd_subinit();
};

#endif // VGRID_5005_H
