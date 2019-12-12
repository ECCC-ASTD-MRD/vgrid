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
  vgrid_0001(int key) : vgrid_0001();
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001();
  vgrid_1001(int key) : vgrid_1001();
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002();
  vgrid_1002(int key) : vgrid_1002();
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_1003_5001 : public vgrid
{
public:
  vgrid_1003_5001(int key);
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_1003 : public vgrid_1003_5001
{
public:
  vgrid_1003();
  vgrid_1003(int key);
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001();
  vgrid_2001(int key) : vgrid_2001();
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001();
  vgrid_4001(int key) : vgrid_4001();
  int c_decode_vert();
  int c_encode_vert();
  int C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001();
  vgrid_5001(int key) : vgrid_5001();
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  vgrid_5002_5003_5004_5005(int key, int k_plus_top_value);
  int c_decode_vert();
  int c_encode_vert();
protected:
  int C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift);
private:
  int k_plus_top;
};

class vgrid_5002 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5002();
  vgrid_5002(int key) : vgrid_5002();
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003();
  vgrid_5003(int key) : vgrid_5003();
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004();
  vgrid_5004(int key) : vgrid_5004();
  int vgrid_5004::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005();
  vgrid_5005(int key) : vgrid_5005();
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100();
  vgrid_5100(int key) : vgrid_5100();
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999();
  vgrid_5999(int key) : vgrid_5999();
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001();
  vgrid_21001(int key) : vgrid_21001();
  int c_decode_vert();
  int c_encode_vert();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002();
  vgrid_21002(int key) : vgrid_21002();
  int c_decode_vert();
  int c_encode_vert();
};

#endif // VGRID_5005_H
