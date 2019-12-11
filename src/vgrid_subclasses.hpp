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
  vgrid_0001(int key);
  int c_decode_vert();
};

class vgrid_1001 : public vgrid
{
public:
  vgrid_1001(int key);
  int c_decode_vert();
  int C_genab(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_1002 : public vgrid
{
public:
  vgrid_1002(int key);
  int c_decode_vert();
  int C_genab(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_1003_5001 : public vgrid
{
public:
  vgrid_1003_5001(int key);
  int c_decode_vert();
};

class vgrid_1003 : public vgrid_1003_5001
{
public:
  vgrid_1003(int key);
  int c_decode_vert();
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_2001 : public vgrid
{
public:
  vgrid_2001(int key);
  int c_decode_vert();
  int C_genab(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_4001 : public vgrid
{
public:
  vgrid_4001(int key);
  int c_decode_vert();
  int C_genab(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_5001 : public vgrid_1003_5001
{
public:
  vgrid_5001(int key);
  int C_genab(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
};

class vgrid_5002_5003_5004_5005 : public vgrid
{
public:
  vgrid_5002_5003_5004_5005(int key, int k_plus_top_value);
  int c_decode_vert();
protected:
  int C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift);
private:
  int k_plus_top;
};

class vgrid_5002 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5002(int key);
};

class vgrid_5003 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5003(int key);
};

class vgrid_5004 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5004(int key);
  int vgrid_5004::C_genab(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
};

class vgrid_5005 : public vgrid_5002_5003_5004_5005
{
public:
  vgrid_5005(int key);
};

class vgrid_5100 : public vgrid
{
public:
  vgrid_5100(int key);
  int c_decode_vert();
};

class vgrid_5999 : public vgrid
{
public:
  vgrid_5999(int key);
  int c_decode_vert();
};

class vgrid_21001 : public vgrid
{
public:
  vgrid_21001(int key);
  int c_decode_vert();
};

class vgrid_21002 : public vgrid
{
public:
  vgrid_21002(int key);
  int c_decode_vert();
};

#endif // VGRID_5005_H
