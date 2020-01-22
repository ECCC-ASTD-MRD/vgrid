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

// This file defines the C-style interface to the vgrid package.
// Being C style, this interface is callable from fortran.


#include "coat_check.hpp"
#include "vgrid_creators.hpp"
#include "vgrid.hpp"
#include "vgrid_descriptor.h"
#include "tunnel_from_fortran.hpp"
#include <stdio.h>

static coat_check grid_check;  // Object for checking in vgrids

int Cvgd_diag_withref_2ref(int vgdid, int ni, int nj, int nk,
                           int *ip1_list, float *levels, float *sfc_field,
                           float *sfc_field_ls, int in_log, int dpidpis)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_diag_withref_2ref(ni, nj, nk,
                                       ip1_list, levels, sfc_field,
                                       sfc_field_ls, in_log, dpidpis);
};

int Cvgd_diag_withref_2ref_8(int vgdid, int ni, int nj, int nk,
                           int *ip1_list,double *levels_8,double *sfc_field_8,
                           double *sfc_field_ls_8, int in_log, int dpidpis)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_diag_withref_2ref_8(ni, nj, nk,
                                         ip1_list, levels_8, sfc_field_8,
                                         sfc_field_ls_8, in_log, dpidpis);
};

int Cvgd_get_char(int vgdid, char *key, char *my_char, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_char(key, my_char, quiet);
};

int Cvgd_get_double(int vgdid, char *key, double *value, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_double(key, value, quiet);
};

int Cvgd_get_double_1d(int vgdid, char *key, double **value, 
                       int *nk, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_double_1d(key, value, nk, quiet);
};

int Cvgd_get_double_3d(int vgdid, char *key, double **value, int *ni, int *nj, int *nk, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  vgd->Cvgd_get_double_3d(key, value, ni, nj, nk, quiet);
};

int Cvgd_get_float(int vgdid, char *key, float *value,
                   int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_float(key, value, quiet);
};

int Cvgd_get_float_1d(int vgdid, char *key, float **value, int *nk, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_float_1d(key, value, nk, quiet);
};

int Cvgd_get_int(int vgdid, char *key, int *value, int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_int(key, value, quiet);
};

int Cvgd_get_int_1d(int vgdid, char *key, int **value, int *nk,
                    int quiet)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_get_int_1d(key, value, nk, quiet);
};

int Cvgd_getopt_int(char *key, int *value, int quiet)
{
  vgrid::Cvgd_getopt_int(key, value, quiet);
};

int Cvgd_is_valid(int vgdid, char *valid_table_name)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_is_valid(valid_table_name);
};

int Cvgd_new_build_vert2(int *vgdid, int kind, int version, int nk,
                         int ip1, int ip2, double *ptop_8, double *pref_8,
                         float *rcoef1, float *rcoef2, float *rcoef3,
                         float *rcoef4, double *a_m_8, double *b_m_8,
                         double *c_m_8, double *a_t_8, double *b_t_8,
                         double *c_t_8, double *a_w_8, double *b_w_8,
                         double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w,
                         int nl_m, int nl_t, int nl_w)
{
  vgrid *my_vgd;
  int status;
  status=Cvgd_new_build_vert2(&my_vgd, kind, version,
                                      nk, ip1, ip2, ptop_8,
                                      pref_8, rcoef1, rcoef2,
                                      rcoef3, rcoef4, a_m_8,
                                      b_m_8, c_m_8, a_t_8,
                                      b_t_8, c_t_8, a_w_8,
                                      b_w_8, c_w_8, ip1_m,
                                      ip1_t, ip1_w, nl_m, nl_t,
                                      nl_w);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
};


int Cvgd_new_from_table(int *vgdid, double *table,
                        int ni, int nj, int nk)
{
  vgrid *my_vgd;
  int status;

  status=Cvgd_new_from_table(&my_vgd, table, ni, nj, nk);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
};


int Cvgd_new_gen2(int *vgdid, int kind, int version, float *hyb,
                  int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3,                      float *rcoef4, double *ptop_8, double *pref_8,
                  double *ptop_out_8, int ip1, int ip2, float *dhm, float *dht,
                  float *dhw, int avg)
{
  vgrid *my_vgd;
  int status;
  status=Cvgd_new_gen2(&my_vgd, kind, version, hyb,
                       size_hyb, rcoef1, rcoef2, rcoef3,
                       rcoef4, ptop_8, pref_8,
                       ptop_out_8, ip1, ip2, dhm, dht,
                       dhw, avg);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Cvgd_read_vgrid_from_file(int *vgdid, int unit, int ip1,int ip2, 
                              int kind, int version)
{
  vgrid *my_vgd;
  int status;

  status=Cvgd_read_vgrid_from_file(&my_vgd, unit, ip1, ip2, kind, version);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_1001(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                        int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_1001(&my_vgd, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_1002(int *vgdid, int ip1, int ip2, double ptop_8, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_1002(&my_vgd, ip1, ip2, ptop_8, a_m_8, b_m_8, ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_1003(int *vgdid, int ip1, int ip2, double ptop_8, double pref_8,
			float rcoef1, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_1003(&my_vgd, ip1, ip2, ptop_8, pref_8, rcoef1, a_m_8, b_m_8,
			     ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_2001(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
			int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_2001(&my_vgd, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_4001(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                        int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_4001(&my_vgd, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_5001(int *vgdid, int ip1, int ip2, double ptop_8, double pref_8,
			float rcoef1, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_5001(&my_vgd, ip1, ip2, ptop_8, pref_8, rcoef1, a_m_8, b_m_8,
			     ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_5005(int *vgdid, int ip1, int ip2,
			double pref_8, float rcoef1, float rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_5005(&my_vgd, ip1, ip2, pref_8, rcoef1, rcoef2,
                             a_m_8, b_m_8, a_t_8, b_t_8, ip1_m, ip1_t, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_5100(int *vgdid, int ip1, int ip2, double pref_8, float rcoef1,
			float rcoef2, float rcoef3, float rcoef4,
			double *a_m_8, double *b_m_8, double *c_m_8,
			double *a_t_8, double *b_t_8, double *c_t_8,
			int *ip1_m, int *ip1_t, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_5100(&my_vgd, ip1, ip2, pref_8, rcoef1, rcoef2, rcoef3, rcoef4,
			     a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8,
			     ip1_m, ip1_t, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_5999(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
			int *ip1_m, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_5999(&my_vgd, ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_21001(int *vgdid, int ip1, int ip2,
			 float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_21001(&my_vgd, ip1, ip2,
			      rcoef1, rcoef2, rcoef3, rcoef4,
			      a_m_8, b_m_8, c_m_8,a_t_8, b_t_8, c_t_8, ip1_m,
			      ip1_t, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Create_from_ab_21002(int *vgdid, int ip1, int ip2, float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 double *a_w_8, double *b_w_8, double *c_w_8,
			 int *ip1_m, int *ip1_t, int *ip1_w, int nl_m)
{
  vgrid *my_vgd;
  int status;

  status=Create_from_ab_21002(&my_vgd, ip1, ip2, rcoef1, rcoef2,
			      rcoef3, rcoef4,
			      a_m_8, b_m_8, c_m_8,
			      a_t_8, b_t_8, c_t_8,
			      a_w_8, b_w_8, c_w_8,
			      ip1_m, ip1_t, ip1_w, nl_m);
  if(status != VGD_ERROR)
    {
      *vgdid=grid_check.get_tag(my_vgd);
    }
  return status;
}

int Cvgd_print_desc(int vgdid, int sout, int convip)
{
  vgrid *vgd;
  try
    {
      vgd=grid_check.get_vgrid(vgdid);
    }
  catch (int x )
    {
      printf("\n(Cvgd_print_desc) ERROR from grid_check:  null grid for vgdid=%d\n\n",x);
      return VGD_ERROR;
    }
  return vgd->Cvgd_print_desc(sout, convip);
};

int Cvgd_print_vcode_description(int vcode)
{
  vgrid::Cvgd_print_vcode_description(vcode);
};

int Cvgd_put_char(int vgdid, char *key, char *value)
{
  vgrid *vgd;
  try
    {
      vgd=grid_check.get_vgrid(vgdid);
    }
  catch (int x )
    {
      printf("\n(Cvgd_put_char) ERROR from grid_check:  null grid for vgdid=%d\n\n",x);
      return VGD_ERROR;
    }
  return vgd->Cvgd_put_char(key, value);
};

int Cvgd_put_int(int vgdid, char *key, int value)
{
  vgrid *vgd;
  try
    {
    vgd=grid_check.get_vgrid(vgdid);
    }
  catch (int x )
    {
      printf("\n(Cvgd_put_int) ERROR from grid_check:  null grid for vgdid=%d\n\n",x);
      return VGD_ERROR;
    }
  return vgd->Cvgd_put_int(key, value);
};

int Cvgd_putopt_int(char *key, int value)
{
  vgrid::Cvgd_putopt_int(key, value);
};

int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb)
{
  vgrid::Cvgd_stda76_hgts_from_pres_list(hgts, pres, nb);
};

int Cvgd_stda76_pres(int vgdid, int *i_val, int nl, float *pres,
                     float *sfc_temp, float *sfc_pres)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_stda76_pres(i_val, nl, pres, sfc_temp, sfc_pres);
}

int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb)
{
  vgrid::Cvgd_stda76_pres_from_hgts_list(pres, hgts, nb);
};

int Cvgd_stda76_temp(int vgdid, int *i_val, int nl, float *temp)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  return vgd->Cvgd_stda76_temp(i_val, nl, temp);
};

void Cvgd_table_shape(int vgdid, int **tshape)
{
  vgrid *vgd;
  vgd=grid_check.get_vgrid(vgdid);
  vgd->Cvgd_table_shape(tshape);
};

int Cvgd_vgdcmp(int vgdid1, int vgdid2)
{
  vgrid *vgd1, *vgd2;
  vgd1=grid_check.get_vgrid(vgdid1);
  vgd2=grid_check.get_vgrid(vgdid2);
  return vgd1->Cvgd_vgdcmp(vgd2);
};

int Cvgd_write_desc(int vgdid, int unit)
{
  vgrid *vgd;
  try
    {
      vgd=grid_check.get_vgrid(vgdid);
  // if((int)vgd == 0)
  //   {
  //     printf("\n(Cvgd_write_desc) ERROR from grid_check:  null grid for vgdid=%d\n\n",
  // 	     vgdid);
  //     return VGD_ERROR;
  //   }
    }
  catch (int x )
    {
      printf("\n(Cvgd_write_desc) ERROR from grid_check:  null grid for vgdid=%d\n\n",x);
      return VGD_ERROR;
    }
  vgd->Cvgd_write_desc(unit);
};
