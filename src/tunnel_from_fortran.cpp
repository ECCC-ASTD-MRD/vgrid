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


#include "vgrid.hpp"
#include "tunnel_from_fortran.hpp"

int Cvgd_new_read(vgrid_descriptor **vgd, int unit, int ip1,int ip2, 
                  int kind, int version)
{
  vgrid::Cvgd_new_read(vgd, unit, ip1, ip2, kind, version);
}


int Cvgd_new_from_table(vgrid_descriptor **vgd, double *table,
                        int ni, int nj, int nk)
{
  vgrid::Cvgd_new_from_table(vgd, table, ni, nj, nk);
};


int Cvgd_new_gen2(vgrid_descriptor **self, int kind, int version, float *hyb,
                  int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3,                      float *rcoef4, double *ptop_8, double *pref_8,
                  double *ptop_out_8, int ip1, int ip2, float *dhm, float *dht,
                  float *dhw, int avg)
{
  vgrid::Cvgd_new_gen2(self, kind, version, hyb,
                  size_hyb, rcoef1, rcoef2, rcoef3,
                  rcoef4, ptop_8, pref_8,
                  ptop_out_8, ip1, ip2, dhm, dht,
                  dhw, avg);
}

int Cvgd_new_build_vert2(vgrid_descriptor **vgd, int kind, int version, int nk,
                         int ip1, int ip2, double *ptop_8, double *pref_8,
                         float *rcoef1, float *rcoef2, float *rcoef3,
                         float *rcoef4, double *a_m_8, double *b_m_8,
                         double *c_m_8, double *a_t_8, double *b_t_8,
                         double *c_t_8, double *a_w_8, double *b_w_8,
                         double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w,
                         int nl_m, int nl_t, int nl_w)
{
  vgrid::Cvgd_new_build_vert2(vgd, kind, version,
                              nk, ip1, ip2, ptop_8,
                              pref_8, rcoef1, rcoef2,
                              rcoef3, rcoef4, a_m_8,
                              b_m_8, c_m_8, a_t_8,
                              b_t_8, c_t_8, a_w_8,
                              b_w_8, c_w_8, ip1_m,
                              ip1_t, ip1_w, nl_m, nl_t,
                              nl_w);
};

int Cvgd_diag_withref_2ref(vgrid_descriptor *vgd, int ni, int nj, int nk,
                           int *ip1_list, float *levels, float *sfc_field,
                           float *sfc_field_ls, int in_log, int dpidpis)
{
  vgrid::Cvgd_diag_withref_2ref(vgd, ni, nj, nk,
                                ip1_list, levels, sfc_field,
                                sfc_field_ls, in_log, dpidpis);
};

int Cvgd_diag_withref_2ref_8(vgrid_descriptor *vgd, int ni, int nj, int nk,
                           int *ip1_list,double *levels_8,double *sfc_field_8,
                           double *sfc_field_ls_8, int in_log, int dpidpis)
{
   vgrid::Cvgd_diag_withref_2ref_8(vgd, ni, nj, nk,
                                   ip1_list, levels_8, sfc_field_8,
                                   sfc_field_ls_8, in_log, dpidpis);
};

int Cvgd_get_int(vgrid_descriptor *vgd, char *key, int *value, int quiet)
{
  vgrid::Cvgd_get_int(vgd, key, value, quiet);
};

int Cvgd_get_int_1d(vgrid_descriptor *vgd, char *key, int **value, int *nk,
                    int quiet)
{
  vgrid::Cvgd_get_int_1d(vgd, key, value, nk, quiet);
};

int Cvgd_get_float(vgrid_descriptor *vgd, char *key, float *value,
                   int quiet)
{
  vgrid::Cvgd_get_float(vgd, key, value, quiet);
};

int Cvgd_get_float_1d(vgrid_descriptor *vgd, char *key, float **value, int *nk, int quiet)
{
  vgrid::Cvgd_get_float_1d(vgd, key, value, nk, quiet);
};

int Cvgd_get_double(vgrid_descriptor *vgd, char *key, double *value, int quiet)
{
  vgrid::Cvgd_get_double(vgd, key, value, quiet);
};

int Cvgd_get_double_1d(vgrid_descriptor *vgd, char *key, double **value, 
                       int *nk, int quiet)
{
  vgrid::Cvgd_get_double_1d(vgd, key, value, nk, quiet);
};

int Cvgd_get_double_3d(vgrid_descriptor *vgd, char *key, double **value, int *ni, int *nj, int *nk, int quiet)
{
  vgrid::Cvgd_get_double_3d(vgd, key, value, ni, nj, nk, quiet);
};

int Cvgd_get_char(vgrid_descriptor *vgd, char *key, char *my_char, int quiet)
{
  vgrid::Cvgd_get_char(vgd, key, my_char, quiet);
};

int Cvgd_getopt_int(char *key, int *value, int quiet)
{
  vgrid::Cvgd_getopt_int(key, value, quiet);
};

int Cvgd_print_desc(vgrid_descriptor *self, int sout, int convip)
{
  vgrid::Cvgd_print_desc(self, sout, convip);
};

int Cvgd_print_vcode_description(int vcode)
{
  vgrid::Cvgd_print_vcode_description(vcode);
};

int Cvgd_put_char(vgrid_descriptor **self, char *key, char *value)
{
  vgrid::Cvgd_put_char(self, key, value);
};

int Cvgd_put_int(vgrid_descriptor **self, char *key, int value)
{
  vgrid::Cvgd_put_int(self, key, value);
};

int Cvgd_putopt_int(char *key, int value)
{
  vgrid::Cvgd_putopt_int(key, value);
};

int Cvgd_is_valid(vgrid_descriptor *vgd, char *valid_table_name)
{
  vgrid::Cvgd_is_valid(vgd, valid_table_name);
};

int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb)
{
  vgrid::Cvgd_stda76_hgts_from_pres_list(hgts, pres, nb);
};

int Cvgd_stda76_pres(vgrid_descriptor *self, int *i_val, int nl, float *pres,
                     float *sfc_temp, float *sfc_pres)
{
  vgrid::Cvgd_stda76_pres(self, i_val, nl, pres, sfc_temp, sfc_pres);
}

int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb)
{
  vgrid::Cvgd_stda76_pres_from_hgts_list(pres, hgts, nb);
};

int Cvgd_stda76_temp(vgrid_descriptor *self, int *i_val, int nl, float *temp)
{
  vgrid::Cvgd_stda76_temp(self, i_val, nl, temp);
};

void Cvgd_table_shape(vgrid_descriptor *vgd, int *tshape)
{
  vgrid::Cvgd_table_shape(vgd, &tshape);
};

int Cvgd_vgdcmp(vgrid_descriptor *vgd1, vgrid_descriptor *vgd2)
{
  vgrid::Cvgd_vgdcmp(vgd1, vgd2);
};

int Cvgd_write_desc(vgrid_descriptor *self, int unit)
{
  vgrid::Cvgd_write_desc(self, unit);
};
