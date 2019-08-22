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
