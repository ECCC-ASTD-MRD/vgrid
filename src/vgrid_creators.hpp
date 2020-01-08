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

#ifndef VGRID_CREATORS_H
#define VGRID_CREATORS_H

#include "vgrid_subclasses.h"

static int Cvgd_read_vgrid_from_file(vgrid **my_new_vgrid, int unit, int ip1, int ip2, int kind, int version);

// A generic work-horse to validate arguments and to construct a vgrid using c_encode_vert and fstd_init
static int Cvgd_new_build_vert2(vgrid *my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
		     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w);

static int Cvgd_new_from_table(vgrid **my_new_vgrid, double *table, int ni, int nj, int nk);

// A generic work-horse to validate arguments and to construct a vgrid using C_genab and Cvgd_new_build_vert2
static int Cvgd_new_gen2(vgrid *my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg);

#endif // VGRID_CREATORS_H
