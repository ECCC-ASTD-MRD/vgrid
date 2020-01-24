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

#include "vgrid_subclasses.hpp"

//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
//
// THESE ARE FACTORY METHODS
//     Each one determines which subclass needs to be constructed
//     and then constructs it, returning it as a pointer to the base class (i.e. vgrid)
//
//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

// Create a bare vgrid object of the correct subclass
static void Cvgd_create_vgrid_from_vcode(vgrid **new_vgrid, int vcode);

int Cvgd_read_vgrid_from_file(vgrid **my_new_vgrid, int unit, int ip1, int ip2, int kind, int version);

// This is a wrapper around vgrid::build_vgrid_from_key(key)
int Cvgd_create_vgrid_from_filekey(vgrid **new_vgrid, int key);

// A generic work-horse to validate arguments and to construct a vgrid using c_encode_vert and fstd_init
int Cvgd_new_build_vert2(vgrid **my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
		     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w);

int Cvgd_new_from_table(vgrid **my_new_vgrid, double *table, int ni, int nj, int nk);

// A generic work-horse to validate arguments and to construct a vgrid using C_genab and Cvgd_new_build_vert2
int Cvgd_new_gen2(vgrid **my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg);


//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
//
// END OF THE FACTORY METHODS
//
//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/



int Create_from_ab_1001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                        int *ip1_m, int nl_m);
int Create_from_ab_1002(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_1003(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double pref_8, float rcoef1,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_2001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_4001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                        int *ip1_m, int nl_m);
int Create_from_ab_5001(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double pref_8, float rcoef1,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_5002(vgrid** new_vgrid, int ip1, int ip2, double ptop_8, double pref_8,
			float rcoef1, float rcoef2, double *a_m_8j,
			double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t,
			int nl_m, int nl_t);
int Create_from_ab_5003(vgrid** new_vgrid, int ip1, int ip2,
			double ptop_8, double pref_8, float rcoef1, float rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t, int nl_m);
int Create_from_ab_5004(vgrid** new_vgrid, int ip1, int ip2,
			double ptop_8, double pref_8, float rcoef1, float rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t, int nl_m);
int Create_from_ab_5005(vgrid** new_vgrid, int ip1, int ip2,
			double pref_8, float rcoef1, float rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t, int nl_m);
int Create_from_ab_5100(vgrid** new_vgrid, int ip1, int ip2, double pref_8, float rcoef1,
			float rcoef2, float rcoef3, float rcoef4,
			double *a_m_8, double *b_m_8, double *c_m_8,
			double *a_t_8, double *b_t_8, double *c_t_8,
			int *ip1_m, int *ip1_t, int nl_m);
int Create_from_ab_5999(vgrid** new_vgrid, int ip1, int ip2,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_21001(vgrid** new_vgrid, int ip1, int ip2,
			 float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m);
int Create_from_ab_21002(vgrid** new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 double *a_w_8, double *b_w_8, double *c_w_8,
			 int *ip1_m, int *ip1_t, int *ip1_w, int nl_m);


int Cvgd_build_from_hyb_1001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1,
			     int ip2);
int Cvgd_build_from_hyb_1002(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			     double ptop_8, int ip1, int ip2);
int Cvgd_build_from_hyb_2001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1,
			     int ip2);
int Cvgd_build_from_hyb_5003(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			     double ptop_8, double pref_8, double *ptop_out_8,
			     float rcoef1, float rcoef2, int ip1, int ip2);
int Cvgd_build_from_hyb_5004(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			     double ptop_8, double pref_8, double *ptop_out_8,
			     float rcoef1, float rcoef2, int ip1, int ip2);
int Cvgd_build_from_hyb_5005(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			     double pref_8, double *ptop_out_8, float rcoef1,
			     float rcoef2, int ip1,
			     int ip2, float *dhm, float *dht);
int Cvgd_build_from_hyb_5100(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			     double pref_8, double *ptop_out_8, float rcoef1,
			     float rcoef2, float rcoef3, float rcoef4, int ip1,
			     int ip2, float *dhm, float *dht, int avg);
int Cvgd_build_from_hyb_21001(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			      float rcoef1, float rcoef2, int ip1, int ip2, float *dhm,
			      float *dht, float *dhw, float rcoef3,float rcoef4);
int Cvgd_build_from_hyb_21002(vgrid **my_new_vgrid, float *hyb, int size_hyb,
			      float rcoef1, float rcoef2, int ip1, int ip2, float *dhm,
			      float *dht, float *dhw, float rcoef3=-1,float rcoef4=-1);




// Front ends to call Cvgd_new_build_vert2 to create a vgrid
int Cvgd_new_build_vert(vgrid **my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t);
int Cvgd_new_build_vert_1001(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
int Cvgd_new_build_vert_1002(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
int Cvgd_new_build_vert_4001(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
int Cvgd_new_build_vert_5001(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
int Cvgd_new_build_vert_5002(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t);
int Cvgd_new_build_vert_5005(vgrid **my_new_vgrid, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl);
int Cvgd_new_build_vert_5100(vgrid **my_new_vgrid, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl);
int Cvgd_new_build_vert_5999(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
int Cvgd_new_build_vert_21001(vgrid **my_new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl);
int Cvgd_new_build_vert_21002(vgrid **my_new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8,
			      double *a_t_8, double *b_t_8, double *c_t_8,
			      double *a_w_8, double *b_w_8, double *c_w_8,
			      int *ip1_m, int *ip1_t, int *ip1_w, int nl);







// Front ends to call Cvgd_new_gen2 to construct a vgrid
int Cvgd_new_gen(vgrid **my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2,
                 double *ptop_8, double *pref_8, double *ptop_out_8,
                 int ip1, int ip2, float *dhm, float *dht, int avg);
int Cvgd_new_gen_1001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2);
int Cvgd_new_gen_2001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2);
int Cvgd_new_gen_5999(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2);
int Cvgd_new_gen_1002(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, int ip1, int ip2);
int Cvgd_new_gen_4001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2);
int Cvgd_new_gen_5001(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, int ip1, int ip2);
int Cvgd_new_gen_5002(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, float rcoef2, int ip1, int ip2);
int Cvgd_new_gen_5005(vgrid **my_new_vgrid, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, int ip1, int ip2, float dhm, float dht );
int Cvgd_new_gen_5100(vgrid **my_new_vgrid, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, int avg);
int Cvgd_new_gen_21001(vgrid **my_new_vgrid, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht);
int Cvgd_new_gen_21002(vgrid **my_new_vgrid, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, float dhw);




int C_gen_legacy_desc(vgrid **my_new_vgrid, int unit, int *keylist , int nb);
int c_legacy(vgrid **my_new_vgrid, int unit, int F_kind);

#endif // VGRID_CREATORS_H
