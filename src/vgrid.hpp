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

#include <stdint.h>
#ifndef VGRID_H
#define VGRID_H

#include "coat_check.hpp"
#include "vgrid_descriptor.h"

#define VGD_OK       0
#define VGD_ERROR    -1
#define VGD_MISSING  -9999.
#define VGD_LEN_RFLD 5
#define VGD_LEN_RFLS 5
#define VGD_NO_REF_NOMVAR "    "
extern float VGD_STDA76_SFC_T;
extern float VGD_STDA76_SFC_P;


class vgrid
{
  // VARIABLES:
public:
  VGD_TFSTD rec;          // RPN standard file header
  double   ptop_8;        // Top level pressure (Pa)
  double   pref_8;        // Reference pressure (Pa)
  double   *table;        // Complete grid descriptor record
  int      table_ni;      //    ni size of table
  int      table_nj;      //    nj size of table
  int      table_nk;      //    nk size of table
  double   *a_m_8;        // A-coefficients for momentum levels  
  double   *b_m_8;        // B-coefficients for momentum levels
  double   *c_m_8;        // C-coefficients for momentum levels
  double   *a_t_8;        // A-coefficients for thermodynamic levels
  double   *b_t_8;        // B-coefficients for thermodynamic levels
  double   *c_t_8;        // C-coefficients for thermodynamic levels
  double   *a_w_8;        // A-coefficients for vertical-velocity levels
  double   *b_w_8;        // B-coefficients for vertical-velocity levels
  double   *c_w_8;        // C-coefficients for vertical-velocity levels
  int      *ip1_m;        // ip1 values for momentum levels
  int      *ip1_t;        // ip1 values for momentum levels
  int      *ip1_w;        // ip1 values for momentum levels
  int      nl_m;          // Number of momentum      level
  int      nl_t;          // Number ot thermodynamic level
  int      nl_w;          // Number ot vertical volocity level
  float    dhm;           // Diag level Height (m) for Momentum variables UU,VV
  float    dht;           // Diag level Height (t) for Thermo variables TT,HU, etc
  float    dhw;           // Diag level Height (m) for vertical-velocity variables WT1 ZZ
  char*    ref_name;      // Reference field name
  char*    ref_namel;     // Reference field name
  float    rcoef1;        // Rectification coefficient
  float    rcoef2;        // Rectification coefficient
  float    rcoef3;        // Rectification coefficient
  float    rcoef4;        // Rectification coefficient
  int      nk;            // Number of momentum levels
  int      ip1;           // ip1 value given to the 3D descriptor
  int      ip2;           // ip2 value given to the 3D descriptor
  int      unit;          // file unit associated with this 3D descriptor
  int      vcode;         // Vertical coordinate code
  int      kind;          // Vertical coordinate code
  int      version;       // Vertical coordinate code
  char     match_ipig;    // do ip/ig matching for records
  char     valid;         // Validity of structure
  // METHODS:
public:
  vgrid_descriptor *self;
  int is_valid(vgrid_descriptor *self, int *table_valid);
  int is_option(vgrid_descriptor *self, int *table_option);
  int Cvgd_is_valid(vgrid_descriptor *self, char *valid_table_name);
  int is_required_double(vgrid_descriptor *self, double *ptr, int *table_valid, char *message);
  int is_required_float(vgrid_descriptor *self, float *ptr, int *table_valid, char *message);
  int c_stda76_temp_from_press(vgrid_descriptor *self, int *i_val, int nl, float *temp);
  int c_stda76_temp_pres_from_heights(vgrid_descriptor *self, int *i_val, int nl, float *temp, float *pres, float *sfc_temp, float *sfc_pres);
  void Cvgd_table_shape(vgrid_descriptor *self, int **tshape);
  int Cvgd_print_desc(vgrid_descriptor *self, int sout, int convip);
  static int Cvgd_print_vcode_description(int vcode);
  int Cvgd_levels_8(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, int in_log);
  int Cvgd_levels(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log);
  int Cvgd_levels_2ref_8(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8, int in_log);
  int Cvgd_levels_2ref(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log);
  int Cvgd_diag_withref_8(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, int in_log, int dpidpis);
  int Cvgd_diag_withref(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis);
  int Cvgd_diag_withref_2ref_8(vgrid_descriptor *self, int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8, int in_log, int dpidpis);
  int Cvgd_diag_withref_2ref(vgrid_descriptor *self, int ni, int nj, int nk,
			   int *ip1_list, float *levels, float *sfc_field,
			   float *sfc_field_ls, int in_log, int dpidpis);
vgrid(vgrid_descriptor *self_in);
  void c_vgd_construct_jwb(vgrid_descriptor *vgrid);
  void c_vgd_free_abci(vgrid_descriptor *self);
  int Cvgd_set_vcode_i(vgrid_descriptor *VGrid,int Kind,int Version);
  int fstd_init(vgrid_descriptor *VGrid);
  int Cvgd_set_vcode(vgrid_descriptor *VGrid);
  int Cvgd_new_build_vert(vgrid_descriptor *self, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t);
  int Cvgd_new_build_vert_1001(vgrid_descriptor *self, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_1002(vgrid_descriptor *self, int ip1, int ip2, double ptop_8,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_2001(vgrid_descriptor *self, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_4001(vgrid_descriptor *self, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_5001(vgrid_descriptor *self, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_5002(vgrid_descriptor *self, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t);
  int Cvgd_new_build_vert_5005(vgrid_descriptor *self, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl);
  int Cvgd_new_build_vert_5100(vgrid_descriptor *self, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl);
  int Cvgd_new_build_vert_5999(vgrid_descriptor *self, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk);
  int Cvgd_new_build_vert_21001(vgrid_descriptor *self, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl);
  int Cvgd_new_build_vert_21002(vgrid_descriptor *self, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8,
			      double *a_t_8, double *b_t_8, double *c_t_8,
			      double *a_w_8, double *b_w_8, double *c_w_8,
			      int *ip1_m, int *ip1_t, int *ip1_w, int nl);
  int Cvgd_new_build_vert2(vgrid_descriptor *self, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
		     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w);
  int c_encode_vert_0001(vgrid_descriptor *self,int nk);
  int c_encode_vert_1001(vgrid_descriptor *self,int nk);
  int c_encode_vert_1002(vgrid_descriptor *self,int nk);
  int c_encode_vert_2001(vgrid_descriptor *self,int nk);
  int c_encode_vert_4001(vgrid_descriptor *self,int nk);
  int c_encode_vert_5001(vgrid_descriptor *self,int nk);
  int c_encode_vert_5002_5003_5004_5005(vgrid_descriptor *self, char update);
  int c_encode_vert_5100(vgrid_descriptor *self, char update);
  int c_encode_vert_5999(vgrid_descriptor *self,int nk);
  int c_encode_vert_21001(vgrid_descriptor *self, char update);
  int c_encode_vert_21002(vgrid_descriptor *self, char update);
  int c_decode_vert_0001(vgrid_descriptor *self);
  int c_decode_vert_1001(vgrid_descriptor *self);
  int c_decode_vert_1002(vgrid_descriptor *self);
  int c_decode_vert_2001(vgrid_descriptor *self);
  int c_decode_vert_4001(vgrid_descriptor *self);
  int c_decode_vert_1003_5001(vgrid_descriptor *self);
  int c_decode_vert_5100(vgrid_descriptor *self);
  int c_decode_vert_5002_5003_5004_5005(vgrid_descriptor *self);
  int c_decode_vert_5999(vgrid_descriptor *self);
  int c_decode_vert_21001(vgrid_descriptor *self);
  int c_decode_vert_21002(vgrid_descriptor *self);
  int C_genab_1001(float *hyb, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_1002(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_1003(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_2001(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_4001(float *hgts, int nk, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_5001(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m);
  int C_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift);
  int C_genab_5004(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t);
  int C_load_toctoc(vgrid_descriptor *self, VGD_TFSTD_ext var, int key);
  int c_table_update(vgrid_descriptor *self);
  int c_vgrid_genab_5005(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht);
  int c_vgrid_genab_5100(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht, int avg);
  int c_vgrid_genab_21001(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, float dhm, float dht);
  int c_vgrid_genab_21002(float *hybuser, int nk, int *nl_m, int *nl_t, int *nl_w, float rcoef1, float rcoef2, float rcoef3, float rcoef4, double **PP_a_m_8, double **PP_b_m_8, double **PP_c_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, double **PP_c_t_8, int **PP_ip1_t, double **PP_a_w_8, double **PP_b_w_8, double **PP_c_w_8, int **PP_ip1_w, float dhm, float dht, float dhw);
  int Cvgd_vgdcmp(vgrid_descriptor *vgd1, vgrid_descriptor *vgd2);
  int Cvgd_vgdcmp_jwb(vgrid *that);
  void Cvgd_free(vgrid_descriptor *self);
static int Cvgd_getopt_int(char *key, int *value, int quiet);
  int Cvgd_get_int(vgrid_descriptor *self, char *key, int *value, int quiet);
  int Cvgd_get_int_1d(vgrid_descriptor *self, char *key, int **value, int *nk, int quiet);
  int Cvgd_get_float(vgrid_descriptor *self, char *key, float *value, int quiet);
  int Cvgd_get_float_1d(vgrid_descriptor *self, char *key, float **value, int *nk, int quiet);
  //int Cvgd_put_double(vgrid_descriptor *self, char *key, double value_put);
  int Cvgd_get_double(vgrid_descriptor *self, char *key, double *value_get, int quiet);
  int Cvgd_get_double_1d(vgrid_descriptor *self, char *key, double **value, int *nk, int quiet);
  int Cvgd_get_double_3d(vgrid_descriptor *self, char *key, double **value, int *ni, int *nj, int *nk, int quiet);
  int Cvgd_get_char(vgrid_descriptor *self, char *key, char out[], int quiet);
  int Cvgd_put_char(vgrid_descriptor *self, char *key, char *value);
  static int Cvgd_putopt_int(char *key, int value);
  int Cvgd_put_int(vgrid_descriptor *self, char *key, int value);
  int Cvgd_new_gen2(vgrid_descriptor *self, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg);
  int C_get_consistent_pt_e1(int iun, float *val, char *nomvar);
  int C_get_consistent_hy(int iun, VGD_TFSTD_ext var, VGD_TFSTD_ext *va2, char *nomvar);
  int C_gen_legacy_desc(vgrid_descriptor *self, int unit, int *keylist , int nb);
  int c_legacy(vgrid_descriptor *self, int unit, int F_kind);
  int Cvgd_new_gen(vgrid_descriptor *self, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
		 int ip1, int ip2, float *dhm, float *dht, int avg);
  int Cvgd_new_gen_1001(vgrid_descriptor *self, float *hyb, int size_hyb, int ip1, int ip2);
  int Cvgd_new_gen_2001(vgrid_descriptor *self, float *hyb, int size_hyb, int ip1, int ip2);
  int Cvgd_new_gen_5999(vgrid_descriptor *self, float *hyb, int size_hyb, int ip1, int ip2);
  int Cvgd_new_gen_1002(vgrid_descriptor *self, float *hyb, int size_hyb, double ptop_8, int ip1, int ip2);
  int Cvgd_new_gen_4001(vgrid_descriptor *self, float *hyb, int size_hyb, int ip1, int ip2);
  int Cvgd_new_gen_5001(vgrid_descriptor *self, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, int ip1, int ip2);
  int Cvgd_new_gen_5002(vgrid_descriptor *self, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, float rcoef2, int ip1, int ip2);
  int Cvgd_new_gen_5005(vgrid_descriptor *self, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, int ip1, int ip2, float dhm, float dht );
  int Cvgd_new_gen_5100(vgrid_descriptor *self, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, int avg);
  int Cvgd_new_gen_21001(vgrid_descriptor *self, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht);
  int Cvgd_new_gen_21002(vgrid_descriptor *self, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, float dhw);

  int Cvgd_new_read(vgrid_descriptor *self, int unit, int ip1, int ip2, int kind, int version);
  int Cvgd_write_desc (vgrid_descriptor *self, int unit);
  int Cvgd_new_from_table(vgrid_descriptor *self, double *table, int ni, int nj, int nk);
  int Cvgd_stda76_temp(vgrid_descriptor *self, int *i_val, int nl_t, float *temp);
  int Cvgd_stda76_pres(vgrid_descriptor *self, int *i_val, int nl_t, float *pres, float *sfc_temp, float *sfc_pres);
static int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb);
static int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb);
};

#endif // VGRID_H
