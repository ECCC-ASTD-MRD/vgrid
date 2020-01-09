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

#include "vgrid_descriptor.h"
#include <exception>      // std::exception

#define VGD_OK       0
#define VGD_ERROR    -1
#define VGD_MISSING  -9999.
#define VGD_LEN_RFLD 5
#define VGD_LEN_RFLS 5
#define VGD_NO_REF_NOMVAR "    "
extern float VGD_STDA76_SFC_T;
extern float VGD_STDA76_SFC_P;



// Validity table for self
#define VALID_TABLE_SIZE 15

static int ptop_out_8_valid [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0, 5004, 5005, 5100,    0,     0,    0};
static int ptop_8_valid     [VALID_TABLE_SIZE] = {0,    0, 1002, 1003,    0,    0, 5001, 5002, 5003, 5004,    0,    0,    0,     0,    0};
static int pref_8_valid     [VALID_TABLE_SIZE] = {0,    0,    0, 1003,    0,    0, 5001, 5002, 5003, 5004, 5005, 5100,    0,     0,    0};
static int rcoef1_valid     [VALID_TABLE_SIZE] = {0,    0,    0, 1003,    0,    0, 5001, 5002, 5003, 5004, 5005, 5100,    0, 21001,21002};
static int rcoef2_valid     [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005, 5100,    0, 21001,21002};
static int rcoef3_valid     [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5100,    0, 21001,21002};
static int rcoef4_valid     [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5100,    0, 21001,21002};
static int rcoef3_option    [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 21001,21002};
static int rcoef4_option    [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 21001,21002};
static int a_m_8_valid      [VALID_TABLE_SIZE] = {1, 1001, 1002, 1003, 2001, 4001, 5001, 5002, 5003, 5004, 5005, 5100, 5999, 21001,21002};
static int b_m_8_valid      [VALID_TABLE_SIZE] = {1, 1001, 1002, 1003, 2001, 4001, 5001, 5002, 5003, 5004, 5005, 5100, 5999, 21001,21002};
static int c_m_8_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5100,    0, 21001,21002};
static int a_t_8_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005, 5100,    0, 21001,21002};
static int b_t_8_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005, 5100,    0, 21001,21002};
static int c_t_8_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5100,    0, 21001,21002};
static int a_w_8_valid      [VALID_TABLE_SIZE] = {1,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     0,21002};
static int b_w_8_valid      [VALID_TABLE_SIZE] = {1,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     0,21002};
static int c_w_8_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     0,21002};
static int ip1_m_valid      [VALID_TABLE_SIZE] = {1, 1001, 1002, 1003, 2001, 4001, 5001, 5002, 5003, 5004, 5005, 5100, 5999, 21001,21002};
static int ip1_t_valid      [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005, 5100,    0, 21001,21002};
static int ip1_w_valid      [VALID_TABLE_SIZE] = {1,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     0,21002};
static int dhm_valid        [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5005, 5100,    0, 21001,21002};
static int dht_valid        [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 5005, 5100,    0, 21001,21002};
static int dhw_valid        [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     0,21002};
static int is_in_logp       [VALID_TABLE_SIZE] = {0,    0,    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005, 5100,    0,     0,    0};
static int vcode_valid      [VALID_TABLE_SIZE] = {1, 1001, 1002, 1003, 2001, 4001, 5001, 5002, 5003, 5004, 5005, 5100, 5999, 21001,21002};


// Options
static int ALLOW_SIGMA = 0;

int max_int(int *vec, int ni);
int my_fstprm(int key,VGD_TFSTD_ext *ff);
int my_alloc_int(int **vec, int size, char *message);
int my_alloc_float(float **vec, int size, char *message);
int my_alloc_double(double **vec, int size, char *message);
void my_copy_double(double *aa, double **bb, int ind);
void my_copy_int(int *aa, int **bb, int ind);
bool operator==(const VGD_TFSTD_ext& lhs, const VGD_TFSTD_ext& rhs);
bool operator!=(const VGD_TFSTD_ext& lhs, const VGD_TFSTD_ext& rhs);


class vgrid
{
  // VARIABLES:
protected:
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
  int      skip;          // space to be added to table_nj
  int      k_plus_top;    // used in c_decode_vert for 5002, 5003

  // METHODS:
private:
  static double c_get_error(char *key, int quiet);
  static void c_hypsometric (float *pkp, float pk, float Tk, float gammaT, float zk, float zkp);
  static int c_set_stda_layer(int ind, float Tk, float pk, float *zk, float *zkp, float *gammaT,
                              float *pkp, int *zero_lapse_rate);
  static int c_get_stda76(float *Tk, float *pk, float *zk, float *gammaT, int *zero_lapse_rate);
  static int same_vec_i(int *vec1, int n1, int *vec2, int n2);
  static int same_vec_r8(double *vec1, int n1, double *vec2, int n2);
  static int similar_vec_r8(double *vec1, int n1, double *vec2, int n2);  static int Cvgd_FindIp1Idx(int Ip1,int *Lst,int Size);

protected:
  static void flip_transfer_d2c(char *name, double val_8);
  static void flip_transfer_c2d(char *name, void *val_8);
  static int c_convip_Level2IP(float level, int kind);
  static int c_convip_Level2IP_old_style(float level, int kind);
  static double c_comp_diag_a_height(double pref_8, float height);
  static double c_comp_diag_a_ip1(double pref_8, int ip1);


public:
  int is_valid(int *table_valid);
  int is_option(int *table_option);
  int Cvgd_is_valid(char *valid_table_name);
  int is_required_double(double *ptr, int *table_valid, char *message);
  int is_required_float(float *ptr, int *table_valid, char *message);
  void set_match_ipig(int match_ipig);
  int c_stda76_temp_from_press(int *i_val, int nl, float *temp);
  int c_stda76_temp_pres_from_heights(int *i_val, int nl, float *temp, float *pres, float *sfc_temp, float *sfc_pres);
  void Cvgd_table_shape(int **tshape);
  int Cvgd_print_desc(int sout, int convip);
  int Cvgd_levels_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, int in_log);
  int Cvgd_levels(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log);
  int Cvgd_levels_2ref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8, int in_log);
  int Cvgd_levels_2ref(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log);
  int Cvgd_diag_withref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, int in_log, int dpidpis);
  int Cvgd_diag_withref(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis);
  int Cvgd_diag_withref_2ref_8(int ni, int nj, int nk, int *ip1_list, double *levels_8, double *sfc_field_8, double *sfc_field_ls_8, int in_log, int dpidpis);
  int Cvgd_diag_withref_2ref(int ni, int nj, int nk,
			   int *ip1_list, float *levels, float *sfc_field,
			   float *sfc_field_ls, int in_log, int dpidpis);
  void c_vgd_free_abci();
  int Cvgd_set_vcode_i(int Kind,int Version);
  int fstd_init();
  int Cvgd_set_vcode();
  int Cvgd_new_build_vert2(int kind, int version, int nk, int ip1, int ip2,
			   double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2,
			   float *rcoef3, float *rcoef4, double *a_m_8, double *b_m_8,
			   double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8,
			   double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m,
			   int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w);
  int Cvgd_new_from_table(double *table, int ni, int nj, int nk);
  int Cvgd_new_gen2(int kind, int version, float *hyb, int size_hyb, float *rcoef1,
                    float *rcoef2, float *rcoef3, float *rcoef4,
                    double *ptop_8, double *pref_8, double *ptop_out_8,
                    int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg);

  int C_compute_heights_0001(int ni, int nj, int nk, int *ip1_list, float *levels);
  int C_compute_heights_0001_8(int ni, int nj, int nk, int *ip1_list, double *levels);
  int C_compute_heights_4001(int ni, int nj, int nk, int *ip1_list, float *levels);
  int C_compute_heights_4001_8(int ni, int nj, int nk, int *ip1_list, double *levels);
  int C_compute_heights_21001(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls);
  int C_compute_heights_21001_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, double *sfc_field_ls);
  int C_compute_pressure_1001_1002(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log);
  int C_compute_pressure_1001_1002_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log);
  int C_compute_pressure_1003_5001(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis );
  int C_compute_pressure_1003_5001_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log, int dpidpis );
  int C_compute_pressure_2001(int ni, int nj, int nk, int *ip1_list, float *levels, int in_log);
  int C_compute_pressure_2001_8(int ni, int nj, int nk, int *ip1_list, double *levels, int in_log);
  int C_compute_pressure_5002_5003_5004_5005(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, int in_log, int dpidpis);
  int C_compute_pressure_5002_5003_5004_5005_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, int in_log, int dpidpis);
  int C_compute_pressure_5100(int ni, int nj, int nk, int *ip1_list, float *levels, float *sfc_field, float *sfc_field_ls, int in_log, int dpidpis);
  int C_compute_pressure_5100_8(int ni, int nj, int nk, int *ip1_list, double *levels, double *sfc_field, double *sfc_field_ls, int in_log, int dpidpis);
  int C_load_toctoc(VGD_TFSTD_ext var, int key);
  int Cvgd_vgdcmp(vgrid *vgd2);
  void Cvgd_free();
  int Cvgd_getopt_int(char *key, int *value, int quiet);
  int Cvgd_get_int(char *key, int *value, int quiet);
  int Cvgd_get_int_1d(char *key, int **value, int *nk, int quiet);
  int Cvgd_get_float(char *key, float *value, int quiet);
  int Cvgd_get_float_1d(char *key, float **value, int *nk, int quiet);
  //int Cvgd_put_double(char *key, double value_put);
  int Cvgd_get_double(char *key, double *value_get, int quiet);
  int Cvgd_get_double_1d(char *key, double **value, int *nk, int quiet);
  int Cvgd_get_double_3d(char *key, double **value, int *ni, int *nj, int *nk, int quiet);
  int Cvgd_get_char(char *key, char out[], int quiet);
  int Cvgd_put_char(char *key, char *value);
  int Cvgd_putopt_int(char *key, int value);
  int Cvgd_put_int(char *key, int value);


  int Cvgd_new_read(int unit, int ip1, int ip2, int kind, int version);
  int Cvgd_write_desc (int unit);
  int Cvgd_stda76_temp(int *i_val, int nl_t, float *temp);
  int Cvgd_stda76_pres(int *i_val, int nl_t, float *pres, float *sfc_temp, float *sfc_pres);

public:
  static int Cvgd_print_vcode_description(int vcode);
  static float c_convip_IP2Level(int IP,int *kind);
  static void decode_HY(VGD_TFSTD_ext var, double *ptop_8, double *pref_8, float *rcoef);
  static int correct_kind_and_version(int key, int kind, int version, VGD_TFSTD_ext *var, int *status);
  static int C_get_consistent_hy(int iun, VGD_TFSTD_ext var, VGD_TFSTD_ext *va2, char *nomvar);
  static int C_get_consistent_pt_e1(int iun, float *val, char *nomvar);
  static int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb);
  static int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb);


// ########## N E W   I N T E R F A C E ##########
public:
  // Constructors
  vgrid();
  vgrid(int unit, int ip1, int ip2, int kind, int version);

public:
  virtual int c_decode_vert() = 0;
  virtual int c_encode_vert() = 0;
  void build_vgrid_from_key(int key);

protected:
  virtual void set_table_nj(int nk) = 0;
  int allocate_table(int nk);
  virtual void fstd_subinit() = 0;  // subclass-specific assignments to initialize the fstd record

  // Most subclasses have a private C_genab method, but the arguments are diffent for each one
  // (Therefore, do not define the interface here.)
  // virtual C_genab()
};

class vgrid_exception : public std::exception
{
};

#endif // VGRID_H
