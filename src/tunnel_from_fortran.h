#ifndef TUNNEL_FROM_FORTRAN_H
#define TUNNEL_FROM_FORTRAN_H

#include "vgrid.hpp"

int Cvgd_diag_withref_2ref(int vgdid, int ni, int nj, int nk,
                           int *ip1_list, float *levels, float *sfc_field,
                           float *sfc_field_ls, int in_log, int dpidpis);

int Cvgd_diag_withref_2ref_8(int vgdid, int ni, int nj, int nk,
                             int *ip1_list,double *levels_8,double *sfc_field_8,
                             double *sfc_field_ls_8, int in_log, int dpidpis);

int Cvgd_get_char(int vgdid, char *key, char *my_char, int quiet);

int Cvgd_get_double(int vgdid, char *key, double *value, int quiet);

int Cvgd_get_double_1d(int vgdid, char *key, double **value, 
                       int *nk, int quiet);

int Cvgd_get_double_3d(int vgdid, char *key, double **value,
                       int *ni, int *nj, int *nk, int quiet);

int Cvgd_get_float(int vgdid, char *key, float *value_CP,
                   int quiet);

int Cvgd_get_float_1d(int vgdid, char *key, float **value, int *nk,
                      int quiet);

int Cvgd_get_int(int vgdid, char *key, int *value, int quiet);

int Cvgd_get_int_1d(int vgdid, char *key, int **value, int *nk,
                    int quiet);

int Cvgd_getopt_int(char *key, int *value, int quiet);

int Cvgd_is_valid(int vgdid, char *valid_table_name);

int Cvgd_new_build_vert2(int *vgdid, int kind, int version, int nk,
                         int ip1, int ip2, double *ptop_8, double *pref_8,
                         float *rcoef1, float *rcoef2, float *rcoef3,
                         float *rcoef4, double *a_m_8, double *b_m_8,
                         double *c_m_8, double *a_t_8, double *b_t_8,
                         double *c_t_8, double *a_w_8, double *b_w_8,
                         double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w,
                         int nl_m, int nl_t, int nl_2);

int Cvgd_new_from_table(int *vgdid, double *table, 
                        int ni, int nj, int nk);

int Cvgd_new_gen2(int *vgdid, int kind, int version, float *hyb,
                  int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3,                      float *rcoef4, double *ptop_8, double *pref_8,
                  double *ptop_out_8, int ip1, int ip2, float *dhm, float *dht,
                  float *dhw, int avg);

int Cvgd_print_desc(int vgdid, int sout, int convip);

int Cvgd_print_vcode_description(int vcode);

int Cvgd_put_char(int vgdid, char *key, char *value);

int Cvgd_put_int(int vgdid, char *key, int value);

int Cvgd_putopt_int(char *key, int value);

int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb);

int Cvgd_stda76_pres(int vgdid, int *i_val, int nl, float *pres,
                     float *sfc_temp, float *sfc_pres);

int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb);

int Cvgd_stda76_temp(int vgdid, int *i_val, int nl, float *temp);

void Cvgd_table_shape(int vgdid, int **tshape);

int Cvgd_vgdcmp(int vgdid1, int vgdid2);

int Cvgd_write_desc(int vgdid, int unit);


// ########## N E W   I N T E R F A C E ##########
int Cvgd_read_vgrid_from_file(int *vgdid, int unit, int ip1,int ip2, 
                              int kind, int version);
int Create_from_ab_1001(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                        int *ip1_m, int nl_m);
int Create_from_ab_1002(int *vgdid, int ip1, int ip2, double ptop_8, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m);
int Create_from_ab_2001(int *vgdid, int ip1, int ip2, double *a_m_8, double *b_m_8,
			int *ip1_m, int nl_m);
#endif
