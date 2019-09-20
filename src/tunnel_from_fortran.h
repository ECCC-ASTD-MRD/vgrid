#ifndef TUNNEL_FROM_FORTRAN_H
#define TUNNEL_FROM_FORTRAN_H

#include "vgrid.h"

int Cvgd_new_read(vgrid_descriptor **vgd, int unit, int ip1,int ip2,
                  int kind, int version);
int Cvgd_new_from_table(vgrid_descriptor **vgd, double *table, 
                        int ni, int nj, int nk);
int Cvgd_new_build_vert2(vgrid_descriptor **vgd, int kind, int version, int nk,
                         int ip1, int ip2, double *ptop_8, double *pref_8,
                         float *rcoef1, float *rcoef2, float *rcoef3,
                         float *rcoef4, double *a_m_8, double *b_m_8,
                         double *c_m_8, double *a_t_8, double *b_t_8,
                         double *c_t_8, double *a_w_8, double *b_w_8,
                         double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w,
                         int nl_m, int nl_t, int nl_2);

int Cvgd_diag_withref_2ref(vgrid_descriptor *vgd, int ni, int nj, int nk,
                           int *ip1_list, float *levels, float *sfc_field,
                           float *sfc_field_ls, int in_log, int dpidpis);

int Cvgd_diag_withref_2ref_8(vgrid_descriptor *vgd, int ni, int nj, int nk,
                             int *ip1_list,double *levels_8,double *sfc_field_8,
                             double *sfc_field_ls_8, int in_log, int dpidpis);

int Cvgd_get_int(vgrid_descriptor *vgd, char *key, int *value, int quiet);

int Cvgd_get_int_1d(vgrid_descriptor *vgd, char *key, int **value, int *nk,
                    int quiet);

int Cvgd_get_float(vgrid_descriptor *vgd, char *key, float *value_CP,
                   int quiet);

int Cvgd_get_float_1d(vgrid_descriptor *vgd, char *key, float **value, int *nk,
                      int quiet);

int Cvgd_get_double(vgrid_descriptor *vgd, char *key, double *value,
                    int quiet);

int Cvgd_get_double_1d(vgrid_descriptor *vgd, char *key, double **value, 
                       int *nk, int quiet);

int Cvgd_get_double_3d(vgrid_descriptor *vgd, char *key, double **value,
                       int *ni, int *nj, int *nk, int quiet);

int Cvgd_get_char(vgrid_descriptor *vgd, char *key, char *my_char, int quiet);

int Cvgd_getopt_int(char *key, int *value, int quiet);

int Cvgd_print_desc(vgrid_descriptor *self, int sout, int convip);

int Cvgd_put_char(vgrid_descriptor **self, char *key, char *value);

int Cvgd_put_int(vgrid_descriptor **self, char *key, int value);

int Cvgd_putopt_int(char *key, int value);

int Cvgd_is_valid(vgrid_descriptor *vgd, char *valid_table_name);

int Cvgd_stda76_hgts_from_pres_list(float *hgts, float *pres, int nb);

int Cvgd_stda76_pres(vgrid_descriptor *self, int *i_val, int nl, float *pres,
                     float *sfc_temp, float *sfc_pres);

int Cvgd_stda76_pres_from_hgts_list(float *pres, float *hgts, int nb);

int Cvgd_stda76_temp(vgrid_descriptor *self, int *i_val, int nl, float *temp);

void Cvgd_table_shape(vgrid_descriptor *vgd, int *tshape);

int Cvgd_write_desc(vgrid_descriptor *self, int unit);
#endif
