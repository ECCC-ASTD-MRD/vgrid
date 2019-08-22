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
#endif
