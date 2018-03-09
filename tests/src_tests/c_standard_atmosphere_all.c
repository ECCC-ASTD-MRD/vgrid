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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "vgrid.h"
#include "armnlib.h"

char *filenames[] = {
    "data/dm_1001_from_model_run",
    "data/dm_1002_from_model_run",
    "data/2001_from_model_run",
    "data/dm_5001_from_model_run",
    "data/dm_5002_from_model_run",
    "data/dm_5005_from_model_run",
    "data/dm_5100_from_model_run",
    "data/dm_5999_from_model_run",
    "data/dm_21001_from_model_run_SLEVE",
    "data/dm_21001_from_model_run_NON_SLEVE",
    "data/dm_21002_from_model_run_SLEVE",
    "data/dm_21002_from_model_run_NON_SLEVE"
};

#define n_file (sizeof (filenames) / sizeof (const char *))
// Do not let write_control set to 1 since this will invalidate the test. Just use this to produce the 
// validation data. To do so, run the program with write_control set to 1, copy the files
// data/*_stda76_temp.txt to data_Linux dir
#define write_control 0

char* concat(const char *s1, const char *s2)
{
    char *result = malloc(strlen(s1)+strlen(s2)+1);//+1 for the null-terminator
    if(! result){
      printf("Problem in tests with concat");
      exit(1);
    }
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

int test_it(char *filename, int ind) {
  FILE *fp;
  char buff[255],  buff2[255];
  int ier, iun, k;
  int quiet=0, *i_val = NULL, i_val_c;
  int nl_t, nl_t_c;
  char mode[]="RND";
  float *temp = NULL, temp_c;
  vgrid_descriptor *vgd = NULL;
      
  iun = 10 + ind;
  
  ier = c_fnom(&iun,filename,mode,0);
  if( ier < 0 ) {
    printf("ERROR with c_fnom on iun, file %s\n", filename);
    return(VGD_ERROR);
  }
  ier = c_fstouv(iun,"RND","");  
  if( ier < 0 ) {
    printf("ERROR with c_fstouv on iun, file %s\n", filename);
    return(VGD_ERROR);
  }

  if( Cvgd_new_read(&vgd, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(VGD_ERROR);
  }
  //ier = Cvgd_print_desc(vgd, -1, -1);

  if( Cvgd_get_int_1d(vgd, "VIPT", &i_val, NULL, quiet) ==  VGD_ERROR ) {
    printf("ERROR with Cvgd_get_int for VIPT\n");
    return(VGD_ERROR);
  }

  if( Cvgd_get_int(vgd, "NL_T", &nl_t, quiet) == VGD_ERROR){
    printf("ERROR cannot Cvgd_get_int on NL_T\n");
    return(VGD_ERROR);
  }
  
  if( Cvgd_standard_atmosphere_1976_temp(vgd, i_val, nl_t, &temp) == VGD_ERROR ) {
    printf("ERROR with Cvgd_standard_atmosphere_1976_temp\n");
    return(VGD_ERROR);
  }
  //for( k=0; k<nl_t; k++){
  //  printf("k=%d\n",k);
  //  printf("i_val[k]=%d, temp[k]=%f\n",i_val[k],temp[k]);
  //}
  char* sss = concat(filename,"_stda76_temp.txt");
  if(write_control){
    fp = fopen(sss, "w");
    fprintf(fp, "size %d\n",nl_t);
    for( k=0; k<nl_t; k++){
      fprintf(fp, "ip1 %i temp %f\n",i_val[k],temp[k]);
    }
    fclose(fp);
  }
  
  fp = fopen(sss, "r");
  if( fp == NULL ){
    printf("ERROR in test, validation file not present %s\n",sss);
  }
  fscanf(fp, "%s %d",buff, &nl_t_c);
  //printf("buff=%s, nl_t_c=%d\n",buff,nl_t_c);
  if( nl_t_c != nl_t){
    printf("ERROR in tests, size problem with validation file\n");
    return(VGD_ERROR);
  }
  for( k=0; k<nl_t; k++){
    fscanf(fp, "%s %d %s %f",buff, &i_val_c, buff2, &temp_c);
    if( i_val_c != i_val[k] || fabs(temp_c - temp[k]) > .01f ){
      printf("ERROR differences found, expecting:\n");
      printf("%i %f\n",i_val_c,temp_c);
      printf("got:\n%i %f\n",i_val[k],temp[k]);
      printf("ERROR TEST on file %s failled\n",filename);
      return(VGD_ERROR);
    }
  }
  printf("TEST on file %s is OK\n",filename);
  
  fclose(fp);
  ier = c_fstfrm(iun);
  ier = c_fclos(iun);
      
  free(sss);
  Cvgd_free(&vgd);
  free(i_val);

  return(VGD_OK);
  
}

//========================================================================
//========================================================================

void c_standard_atmosphere_all() {
  
  int i, ier, status = VGD_OK;

  ier = Cvgd_putopt_int("ALLOW_SIGMA",1);
  
  for (i = 0; i < (int) n_file; i++) {
    printf ("Testing %s\n", filenames[i]);
    if(test_it(filenames[i],i) == VGD_ERROR){
      printf("ERROR with %s\n",filenames[i]);
      status = VGD_ERROR;
      exit(1);
    }
  }  
  ier = c_ut_report(status,"testing Cvgd_standard_atmosphere");  
  
}
