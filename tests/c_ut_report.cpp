#include <stdio.h>
#include <stdlib.h>
#include <vgrid.hpp>
#include "c_ut_report.h"

int c_ut_report(int status, char *message)
{
  FILE *fp;
  fp = fopen("test_report.txt","w+");
  if(! fp){
    printf("ERROR in c_ut_report: cannot open file test_report.txt\n");
    return(VGD_ERROR);
  }
  if(status == VGD_OK){
    fprintf(fp," ok\n");
  } else {
    fprintf(fp," failed %s\n",message);
  }
  return(VGD_OK);
}
