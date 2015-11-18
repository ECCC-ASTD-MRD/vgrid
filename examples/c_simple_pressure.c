#include <stdio.h>
#include <stdlib.h>
#include "vgrid.h"

int c_simple_pressure() {
  
  // Goal: compute pressure for all thermo levels and a pressure profile
  // Note: error trapping is kept to a mimimum for clarity
  
  char filename[]="/users/dor/afsg/apm/ords/cmdn/vgrid/tests/data/dm_5002_from_model_run";
  int *i_val = NULL, nl_t, iun=10, ier, key, ni, nj, nk, ij, k;
  float *p0 = NULL, p0_stn, *pres = NULL, *prof = NULL;
  vgrid_descriptor *vgd = NULL;
#include "vgrid_version.hc"
  
  printf("%s\n", vgrid_descriptors_version);

  // Open RPN standard file on unit iun
  ier = c_fnom(iun, filename, "RND", 0);
  ier = c_fstouv(iun, "RND");  

  // Construct a new Vgrid descriptor
  if( Cvgd_new_read(&vgd, iun, -1, -1, -1, -1) == VGD_ERROR ) {
    printf("ERROR with Cvgd_new_read on iun\n");
    return(1);
  }

  // Print info of this descriptor
  Cvgd_print_desc(vgd, -1, -1);

  // Get ip1 list of all Thermo levels, list will be malloc in function, size of list will be returned in nl_t
  ier = Cvgd_get_int_1d(vgd, "VIPT", &i_val, &nl_t, -1);

  // Get surface pressure
  key = c_fstinf(iun, &ni, &nj, &nk, -1," ", -1, -1, -1," ","P0");
  p0 = malloc( ni*nj * sizeof(float) );
  ier = c_fstluk(p0, key, &ni, &nj, &nk );
  
  // Convert pressure in Pa
  for( ij=0; ij < ni*nj; ij++){
    p0[ij] = p0[ij] * 100.;
  }

  // Allocate pressure cube
  pres = malloc( ni*nj*nl_t * sizeof(float) );

  // Compute pressure for all thermo levels
  ier = Cvgd_levels(vgd, ni, nj, nl_t, i_val, pres, p0, -1);

  // Compute pressure for a single profile with surface pressure at 1013 hPa
  prof = malloc( nl_t * sizeof(float) );
  p0_stn = 1013. * 100.;
  ier = Cvgd_levels(vgd, 1, 1, nl_t, i_val, prof, &p0_stn, -1);
  for( k = 0; k < nl_t; k++){
    printf("k = %d, prof[k] = %f\n", k, prof[k]);
  }

  // Free Vgrid descriptor
  Cvgd_free(&vgd);
  free(i_val);
  free(p0);
  free(pres);
  free(prof);

  return(0);
}
