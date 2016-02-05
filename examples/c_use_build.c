#include <stdio.h>
#include <stdlib.h>
#include "vgrid.h"

int c_use_build () {

  // This program is include in the examples package directory
  // To know what parameters to pass to Cvgd_new_build_vert for a given Vcode, call de function with NULL pointer for optional arguments (7th and up).
  //    Functio is called below on all usefull Vcode with all options set to NULL to get the list of required options.
  //    One example of a complete call is given for the pressure levels.
  //    Note that the parameter nl_m is always required and nl_t is required only if thermo level are required. The size of these vectors
  //         are variable depending of Vcode. This is why the use of this function is for the experts user like model developers.

  int ier, kind, version, ip1 = 111, ip2 = 222;
  vgrid_descriptor *vgd = NULL;
#include "vgrid_version.hc"
  // Data for pressure example
  double a_m_8[3] = {100000.0, 85000.0, 50000.0};
  double b_m_8[3] = {     0.0,     0.0,     0.0};
  int    ip1_m[3] = {1000    , 8500   , 500    };
  
  printf("vgrid_descriptors_version = %s\n", vgrid_descriptors_version);
  
  kind=1;
  version=1;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  kind=1;
  version=2;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  kind=1;
  version=3;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  kind=2;
  version=1;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  kind=5;
  version=1;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  kind=5;
  version=2;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  // Vcode 5003 and 5004 were not used much, so we skip them

  kind=5;
  version=5;
  printf("\nOptional arguments needed for Vcode %d\n", kind*1000 + version);
  ier = Cvgd_new_build_vert(&vgd, kind, version, 3, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  // Here is the pressure example (Vcode 2001)
     kind=2;
  version=1;
  ier = Cvgd_new_build_vert(&vgd, kind, version,  3, ip1, ip2, NULL   , NULL   , NULL   , NULL   ,  a_m_8,  b_m_8, NULL  , NULL  , ip1_m , NULL  , 3, NULL);
  if ( ier == VGD_ERROR){
    printf("ERROR with Cvgd_new_build_vert on Vcode %d\n", kind*1000 + version);
    return(1);
  }
  ier = Cvgd_print_desc(vgd, -1, -1);

  return(0);
}
