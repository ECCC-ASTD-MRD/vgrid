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

#include "vgrid_creators.hpp"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "armnlib.hpp"

#define MAX_DESC_REC 10000      //maximum number of descriptor records in a single file
#define ZNAME "!!"              //name of the vertical coodinate

void Cvgd_create_vgrid_from_vcode(vgrid **new_vgrid, int vcode)
{
  // Instantiate a vgrid subclass on the heap, according to the vcode
  switch (vcode)
  {
  case 0001:
    *new_vgrid = new vgrid_0001();
    break;

  case 1001:
    *new_vgrid = new vgrid_1001();
    break;

  case 1002:
    *new_vgrid = new vgrid_1002();
    break;

  case 1003:
    *new_vgrid = new vgrid_1003();
    break;

  case 2001:
    *new_vgrid = new vgrid_2001();
    break;

  case 4001:
    *new_vgrid = new vgrid_4001();
    break;

  case 5001:
    *new_vgrid = new vgrid_5001();
    break;

  case 5002:
    *new_vgrid = new vgrid_5002();
    break;

  case 5003:
    *new_vgrid = new vgrid_5003();
    break;

  case 5004:
    *new_vgrid = new vgrid_5004();
    break;

  case 5005:
    *new_vgrid = new vgrid_5005();
    break;

  case 5100:
    *new_vgrid = new vgrid_5100();
    break;

  case 5999:
    *new_vgrid = new vgrid_5999();
    break;

  case 21001:
    *new_vgrid = new vgrid_21001();
    break;

  case 21002:
    *new_vgrid = new vgrid_21002();
    break;

  default:
    printf("In Cvgd_create_vgrid_from_vcode, vcode %d not recognized\n", vcode);
    throw vgrid_exception();
    break;
  }
}

int Cvgd_read_vgrid_from_file(vgrid **my_new_vgrid, int unit, int ip1, int ip2, int kind_sought, int version_sought)
{
  char  match_ipig;
  int error, i, ni, nj, nk;
  int toc_found = 0, count, nkeyList = MAX_DESC_REC;
  int keyList[nkeyList], status;
  VGD_TFSTD_ext var;
  double *table, *table2;
  int table_size;
  int ni_dummy, nj_dummy, nk_dummy, istat;
  int key, kind_found, version_found, vcode;
  const int KEY_NOT_FOUND = -997;
  vgrid *vgrid_first_found;

  key = KEY_NOT_FOUND;
  kind_found    = 0;
  version_found = 0;
  
  if(ip1 >= 0 && ip2 < 0)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, expecting optional value ip2\n");
    return(VGD_ERROR);
  }
  
  if(ip2 >= 0 && ip1 < 0)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, expecting optional value ip1\n");
    return(VGD_ERROR);
  }
  match_ipig = 0;
  if(ip1 >= 0)
  {
    match_ipig = 1;
  }
  if(kind_sought == -1 && version_sought != -1)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, option kind must be used with option version\n");
    return(VGD_ERROR);
  }
  
  // Get a list of !! records
  error = c_fstinl(unit, &ni_dummy, &nj_dummy, &nk_dummy, -1, " ", ip1, ip2, -1, " ", ZNAME, keyList,
		   &count, nkeyList);
  if (error < 0)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, with fstinl on nomvar !!\n");
    return(VGD_ERROR);
  }

  if(count == 0)
  {
    // There are no !! records in the file
    // Create a vgrid object using legacy techniques to extract the information from the file

    printf("(Cvgd) Cannot find %s with the following ips: ip1=%d, ip2=%d\n", ZNAME, ip1, ip2);
    if(match_ipig)
    {
      return(VGD_ERROR);
    }
    printf("(Cvgd) Trying to construct vgrid descriptor from legacy encoding (PT,HY ...)\n");
    if(c_legacy(my_new_vgrid, unit,kind_sought) == VGD_ERROR)
    {
      printf("(Cvgd) ERROR: failed to construct vgrid descriptor from legacy encoding\n");
      return(VGD_ERROR);
    }
    if((*my_new_vgrid)->fstd_init() == VGD_ERROR)
    {
      printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, problem creating record information\n");
    }
    return(VGD_OK);
  }

  else // count != 0
  {
    // Loop on all !! records found
    for( i=0; i < count; i++)
    {     
      // Check if kind_sought and version_sought match, skip the !! if not.
      // Also read all the description information for the key
      if( vgrid::correct_kind_and_version(keyList[i], kind_sought, version_sought, &var, &status) == VGD_ERROR)
      {
	return(VGD_ERROR);
      }
      if( status != 1)
      {
	continue;
      }

      // If we reached this stage then the toc satisfy the selection criteria
      // but it may not be the only such record.
      if(! toc_found)
      {
	// This is the first toctoc
	toc_found = 1;

	// Save the record information.  Load var into a vgrid.
	key = keyList[i];
	table_size = var.ni*var.nj*var.nk;
	table2 = (double*)malloc(table_size * sizeof(double));
	istat = c_fstluk(table2, keyList[i], &ni_dummy, &nj_dummy, &nk_dummy);
	if(istat < 0)
	{
	  printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, problem with fstluk\n");
	  free(table2);
	  return(VGD_ERROR);
	}
	kind_found    = (int) table2[0];
	version_found = (int) table2[1];
        vcode = kind_found*1000 + version_found;
	try
	{
          Cvgd_create_vgrid_from_vcode(&vgrid_first_found, vcode);
	}
        catch(vgrid_exception)
        {
          printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, unable to construct from a key %d, for vcode %d\n", key, vcode);
          return(VGD_ERROR);
        }
        if( vgrid_first_found->C_load_var(var) == VGD_ERROR )
        {
          printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file: cannot load !!\n");
          return(VGD_ERROR);
        }
      }
      else // A matching toctoc has already been found
      {
        // Check the new toctoc to see whether it is the same.
	// If not, return with an error message.
        if( my_fstprm(keyList[i], &var) == VGD_ERROR )
	{
          printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, with my_fstprm on keyList[i] = %d\n",
		 keyList[i]);
          return(VGD_ERROR);
        }

	// Extract the record information
	table = (double*)malloc(var.ni*var.nj*var.nk * sizeof(double));
	istat = c_fstluk(table, keyList[i], &ni_dummy, &nj_dummy, &nk_dummy);
	if(istat < 0)
	{
	  printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, problem with fstluk\n");
	  free(table);
	  return(VGD_ERROR);
	}

	// Compare the record information to that of the first matching record
	// Load the var information into a vgrid before comparison, because not all of
	// the var content is pertinent
	kind_found    = (int) table[0];
	version_found = (int) table[1];
	free(table);
        vcode = kind_found*1000 + version_found;
	try
	{
          Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);
	}
        catch(vgrid_exception)
        {
          printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, unable to construct from a key %d, for vcode %d\n", key, vcode);
          return(VGD_ERROR);
        }
        if( (*my_new_vgrid)->C_load_var(var) == VGD_ERROR )
        {
          printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file: cannot load !!\n");
          return(VGD_ERROR);
        }

	if ((*my_new_vgrid)->Cvgd_vgdcmp(vgrid_first_found) != 0)
	{
	  printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, found different entries in vertical descriptors after search on ip1 = %d, ip2 = %d, kind = %d, version = %d\n",ip1,ip2,kind_sought,version_sought);
	  return(VGD_ERROR);
	}
      } // toc_ // Loop in !!
    } // for count
  } //if(count == 0)

  if(! toc_found)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, cannot find !! or it generate from legacy encoding\n");
    return(VGD_ERROR);
  }

  vcode = kind_found*1000 + version_found;

  try
  {
    Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, unable to construct from a key %d for vcode %d\n", key, vcode);
    return(VGD_ERROR);
  }
  if( (*my_new_vgrid)->C_load_var(var) == VGD_ERROR )
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, cannot load !!\n");
    return(VGD_ERROR);
  }


  status=(*my_new_vgrid)->Cvgd_build_from_table(table2, var.ni, var.nj, var.nk);
  free(table2);

  (*my_new_vgrid)->set_match_ipig(match_ipig);

  return(status);
}



int Cvgd_create_vgrid_from_filekey(vgrid **new_vgrid, int key)
{
  double *table;
  VGD_TFSTD_ext var;
  int ni, nj, nk, table_size, istat;
  int kind, version, vcode;
  float dummy;

  // Read all the description information, var, for the key
  if( my_fstprm(key, &var) == VGD_ERROR )
  {
    printf("(Cvgd) ERROR in Cvgd_create_vgrid_from_filekey, with my_fstprm on key %d\n",
	   key);
    return(VGD_ERROR);
  }

  // Read the field into table
  ni = var.ni;
  nj = var.nj;
  nk = var.nk;
  table_size=ni*nj*nk;
  table = (double*)malloc ( table_size * sizeof(double) );
  if(! table )
  {
    printf("(Cvgd) ERROR in Cvgd_create_vgrid_from_filekey, cannot allocate table of doubles of size %d\n",table_size );
    return(VGD_ERROR);
  }
  istat = c_fstluk(table, key, &ni, &nj, &nk);
  if(istat < 0)
  {
    printf("(Cvgd) ERROR in Cvgd_create_vgrid_from_filekey, problem with fstluk\n");
    free(table);
    return(VGD_ERROR);
  }
  kind    = (int) table[0];
  version = (int) table[1];

  vcode = kind*1000 + version;
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, vcode);

    // Complete the construction of the vgrid
    (*new_vgrid)->build_vgrid_from_key(key);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, unable to construct from a key %d\n", key);
    return(VGD_ERROR);
  }
}



int Cvgd_new_build_vert2(vgrid **my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
		     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w)
{
  int vcode, status;

  try
  {
    vcode = kind*1000 + version;
    Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Cvgd_new_build_vert2, unable to construct from parameters\n");
    return(VGD_ERROR);
  }

  status=(*my_new_vgrid)->Cvgd_build_from_ab_old(
			    kind, version, nk, ip1, ip2,
			    ptop_8, pref_8, rcoef1, rcoef2,
			    rcoef3, rcoef4, a_m_8, b_m_8,
			    c_m_8, a_t_8, b_t_8, c_t_8,
			    a_w_8, b_w_8, c_w_8, ip1_m,
			    ip1_t, ip1_w, nl_m, nl_t, nl_w);

  return(status);
}


int Cvgd_new_from_table(vgrid **my_new_vgrid, double *table, int ni, int nj, int nk)
{
  int table_size, i;
  double *ltable;
  int kind, version, vcode, status;

  // Coordinate constructor - build vertical descriptor from table input

  // V V V V V   THIS SHOULD NO LONGER BE NECESSARY   V V V V V
  // Since table passed in argument may be the this->table, we take a copy before the call to free
  table_size = ni * nj * nk;
  ltable = (double*)malloc ( table_size * sizeof(double) );
  if(! ltable ) {
    printf("(Cvgd) ERROR in Cvgd_new_from_table, cannot allocate ltable of bouble of size %d\n", table_size);
    return(VGD_ERROR);
  }
  my_copy_double(table, &ltable, table_size);
  // ^ ^ ^ ^ ^  THIS SHOULD NO LONGER BE NECESSARY   ^ ^ ^ ^ ^

  kind    = (int) ltable[0];
  version = (int) ltable[1];
  vcode = kind*1000 + version;

  try
  {
    Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Cvgd_new_from_table, unable to construct from kind=%d, version=%d\n",
           kind, version);
    return(VGD_ERROR);
  }

  status=(*my_new_vgrid)->Cvgd_build_from_table(table, ni, nj, nk);

  return(status);
}

int Cvgd_new_gen2(vgrid **my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg)
{
  float *l_rcoef3 = NULL, *l_rcoef4 = NULL, minus_one = -1.;
  double *a_m_8 = NULL, *b_m_8 = NULL, *c_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL, *c_t_8 = NULL, *a_w_8 = NULL, *b_w_8 = NULL, *c_w_8 = NULL;
  int *ip1_m = NULL, *ip1_t = NULL, *ip1_w = NULL, tlift, OKInput;
  int vcode, status;

  try
  {
    vcode = kind*1000 + version;
    Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Cvgd_new_gen2, vcode %d is not valid.\n",vcode);
    return(VGD_ERROR);
  }

  status=(*my_new_vgrid)->Cvgd_build_from_hyb(kind, version, hyb, size_hyb, rcoef1, rcoef2, rcoef3, rcoef4,
	                                ptop_8, pref_8, ptop_out_8, ip1, ip2, dhm, dht, dhw, avg);

  return (status);
}





//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
//
// VCODE-SPECIFIC CREATORS
//
//_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
int Create_from_ab_1001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 1001);
    ((vgrid_1001*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_1001\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_1002(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 1002);
    ((vgrid_1002*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, ptop_8, a_m_8, b_m_8,
						    ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_1002\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_1003(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double pref_8, float rcoef1,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 1003);
    ((vgrid_1003*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, ptop_8, pref_8, rcoef1,
						    a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_1003\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_2001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8,
			double *b_m_8, int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 2001);
    ((vgrid_2001*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_2001\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_4001(vgrid** new_vgrid, int ip1, int ip2, double *a_m_8, double *b_m_8,
                         int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 4001);
    ((vgrid_4001*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_4001\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_5001(vgrid** new_vgrid, int ip1, int ip2, double ptop_8,
			double pref_8, float rcoef1,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 5001);
    ((vgrid_5001*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, ptop_8, pref_8, rcoef1,
						    a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_5001\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_5005(vgrid** new_vgrid, int ip1, int ip2,
			double pref_8, float rcoef1, float rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8,
			int *ip1_m, int *ip1_t, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 5005);
    ((vgrid_5005*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, pref_8, rcoef1, rcoef2,
                                                    a_m_8, b_m_8, a_t_8, b_t_8, ip1_m,
						    ip1_t, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_5005\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_5100(vgrid** new_vgrid, int ip1, int ip2, double pref_8, float rcoef1,
			float rcoef2, float rcoef3, float rcoef4,
			double *a_m_8, double *b_m_8, double *c_m_8,
			double *a_t_8, double *b_t_8, double *c_t_8,
			int *ip1_m, int *ip1_t, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 5100);
    ((vgrid_5100*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, pref_8, rcoef1,
						    rcoef2, rcoef3, rcoef4,
						    a_m_8, b_m_8, c_m_8,
						    a_t_8, b_t_8, c_t_8,
						    ip1_m, ip1_t, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_5100\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_5999(vgrid** new_vgrid, int ip1, int ip2,
			double *a_m_8, double *b_m_8, int *ip1_m, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 5999);
    ((vgrid_5999*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, a_m_8, b_m_8, ip1_m, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_5999\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_21001(vgrid** new_vgrid, int ip1, int ip2,
			 float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 int *ip1_m, int *ip1_t, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 21001);
    ((vgrid_21001*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2,
			 rcoef1, rcoef2, rcoef3, rcoef4,
			 a_m_8, b_m_8, c_m_8,a_t_8, b_t_8, c_t_8, ip1_m, ip1_t, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_21001\n");
    return(VGD_ERROR);
  }
}

int Create_from_ab_21002(vgrid** new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2,
			 float rcoef3, float rcoef4,
			 double *a_m_8, double *b_m_8, double *c_m_8,
			 double *a_t_8, double *b_t_8, double *c_t_8,
			 double *a_w_8, double *b_w_8, double *c_w_8,
			 int *ip1_m, int *ip1_t, int *ip1_w, int nl_m)
{
  try
  {
    Cvgd_create_vgrid_from_vcode(new_vgrid, 21002);
    ((vgrid_21002*)(*new_vgrid))->Cvgd_build_from_ab(ip1, ip2, rcoef1, rcoef2,
						     rcoef3, rcoef4,
						     a_m_8, b_m_8, c_m_8,
						     a_t_8, b_t_8, c_t_8,
						     a_w_8, b_w_8, c_w_8,
						     ip1_m, ip1_t, ip1_w, nl_m);
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in Create_from_ab_21002\n");
    return(VGD_ERROR);
  }
}





int Cvgd_new_build_vert(vgrid **my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2,
			double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t){
  if( Cvgd_new_build_vert2(my_new_vgrid, kind, version, nk, ip1, ip2, ptop_8, pref_8, rcoef1, rcoef2, NULL, NULL,
		       a_m_8, b_m_8, NULL, a_t_8, b_t_8, NULL, NULL, NULL, NULL, ip1_m, ip1_t, NULL, nl_m, nl_t, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_1001(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk){
  if( Cvgd_new_build_vert2(my_new_vgrid, 1, 1, nk, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL,
		       a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1_m, NULL, NULL, nk, 0, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_1001 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_1002(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk){
  if( Cvgd_new_build_vert2(my_new_vgrid, 1, 2, nk, ip1, ip2, &ptop_8, NULL, NULL, NULL, NULL, NULL,
		       a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1_m, NULL, NULL, nk, 0, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_1001 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_4001(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk){
  if( Cvgd_new_build_vert2(my_new_vgrid, 4, 1, nk, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL,
		       a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1_m, NULL, NULL, nk, 0, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_4001 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_5999(vgrid **my_new_vgrid, int ip1, int ip2, 
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk){
  if( Cvgd_new_build_vert2(my_new_vgrid, 5, 999, nk, ip1, ip2, NULL, NULL, NULL, NULL, NULL, NULL,
		       a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1_m, NULL, NULL, nk, 0, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_5999 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_5001(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1,
			     double *a_m_8, double *b_m_8, int *ip1_m, int nk){
  if( Cvgd_new_build_vert2(my_new_vgrid, 5, 1, 0, ip1, ip2, &ptop_8, &pref_8, &rcoef1, NULL, NULL, NULL,
		       a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1_m, NULL, NULL, nk, 0, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_5001 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_5002(vgrid **my_new_vgrid, int ip1, int ip2, double ptop_8, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t){
  if( Cvgd_new_build_vert2(my_new_vgrid, 5, 2, 0, ip1, ip2, &ptop_8, &pref_8, &rcoef1, &rcoef2, NULL, NULL,
		       a_m_8, b_m_8, NULL, a_t_8, b_t_8, NULL, NULL, NULL, NULL, ip1_m, ip1_t, NULL, nl_m, nl_t, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_5002 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_5005(vgrid **my_new_vgrid, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2,
			     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl){
  if( Cvgd_new_build_vert2(my_new_vgrid, 5, 5, 0, ip1, ip2, NULL, &pref_8, &rcoef1, &rcoef2, NULL, NULL,
		       a_m_8, b_m_8, NULL, a_t_8, b_t_8, NULL, NULL, NULL, NULL, ip1_m, ip1_t, NULL, nl, nl, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_5005 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_5100(vgrid **my_new_vgrid, int ip1, int ip2, double pref_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4,
			     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl){
  if( Cvgd_new_build_vert2(my_new_vgrid, 5, 100, 0, ip1, ip2, NULL, &pref_8, &rcoef1, &rcoef2, &rcoef3, &rcoef4 ,
		       a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, NULL, NULL, NULL, ip1_m, ip1_t, NULL, nl, nl, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_5100 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_21001(vgrid **my_new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, int *ip1_m, int *ip1_t, int nl){
  if( Cvgd_new_build_vert2(my_new_vgrid, 21, 1, 0, ip1, ip2, NULL, NULL, &rcoef1, &rcoef2, &rcoef3, &rcoef4,
		       a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, NULL, NULL, NULL, ip1_m, ip1_t, NULL, nl, nl, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_21001 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_build_vert_21002(vgrid **my_new_vgrid, int ip1, int ip2, float rcoef1, float rcoef2, float rcoef3, float rcoef4, 
			      double *a_m_8, double *b_m_8, double *c_m_8,
			      double *a_t_8, double *b_t_8, double *c_t_8,
			      double *a_w_8, double *b_w_8, double *c_w_8,
			      int *ip1_m, int *ip1_t, int *ip1_w, int nl){
  if( Cvgd_new_build_vert2(my_new_vgrid, 21, 2, 0, ip1, ip2, NULL, NULL, &rcoef1, &rcoef2, &rcoef3, &rcoef4,
		       a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, a_w_8, b_w_8, c_w_8, ip1_m, ip1_t, ip1_w, nl, nl, nl) == VGD_ERROR ){
    printf("(Cvgd) ERROR with Cvgd_new_build_vert_21002 see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}







int Cvgd_new_gen(vgrid **my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, 
	      double *ptop_8, double *pref_8, double *ptop_out_8,
		 int ip1, int ip2, float *dhm, float *dht, int avg){
  if(Cvgd_new_gen2(my_new_vgrid, kind, version, hyb, size_hyb, rcoef1, rcoef2, NULL, NULL,
		   ptop_8, pref_8, ptop_out_8,
		ip1, ip2, dhm, dht, NULL, avg) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_1001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 1, 1, hyb, size_hyb, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_1001, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_2001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 2, 1, hyb, size_hyb, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_2001, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_5999(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 5, 999, hyb, size_hyb, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_5999, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_1002(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 1, 2, hyb, size_hyb, NULL, NULL, NULL, NULL,
		&ptop_8, NULL, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_1002, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_4001(vgrid **my_new_vgrid, float *hyb, int size_hyb, int ip1, int ip2) {  
  if(Cvgd_new_gen2(my_new_vgrid, 4, 1, hyb, size_hyb, NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_4001, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_5001(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 5, 1, hyb, size_hyb, &rcoef1, NULL, NULL, NULL,
		&ptop_8, &pref_8, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_5001, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_5002(vgrid **my_new_vgrid, float *hyb, int size_hyb, double ptop_8, double pref_8, float rcoef1, float rcoef2, int ip1, int ip2) {
  if(Cvgd_new_gen2(my_new_vgrid, 5, 2, hyb, size_hyb, &rcoef1, &rcoef2, NULL, NULL,
		&ptop_8, &pref_8, NULL, ip1, ip2, NULL, NULL, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_5002, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_5005(vgrid **my_new_vgrid, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, int ip1, int ip2, float dhm, float dht) {
  if(Cvgd_new_gen2(my_new_vgrid, 5, 5, hyb, size_hyb, &rcoef1, &rcoef2, NULL, NULL,
		NULL, &pref_8, ptop_out_8, ip1, ip2, &dhm, &dht, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_5005, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}
int Cvgd_new_gen_5100(vgrid **my_new_vgrid, float *hyb, int size_hyb, double pref_8, double *ptop_out_8, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, int avg) {
  if(Cvgd_new_gen2(my_new_vgrid, 5, 100, hyb, size_hyb, &rcoef1, &rcoef2,  &rcoef3, &rcoef4,
		NULL, &pref_8, ptop_out_8, ip1, ip2, &dhm, &dht, NULL, avg) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_5100, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_gen_21001(vgrid **my_new_vgrid, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht) {
  if(Cvgd_new_gen2(my_new_vgrid, 21, 1, hyb, size_hyb, &rcoef1, &rcoef2, &rcoef3, &rcoef4,
		NULL, NULL, NULL, ip1, ip2, &dhm, &dht, NULL, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_21001, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int Cvgd_new_gen_21002(vgrid **my_new_vgrid, float *hyb, int size_hyb, float rcoef1, float rcoef2, float rcoef3, float rcoef4, int ip1, int ip2, float dhm, float dht, float dhw) {
  if(Cvgd_new_gen2(my_new_vgrid, 21, 2, hyb, size_hyb, &rcoef1, &rcoef2, &rcoef3, &rcoef4,
		NULL, NULL, NULL, ip1, ip2, &dhm, &dht, &dhw, 0) == VGD_ERROR ){
    printf("(Cvgd) ERROR in Cvgd_new_gen_21002, see details above\n");
    return(VGD_ERROR);
  }
  return(VGD_OK);
}










int C_gen_legacy_desc( vgrid **my_new_vgrid, int unit, int *keylist , int nb )
{
  int *ip1 = NULL;
  int kind, origkind, version, vcode, k, ni, nj, nk, hy_key, pt_key, e1_key;
  float ptop, rcoef;
  float *hyb = NULL;
  double ptop_8, pref_8;
  double *a_m_8 = NULL, *b_m_8 = NULL;
  VGD_TFSTD_ext var, va2;

  if(my_alloc_float (&hyb  ,nb,"(Cvgd) ERROR: in C_gen_legacy_desc, cannot allocate hyb of size")   == VGD_ERROR)
    return(VGD_ERROR);

  if( my_fstprm(keylist[0], &var) == VGD_ERROR ){
    printf("(Cvgd) ERROR: in C_gen_legacy_desc, fstprm 1 on key %d\n", keylist[0]);
    goto bomb;
  }
  hyb[0] = vgrid::c_convip_IP2Level(var.ip1,&kind);
  if( kind != 1 && kind != 2 && kind != 5 ){
    printf("(Cvgd) ERROR: in C_gen_legacy_desc, kind = %d, has to be 1, 2 or 5\n", kind);
    goto bomb;
  }
  origkind=kind;

  // Fill in the hyb array.  Also,
  // Verify the dimensions and kind at each vertical level
  for( k = 1; k < nb; k++ ){
    if( my_fstprm(keylist[k], &va2) == VGD_ERROR ){
      printf("(Cvgd) ERROR: in C_gen_legacy_desc, fstprm 2 on key %d\n", keylist[k]);
      goto bomb;
    }
    if ( va2.ni != var.ni && va2.nj != var.nj && va2.nk != var.nk ){
      printf("(Cvgd) ERROR: in C_gen_legacy_desc, dim misatch expected (%d,%d,%d), got (%d,%d,%d)\n", var.ni, var.nj, var.nk, va2.ni, va2.nj, va2.nk);
      goto bomb;
    }
    hyb[k] = vgrid::c_convip_IP2Level(va2.ip1,&kind);
    if( kind != origkind ){
      printf("(Cvgd) ERROR: in C_gen_legacy_desc, expecting kind = %d, got kind = %d\n",origkind, kind);
      goto bomb;
    }
  }

  hy_key = c_fstinf (unit,&ni,&nj,&nk,-1," ",-1,  -1,  -1," ","HY  ");
  pt_key = c_fstinf (unit,&ni,&nj,&nk,-1," ",-1,  -1,  -1," ","PT  ");
  e1_key = c_fstinf (unit,&ni,&nj,&nk,-1," ",-1,  -1,  -1," ","E1  ");


  if(kind == 1)
  {
    //============================
    // SIGMA ETA HYBRID-NORMALIZED
    //----------------------------
    if(pt_key >= 0)
    {
      if(e1_key >= 0)
        version = 4;
      else
        version = 2;
    }

    else // pt_key < 0
    {
      if(hy_key >= 0)
        version = 3;
      else
        version = 1;
    }
  }

  else // kind != 1
    version = 1;

  vcode = kind*1000 + version;


  // Verify whether HY constistant with PT
  if(  (vcode == 1002 || vcode == 1004)
         && hy_key >= 0)
  {
    // pt_key > 0
    // Verify whether HY constistant with PT
    if( vgrid::C_get_consistent_pt_e1(unit, &ptop,"PT  ") == VGD_ERROR )
    {
      printf("(Cvgd) ERROR in C_gen_legacy_desc, consistency check on PT failed\n");
      goto bomb;
    }


    if( vgrid::C_get_consistent_hy(unit, var, &va2, "HY  ") == VGD_ERROR )
    {
      printf("(Cvgd) ERROR in C_gen_legacy_record, consistency check on HY failed (1)\n");
      goto bomb;
    }
    vgrid::decode_HY(va2, &ptop_8, &pref_8, &rcoef);
    if( fabs(rcoef - 1.0) > 1.e-5)
    {
      printf("(Cvgd) ERROR in C_gen_legacy_desc, HY rcoef should by 1.0 since PT record is present in file\n");
      goto bomb;
    }
    if( fabs( ptop - ptop_8/100.) > 1.e-5 )
    {
      printf("(Cvgd) ERROR in C_gen_legacy_desc, ptop from HY is %f while it is %f in PT record\n",ptop_8/100., ptop);
      goto bomb;
    }
    printf("(Cvgd) INFO : in C_gen_legacy_desc HY record consistent with PT\n");
  }


  try
  {
    Cvgd_create_vgrid_from_vcode(my_new_vgrid, vcode);

    switch(vcode)
    {
    case 1001:
      // SIGMA SIGMA SIGMA SIGMA SIGMA SIGMA SIGMA SIGMA
      if( ! ALLOW_SIGMA )
      {
	printf("(Cvgd)   C_gen_legacy_desc error: sigma coordinate construction is not ALLOWED.\n(Cvgd)       If your are certain that you want this sigma coordinate, set ALLOW_SIGMA to true e.g.\n(Cvgd)          in fortran stat =  vgd_putopt(\"ALLOW_SIGMA\",.true.)\n(Cvgd)          in C       stat = Cvgd_putopt_int(\"ALLOW_SIGMA\",1)\n");
	goto bomb;
      }
      printf("(Cvgd)   sigma coordinate found\n");
      if( ((vgrid_1001*)(*my_new_vgrid))->C_genab(hyb, nb, &a_m_8, &b_m_8, &ip1) == VGD_ERROR )
      {
      	goto bomb;
      }
      if(Cvgd_new_build_vert2(my_new_vgrid, kind, 1, nb, var.ig1, var.ig2, NULL, NULL, NULL, NULL, NULL, NULL, a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1, NULL, NULL, nb, 0, 0) == VGD_ERROR )
      {
	goto bomb;
      }
      break;

    case 1002:
      //=============================================
      // PT PT PT PT PT PT PT PT PT PT PT PT PT PT PT
      //---------------------------------------------
      printf("(Cvgd)   eta coordinate found\n");
      if( vgrid::C_get_consistent_pt_e1(unit, &ptop,"PT  ") == VGD_ERROR ){
	printf("(Cvgd) ERROR in C_gen_legacy_desc, consistency check on PT failed\n");
	goto bomb;
      }
      ptop_8 = ptop*100.;
      if( ((vgrid_1002*)(*my_new_vgrid))->C_genab(hyb, nb, &ptop_8, &a_m_8, &b_m_8, &ip1) == VGD_ERROR )
      {	  
        goto bomb;
      }
      if(Cvgd_new_build_vert2(my_new_vgrid, kind, 2, nb, var.ig1, var.ig2, &ptop_8, NULL, NULL, NULL, NULL, NULL, a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1, NULL, NULL, nb, 0, 0) == VGD_ERROR )
      {
        goto bomb;
      }
      break;

    case 1003:
      //================================================
      // HY HY HY HY HY HY HY HY HY HY HY HY HY HY HY HY
      //------------------------------------------------
      printf("(Cvgd)   hybrid (normalized) coordinate found\n");
      if( vgrid::C_get_consistent_hy(unit, var, &va2, "HY  ") == VGD_ERROR ){
	printf("(Cvgd) ERROR in C_gen_legacy_record, consistency check on HY failed (2)\n");
	goto bomb;
      }
      vgrid::decode_HY(va2, &ptop_8, &pref_8, &rcoef);
      if( ((vgrid_1003*)(*my_new_vgrid))->C_genab(hyb, nb, rcoef, ptop_8, pref_8, &a_m_8, &b_m_8, &ip1) == VGD_ERROR ) {      
	goto bomb;
      }
      if(Cvgd_new_build_vert2(my_new_vgrid, 1, 3, nb, var.ig1, var.ig2, &ptop_8, &pref_8, &rcoef, NULL, NULL, NULL, a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1, NULL, NULL, nb, 0, 0) == VGD_ERROR ){
	goto bomb;
      }   
      break;   

    case 1004:
      printf("(Cvgd) TODO in C_gen_legacy_desc, add support to 1004 etasef coordinate");
      goto bomb;
      break;

    case 2001:
      printf("(Cvgd)   pressure coordinate found\n");
      if( ((vgrid_2001*)(*my_new_vgrid))->C_genab(hyb, nb, &a_m_8, &b_m_8, &ip1) == VGD_ERROR )
      {
        goto bomb;
      }
      if(Cvgd_new_build_vert2(my_new_vgrid, kind, 1, nb, var.ig1, var.ig2, NULL, NULL, NULL, NULL, NULL, NULL, a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1, NULL, NULL, nb, 0, 0) == VGD_ERROR )
      {
        goto bomb;
      }	
      break;

    case 5001:
      printf("(Cvgd)   Hybrid coordinate found\n");
      if( vgrid::C_get_consistent_hy(unit, var, &va2, "HY  ") == VGD_ERROR )
      {
        printf("(Cvgd) ERROR in C_gen_legacy_desc, consistency check on HY failed\n");
        goto bomb;
      }
      vgrid::decode_HY(va2, &ptop_8, &pref_8, &rcoef);
      if( ((vgrid_5001*)(*my_new_vgrid))->C_genab(hyb, nb, rcoef, ptop_8, pref_8, &a_m_8, &b_m_8, &ip1) == VGD_ERROR )
      {
        goto bomb;
      }
      if(Cvgd_new_build_vert2(my_new_vgrid, kind, 1, nb, var.ig1, var.ig2, &ptop_8, &pref_8, &rcoef, NULL, NULL, NULL, a_m_8, b_m_8, NULL, NULL, NULL, NULL, NULL, NULL, NULL, ip1, NULL, NULL, nb, 0, 0) == VGD_ERROR )
      {
        goto bomb;
      }
      break;

    default:
      printf("(Cvgd ERROR: in C_gen_legacy_desc, kind %d is not supported\n",kind);
      return(VGD_ERROR);
    }
  }
  catch (vgrid_exception)
  {
    goto bomb;
  }

  free(ip1);
  free(hyb);
  free(a_m_8);
  free(b_m_8);
  return(VGD_OK);
  
 bomb:
  free(ip1);
  free(hyb);
  free(a_m_8);
  free(b_m_8);
  return(VGD_ERROR);

}

int c_legacy(vgrid **my_new_vgrid, int unit, int F_kind)
{
  // Construct vertical structure from legacy encoding (PT,HY...)

  int error, ni, nj, nk, nip1, i, j, k, kind, nb_kind=100, aa, nb;
  int count, nkeylist = MAX_DESC_REC, valid_kind;
  int keylist[nkeylist], ip1list[nkeylist], num_in_kind[nb_kind];
  float preslist[nkeylist], xx, f_zero=0.f;
  VGD_TFSTD_ext var;

  for( i = 0; i < nb_kind; i++){
    num_in_kind[i] = 0;
  }

  printf("(Cvgd) Looking for kind = %d\n",F_kind);

  error = c_fstinl(unit, &ni, &nj, &nk, -1, " ", -1, -1, -1, " ", " ", keylist, &count, nkeylist);
  if (error < 0) {
    printf("(Cvgd) ERROR in c_legacy, with fstinl\n");
    return(VGD_ERROR);
  }
  nip1 = 0;
  for( i = 0; i < count; i++){
    error = my_fstprm(keylist[i], &var);
    if (error == VGD_ERROR) {
      printf("(Cvgd) ERROR in c_legacy, error return from fstprm wrapper for fst key = %d",keylist[i]);
      return(VGD_ERROR);
    }
    preslist[i] = vgrid::c_convip_IP2Level(var.ip1,&kind);
    if( strcmp(var.nomvar, ">>  ") == 0 )
      continue;
    if( strcmp(var.nomvar, "^^  ") == 0 )
      continue;
    if( strcmp(var.nomvar, "^>  ") == 0 )
      continue;
    if( strcmp(var.nomvar, "P0   ") == 0 )
      continue;
    if( strcmp(var.nomvar, "PT  ") == 0 )
      continue;
    if( strcmp(var.nomvar, "HY  ") == 0 )
      continue;
    if( kind == 2 ){
      // Pressure at 0.0 is not part of the vertical structure
      if( memcmp( &(preslist[i]), &f_zero, sizeof(float)/sizeof(char) ) == 0 )
	continue;
    }
    if(F_kind > 0 && kind != F_kind)
      continue;
    if(kind == 1 || kind == 2 || kind == 5) {
      num_in_kind[kind] = num_in_kind[kind]+1;
      ip1list[nip1]=var.ip1;
      keylist[nip1]=keylist[i];
      preslist[nip1]=preslist[i];
      valid_kind=kind;
      nip1++;
    }
  }
  if(max_int(num_in_kind,nb_kind) != nip1){
    printf("(Cvgd) ERROR: more than one pressure/sigma/hyb coordinate in file\n");
    for(i = 0; i < nb_kind; i++){
      if(num_in_kind[i] > 0) {
	printf("(Cvgd)           There are %d records of kind %d\n",num_in_kind[i],i);
      }
    }
    return(VGD_ERROR);
  }
  // Sort levels in ascending order
  for( i = 0; i < nip1 - 1; i++){
    k = i;
    for( j = i + 1; j < nip1; j++){
      if( preslist[j] < preslist[k] )
	k = j;
    }
    if( k != i ){
      // hyb
      xx          = preslist[k];
      preslist[k] = preslist[i];
      preslist[i] = xx;
      // ip1
      aa          = ip1list[k];
      ip1list[k]  = ip1list[i];
      ip1list[i]  = aa;	
      // fstkey
      aa          = keylist[k];
      keylist[k]  = keylist[i];
      keylist[i]  = aa;
    }
  }
  // Remove duplictate (there must be a better way to do this)
  for( i = 0; i < nip1-1; i++ ){
    if( ip1list[i] != -1 ){
      for( j = i+1; j < nip1; j++ ){
	if( ip1list[j] == ip1list[i] ){
	  ip1list[j] = -1;
	}
      }
    }
  }
  nb=0;
  for( i = 0; i < nip1; i++ ){
    if( ip1list[i] != -1 ){
      ip1list[nb]  = ip1list[i];
      keylist[nb]  = keylist[i];
      // pres is not used below but adjusting for consistency and possible future use.
      preslist[nb] = preslist[i];
      nb++;
    }
  }
  if( nb == 0){
    printf("(Cvgd) ERROR: No record of type pressure/sigma/hyb in file\n");
    return(VGD_ERROR);
  }
  printf("(Cvgd)   Found %d unique ip1 of kind %d among the %d records in file to construct the vertical descriptor\n", nb, valid_kind, count);
  error = C_gen_legacy_desc(my_new_vgrid, unit, keylist , nb);

  if( error == VGD_ERROR ){
    printf("(Cvgd) ERROR: problem with C_gen_legacy_desc\n");
    return(VGD_ERROR);
  }  
  printf("(Cvgd)   Vertical descriptor successfully reconstructed\n");
  return(VGD_OK);
}
