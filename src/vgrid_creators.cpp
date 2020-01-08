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
int vgrid::Cvgd_read_vgrid_from_file(vgrid **my_new_vgrid, int unit, int ip1, int ip2, int kind_sought, int version_sought)
{
  char  match_ipig;
  int error, i, ni, nj, nk;
  int toc_found = 0, count, nkeyList = MAX_DESC_REC;
  int keyList[nkeyList], status;
  VGD_TFSTD_ext var, var2;
  double *table, *table2;
  int table_size;
  int ni_dummy, nj_dummy, nk_dummy, istat;
  int key, kind_found, version_found, vcode;
  const int KEY_NOT_FOUND = -997;

  key = KEY_NOT_FOUND;
  kind_found    = 0;
  version_found = 0;
  
  if(ip1 >= 0 && ip2 < 0)
  {
    printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, expecting optional value ip2\n");      
    return(VGD_ERROR);
  }
  
  if(ip2 >= 0 && ip1 < 0)s
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
  error = c_fstinl(unit, &ni, &nj, &nk, -1, " ", ip1, ip2, -1, " ", ZNAME, keyList, &count, nkeyList);
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
      this->vcode = -1;
      return(VGD_ERROR);
    }
    printf("(Cvgd) Trying to construct vgrid descriptor from legacy encoding (PT,HY ...)\n");
    if(this->c_legacy(my_new_vgrid, unit,kind_sought) == VGD_ERROR)
    {
      printf("(Cvgd) ERROR: failed to construct vgrid descriptor from legacy encoding\n");
      return(VGD_ERROR);
    }
    if(*my_new_vgrid->fstd_init() == VGD_ERROR)
    {
      printf("(Cvgd) ERROR in Cvgd_read_vgrid_from_file, problem creating record information\n");
    }
    return(VGD_OK);
  }

  else
  {
    // Loop on all !! records found
    for( i=0; i < count; i++)
    {     
      // Check if kind_sought and version_sought match, skip the !! if not.
      // Also read all the description information for the key
      if( correct_kind_and_version(keyList[i], kind_sought, version_sought, &var, &status) == VGD_ERROR)
      {
	this->valid = 0;
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

	// Save the record information
	key = keyList[i];
	var2 = var;
	table_size = var2.ni*var2.nj*var2.nk;
	table2 = (double*)malloc(table_size * sizeof(double));
	istat = c_fstluk(table2, keyList[i], &ni_dummy, &nj_dummy, &nk_dummy);
	if(istat < 0)
	{
	  printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, problem with fstluk\n");
	  free(table2);
	  return(VGD_ERROR);
	}
	kind_found    = (int) table2[0];
	version_found = (int) table2[1];
	free(table2);
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
	  printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, problem with fstluk\n");
	  free(table);
	  return(VGD_ERROR);
	}
	kind_found    = (int) table[0];
	version_found = (int) table[1];
	free(table);

	// Compare the record information to that of the first matching record
	if (   var     != var2
	    || kind_sought    != kind_found
	    || version_sought != version_found
	   )
	{
	  printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, found different entries in vertical descriptors after search on ip1 = %d, ip2 = %d, kind = %d, version = %d\n",ip1,ip2,kind_found,version_found);
	  return(VGD_ERROR);
	}
      } // toc_ // Loop in !!
    } // for count
  } //if(count == 0)

  if(! toc_found)
  {
    printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, cannot find !! or it generate from legacy encoding\n");
    return(VGD_ERROR);
  }

  vcode = kind_found*1000 + version_found;

  try
  {
    // Instantiate a vgrid subclass from the key, according to the vcode
    switch (vcode)
    {
    case 0001:
      vgrid_0001 new_vgrid_0001(key);
      *my_new_vgrid = & new_vgrid_0001;

    case 1001:
      vgrid_1001 new_vgrid_1001(key);
      *my_new_vgrid = & new_vgrid_1001;

    case 1002:
      vgrid_1002 new_vgrid_1002(key);
      *my_new_vgrid = & new_vgrid_1002;

    case 1003:
      vgrid_1003 new_vgrid_1003(key);
      *my_new_vgrid = & new_vgrid_1003;

    case 2001:
      vgrid_2001 new_vgrid_2001(key);
      *my_new_vgrid = & new_vgrid_2001;

    case 4001:
      vgrid_4001 new_vgrid_4001(key);
      *my_new_vgrid = & new_vgrid_4001;

    case 5001:
      vgrid_5001 new_vgrid_5001(key);
      *my_new_vgrid = &new_vgrid_5001;

    case 5002:
      vgrid_5002 new_vgrid_5002(key);
      *my_new_vgrid = &new_vgrid_5002;

    case 5003:
      vgrid_5003 new_vgrid_5003(key);
      *my_new_vgrid = &new_vgrid_5003;

    case 5004:
      vgrid_5004 new_vgrid_5004(key);
      *my_new_vgrid = &new_vgrid_5004;

    case 5005:
      vgrid_5005 new_vgrid_5005(key);
      *my_new_vgrid = &new_vgrid_5005;

    case 5100:
      vgrid_5100 new_vgrid_5100(key);
      *my_new_vgrid = & new_vgrid_5100;

    case 5999:
      vgrid_5999 new_vgrid_5999(key);
      *my_new_vgrid = & new_vgrid_5999;

    case 21001:
      vgrid_21001 new_vgrid_21001(key);
      *my_new_vgrid = & new_vgrid_21001;

    case 21002:
      vgrid_21002 new_vgrid_21002(key);
      *my_new_vgrid = & new_vgrid_21002;

    default:
    printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, unrecognized vcode=%s\n",vcode);
    return(VGD_ERROR);
    }
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in vgrid::Cvgd_read_vgrid_from_file, unable to construct from a key %d\n", key);
    return(VGD_ERROR);
  }
  this->match_ipig = match_ipig;

  return(VGD_OK);
}



int vgrid::Cvgd_new_build_vert2(vgrid *my_new_vgrid, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
		     double *a_m_8, double *b_m_8, double *c_m_8, double *a_t_8, double *b_t_8, double *c_t_8, double *a_w_8, double *b_w_8, double *c_w_8, int *ip1_m, int *ip1_t, int *ip1_w, int nl_m, int nl_t, int nl_w)
{
  int errorInput = 0, vcode;


  try
  {
    vgrid new_vgrid(kind, version);
    *my_new_vgrid = &new_vgrid
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in vgrid::Cvgd_new_build_vert2, unable to construct from parameters\n");
    return(VGD_ERROR);
  }

  // Complete the initializations
  my_new_vgrid->unit       = -1;
  my_new_vgrid->match_ipig = 1;
  my_new_vgrid->nk         = nk;
  my_new_vgrid->nl_m       = nl_m;
  // Note that my_new_vgrid->nl_t and my_new_vgrid->nl_w may be overwritten in c_encode_vert function below
  my_new_vgrid->nl_t       = nl_t;
  my_new_vgrid->nl_w       = nl_w;
  my_new_vgrid->rec.ip1    = (int) fmax(0,ip1);
  my_new_vgrid->rec.ip2    = (int) fmax(0,ip2);
  strcpy(my_new_vgrid->rec.nomvar,"!!  ");
  my_new_vgrid->rec.ig1   = my_new_vgrid->vcode;



  // Check for required inputs
  if( my_new_vgrid->is_valid( ptop_8_valid) ) {
    if(ptop_8) {
      my_new_vgrid->ptop_8 = *ptop_8;
    } else {
      printf("(Cvgd) ptop_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( pref_8_valid)) {
    if(pref_8){
      my_new_vgrid->pref_8 = *pref_8;
    } else {
      printf("(Cvgd) pref_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( rcoef1_valid)) {
    if(rcoef1){
      my_new_vgrid->rcoef1 = *rcoef1;
    } else {
      printf("(Cvgd) rcoef1 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( rcoef2_valid)) {
    if(rcoef2){
      my_new_vgrid->rcoef2 = *rcoef2;
    } else {
      printf("(Cvgd) rcoef2 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( rcoef3_valid)) {
    if(rcoef3){
      my_new_vgrid->rcoef3 = *rcoef3;
    } else {
      printf("(Cvgd) rcoef3 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( rcoef4_valid)) {
    if(rcoef4){
      my_new_vgrid->rcoef4 = *rcoef4;
    } else {
      printf("(Cvgd) rcoef4 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( a_m_8_valid)) {
    if(a_m_8){
      free(my_new_vgrid->a_m_8);
      my_new_vgrid->a_m_8 = (double*)malloc( nl_m * sizeof(double) );
      if(! my_new_vgrid->a_m_8){ 
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating a_m_8 of size = %d\n", nl_m);
	return(VGD_ERROR);
      }
      my_copy_double(a_m_8, &(my_new_vgrid->a_m_8), nl_m);
    } else {
      printf("(Cvgd) a_m_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( b_m_8_valid)) {
    if(b_m_8){
      free(my_new_vgrid->b_m_8);
      my_new_vgrid->b_m_8 = (double*)malloc( nl_m * sizeof(double) );
      if(! my_new_vgrid->b_m_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating b_m_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_m_8, &(my_new_vgrid->b_m_8), nl_m);
    } else {
      printf("(Cvgd) b_m_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( c_m_8_valid)) {
    if(c_m_8){
      free(my_new_vgrid->c_m_8);
      my_new_vgrid->c_m_8 = (double*)malloc( nl_m * sizeof(double) );
      if(! my_new_vgrid->c_m_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating c_m_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(c_m_8, &(my_new_vgrid->c_m_8), nl_m);
    } else {
      printf("(Cvgd) c_m_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( a_t_8_valid)) {
    if(a_t_8){
      free(my_new_vgrid->a_t_8);
      my_new_vgrid->a_t_8 = (double*)malloc( nl_t * sizeof(double) );
      if(! my_new_vgrid->a_t_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating a_t_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_t_8, &(my_new_vgrid->a_t_8), nl_t);
    } else {
      printf("(Cvgd) a_t_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( b_t_8_valid)) {
    if(b_t_8){
      free(my_new_vgrid->b_t_8);
      my_new_vgrid->b_t_8 = (double*)malloc( nl_t * sizeof(double) );
      if(! my_new_vgrid->b_t_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating b_t_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_t_8, &(my_new_vgrid->b_t_8), nl_t);
    } else {
      printf("(Cvgd) b_t_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( c_t_8_valid)) {
    if(c_t_8){
      free(my_new_vgrid->c_t_8);
      my_new_vgrid->c_t_8 = (double*)malloc( nl_t * sizeof(double) );
      if(! my_new_vgrid->c_t_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating c_t_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(c_t_8, &(my_new_vgrid->c_t_8), nl_t);
    } else {
      printf("(Cvgd) c_t_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( a_w_8_valid)) {
    if(a_w_8){
      free(my_new_vgrid->a_w_8);
      my_new_vgrid->a_w_8 = (double*)malloc( nl_w * sizeof(double) );
      if(! my_new_vgrid->a_w_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating a_w_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_w_8, &(my_new_vgrid->a_w_8), nl_w);
    } else {
      printf("(Cvgd) a_w_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( b_w_8_valid)) {
    if(b_w_8){
      free(my_new_vgrid->b_w_8);
      my_new_vgrid->b_w_8 = (double*)malloc( nl_w * sizeof(double) );
      if(! my_new_vgrid->b_w_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating b_w_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_w_8, &(my_new_vgrid->b_w_8), nl_w);
    } else {
      printf("(Cvgd) b_w_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( c_w_8_valid)) {
    if(c_w_8){
      free(my_new_vgrid->c_w_8);
      my_new_vgrid->c_w_8 = (double*)malloc( nl_w * sizeof(double) );
      if(! my_new_vgrid->c_w_8) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating c_w_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(c_w_8, &(my_new_vgrid->c_w_8), nl_w);
    } else {
      printf("(Cvgd) c_w_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }

  if(my_new_vgrid->is_valid( ip1_m_valid)) {
    if(ip1_m){
      free(my_new_vgrid->ip1_m);
      my_new_vgrid->ip1_m = (int*)malloc( nl_m * sizeof(int) );
      if(! my_new_vgrid->ip1_m) {
	printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem allocating ip1_m in Cvgd_new_build_vert2\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_m, &(my_new_vgrid->ip1_m), nl_m);
    } else {
      printf("(Cvgd) ip1_m is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( ip1_t_valid)) {
    if(ip1_t){
      free(my_new_vgrid->ip1_t);
      my_new_vgrid->ip1_t = (int*)malloc( nl_t * sizeof(int) );
      if(! my_new_vgrid->ip1_t) {
	printf("(Cvgd) ERROR: in Cvgd_new_build_vert2, problem allocating ip1_t\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_t, &(my_new_vgrid->ip1_t), nl_t);
    } else {
      printf("(Cvgd) ip1_t is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(my_new_vgrid->is_valid( ip1_w_valid)) {
    if(ip1_w){
      free(my_new_vgrid->ip1_w);
      my_new_vgrid->ip1_w = (int*)malloc( nl_w * sizeof(int) );
      if(! my_new_vgrid->ip1_w) {
	printf("(Cvgd) ERROR: in Cvgd_new_build_vert2, problem allocating ip1_w\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_w, &(my_new_vgrid->ip1_w), nl_w);
    } else {
      printf("(Cvgd) ip1_w is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if (errorInput > 0)
  {
    return (VGD_ERROR);
  }



  // Fill the table (encode the vertical co-ordinate)
  if(my_new_vgrid->allocate_table(nk) == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem with allocate_table for vcode=_%s\n",my_new_vgrid->vcode);
    return(VGD_ERROR);
  }
  if(my_new_vgrid->c_encode_vert() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem with c_encode_vert for vcode=_%s\n",my_new_vgrid->vcode);
    return(VGD_ERROR);
  }


  my_new_vgrid->valid = 1;
  if(my_new_vgrid->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_new_build_vert2, problem with fstd_init\n");
  }

  return(VGD_OK);
}


int vgrid::Cvgd_new_from_table(vgrid **my_new_vgrid, double *table, int ni, int nj, int nk) {
  int table_size, i;
  double *ltable;
  int kind, version;

  // Coordinate constructor - build vertical descriptor from table input
  // Set internal vcode (if all above was successful)

  this->valid = 0;

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


  try
  {
    vgrid new_vgrid(kind, version);
    *my_new_vgrid = &new_vgrid;
  }
  catch(vgrid_exception)
  {
    printf("(Cvgd) ERROR in vgrid::Cvgd_new_from_table, unable to construct from kind=%d, version=%d\n",
           kind, version);
    return(VGD_ERROR);
  }


  my_new_vgrid->table_ni = ni;
  my_new_vgrid->table_nj = nj;
  my_new_vgrid->table_nk = nk;

  my_new_vgrid->table = (double*)malloc ( ni * nj * nk * sizeof(double) );
  if(! my_new_vgrid->table ) {
    printf("(Cvgd) ERROR in Cvgd_new_from_table, cannot allocate table of double of size %d\n",table_size );
    return(VGD_ERROR);
  }
  for(i = 0; i < table_size; i++) {
    my_new_vgrid->table[i] = ltable[i];
  }
  free(ltable);

  // Fill remainder of structure
  if( my_new_vgrid->c_decode_vert() == VGD_ERROR )
  {
    printf("(Cvgd) in Cvgd_new_from_table, problem decoding table with vcode %d\n", this->vcode);
    return(VGD_ERROR);
  }

  this->valid = 1;
  if(this->fstd_init() == VGD_ERROR)
  {
    printf("(Cvgd) ERROR in Cvgd_new_from_table, problem creating record information\n");
  }

  return(VGD_OK);
}

int vgrid::Cvgd_new_gen2(vgrid **my_new_vgrid, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2, float *rcoef3, float *rcoef4,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, float *dhm, float *dht, float *dhw, int avg){

  float *l_rcoef3 = NULL, *l_rcoef4 = NULL, minus_one = -1.;
  double *a_m_8 = NULL, *b_m_8 = NULL, *c_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL, *c_t_8 = NULL, *a_w_8 = NULL, *b_w_8 = NULL, *c_w_8 = NULL;
  int *ip1_m = NULL, *ip1_t = NULL, *ip1_w = NULL, tlift, OKInput;

  if(this->Cvgd_set_vcode_i(kind, version) == VGD_ERROR)  {
    printf("(Cvgd) ERROR in Cvgd_new_gen2, ERROR with Cvgd_set_vcode_i");
    return (VGD_ERROR);
  }
  if( ! this->is_valid(vcode_valid) ){
    printf("(Cvgd) ERROR in Cvgd_new_gen2, vcode %d is not valid.\n",this->vcode);
    return (VGD_ERROR);
  }
  
  //TODO get better error handling like in new_build
  OKInput = 0;
  OKInput = OKInput + this->is_required_double(ptop_8,     ptop_8_valid,     "ptop_8"    );
  OKInput = OKInput + this->is_required_double(ptop_out_8, ptop_out_8_valid, "ptop_out_8");
  OKInput = OKInput + this->is_required_double(pref_8,     pref_8_valid,     "pref_8"    );
  OKInput = OKInput + this->is_required_float (rcoef1,     rcoef1_valid,     "rcoef1"    );
  OKInput = OKInput + this->is_required_float (rcoef2,     rcoef2_valid,     "rcoef2"    );
  OKInput = OKInput + this->is_required_float (dhm,        dhm_valid,        "dhm"       );
  OKInput = OKInput + this->is_required_float (dht,        dht_valid,        "dht"       );  
  OKInput = OKInput + this->is_required_float (dhw,        dhw_valid,        "dhw"       );  

  if (OKInput != 8 ) {
    return(VGD_ERROR);
  }
  OKInput = 0;
  if(this->is_option(rcoef3_option)) {
    OKInput++;
    l_rcoef3 = &minus_one;
    if(rcoef3){
      l_rcoef3 = rcoef3;
    }
  } else {    
    OKInput = OKInput + this->is_required_float (rcoef3, rcoef3_valid, "rcoef3");
    l_rcoef3 = rcoef3;
  }
  if(this->is_option(rcoef4_option)) {
    OKInput++;
    l_rcoef4 = &minus_one;
    if(rcoef4){
      l_rcoef4 = rcoef4;
    }
  } else {    
    OKInput = OKInput + this->is_required_float (rcoef4, rcoef4_valid, "rcoef4");
    l_rcoef4 = rcoef4;
  }  
  if (OKInput != 2 ) {
    return(VGD_ERROR);
  }
  int nk = -1, nl_m = -1, nl_t = -1, nl_w = -1;

  try
  {
    vgrid new_vgrid(vcode);
    *my_new_vgrid = &new_vgrid;

    switch(vcode) 
    {
    case 1:	
      fprintf(stderr,"(Cvgd) ERROR in Cvgd_new_gen2, kind=%d, version=%d\n cannot be generated, function to di so is in Nemo\n",kind,version);
      return(VGD_ERROR);
      break;

    case 1001:	
      nk   = size_hyb;
      nl_m = size_hyb;
      nl_t = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        return(VGD_ERROR);
      }
      break;

  case 1002:
    nk   = size_hyb;
    nl_m = size_hyb;
    nl_t = size_hyb;
    if(my_new_vgrid->C_genab(hyb, size_hyb, ptop_8, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR )
    {
      free(a_m_8);
      free(b_m_8);
      free(ip1_m);
      return(VGD_ERROR);
    }
    break;

    case 1003:
      fprintf(stderr,"(Cvgd) ERROR in Cvgd_new_gen2, kind=%d, version=%d\n cannot be generated, please use kind 1 of version 2\n",kind,version);
      return(VGD_ERROR);
      break;

    case 2001:
      nk   = size_hyb;
      nl_m = size_hyb;
      nl_t = -1;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        return(VGD_ERROR);
      }
      break;

  case 4001:
    nk   = size_hyb;
    nl_m = size_hyb;
    nl_t = -1;
    if(my_new_vgrid->C_genab(hyb, size_hyb, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR )
    {
      free(a_m_8);
      free(b_m_8);
      free(ip1_m);
      return(VGD_ERROR);
    }
    break;

    case 5001:
      nk   = size_hyb;
      nl_m = size_hyb;
      nl_t = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, *rcoef1, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        return(VGD_ERROR);
      }
      break;

    case 5002:
      nk   = size_hyb;
      tlift = 0;
      if(my_new_vgrid->C_genab_5002_5003(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, tlift) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }    
      break;

    case 5003:
      nk   = size_hyb;
      tlift = 1;
      if(my_new_vgrid->C_genab_5002_5003(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, tlift) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }    
      break;

    case 5004:
      nk   = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }    
      break;

    case 5005:
      nk   = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, &ptop_out_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, *dhm, *dht) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }
      break;

    case 5100:
      nk   = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *l_rcoef3, *l_rcoef4, &ptop_out_8, *pref_8, &a_m_8, &b_m_8, &c_m_8, &ip1_m, &a_t_8, &b_t_8, &c_t_8, &ip1_t, *dhm, *dht, avg) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(c_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(c_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }
      break;

    case 21001:
      nk   = size_hyb;
      if(my_new_vgrid->C_genab(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *l_rcoef3, *l_rcoef4, &a_m_8, &b_m_8, &c_m_8, &ip1_m, &a_t_8, &b_t_8, &c_t_8, &ip1_t, *dhm, *dht) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(c_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(c_t_8);
        free(ip1_t);
        return(VGD_ERROR);
      }
      break;

    case 21002:
      nk   = size_hyb;    
      if(my_new_vgrid->C_genab(hyb, size_hyb, &nl_m, &nl_t, &nl_w, *rcoef1, *rcoef2, *l_rcoef3, *l_rcoef4, &a_m_8, &b_m_8, &c_m_8, &ip1_m, &a_t_8, &b_t_8, &c_t_8, &ip1_t, &a_w_8, &b_w_8, &c_w_8, &ip1_w, *dhm, *dht, *dhw) == VGD_ERROR )
      {
        free(a_m_8);
        free(b_m_8);
        free(c_m_8);
        free(ip1_m);
        free(a_t_8);
        free(b_t_8);
        free(c_t_8);
        free(ip1_t);
        free(a_w_8);
        free(b_w_8);
        free(c_w_8);
        free(ip1_w);
        return(VGD_ERROR);
      }
      break;

    default:
      printf("(Cvgd) ERROR in Cvgd_new_gen2, invalid kind or version, kind = %d, version = %d\n",kind,version);
      return(VGD_ERROR);
    }
  }
  catch (vgrid_exception)
  {
    free(a_m_8);
    free(b_m_8);
    free(c_m_8);
    free(a_t_8);
    free(b_t_8);
    free(c_t_8);
    free(ip1_m);
    free(ip1_t);

    return(VGD_ERROR);
  }
  if( VGD_ERROR == Cvgd_new_build_vert2(my_new_vgrid,kind,version,nk,ip1,ip2,ptop_8,pref_8,rcoef1,rcoef2,l_rcoef3,l_rcoef4,a_m_8,b_m_8,c_m_8,a_t_8,b_t_8,c_t_8,a_w_8,b_w_8,c_w_8,ip1_m,ip1_t,ip1_w,nl_m,nl_t,nl_w) ) {
    fprintf(stderr,"(Cvgd) ERROR in Cvgd_new_gen2, problem with new_build_vert for kind = %d, version = %d\n",kind,version);
    return(VGD_ERROR);
  }
  free(a_m_8);
  free(b_m_8);
  free(c_m_8);
  free(a_t_8);
  free(b_t_8);
  free(c_t_8);
  free(ip1_m);  
  free(ip1_t);  

  return (VGD_OK);
}
