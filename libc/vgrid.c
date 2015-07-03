#include "vgrid.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "rpnmacros.h"

#define STR_INIT(str,len) if(len>1) memset(str,' ',len-1); if(len>0) str[len-1] = '\0'

// Constants
#define MAX_DESC_REC 10000      //maximum number of descriptor records in a single file
#define MAX_VKIND    100
#define ZNAME "!!"              //name of the vertical coodinate
// Macros
#define FREE(x) if(x) { free(x); x=NULL; }

// Validity table for self
#define VALID_TABLE_SIZE 9

int ptop_out_8_valid [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0,    0,    0, 5004, 5005};
int ptop_8_valid     [VALID_TABLE_SIZE] = {    0, 1002, 1003,    0, 5001, 5002, 5003, 5004,    0};
int pref_8_valid     [VALID_TABLE_SIZE] = {    0,    0,    0, 1003, 5001, 5002, 5003, 5004, 5005};
int rcoef1_valid     [VALID_TABLE_SIZE] = {    0,    0,    0, 1003, 5001, 5002, 5003, 5004, 5005};
int rcoef2_valid     [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005};
int a_m_8_valid      [VALID_TABLE_SIZE] = { 1001, 1002, 1003, 2001, 5001, 5002, 5003, 5004, 5005};
int b_m_8_valid      [VALID_TABLE_SIZE] = { 1001, 1002, 1003, 2001, 5001, 5002, 5003, 5004, 5005};
int a_t_8_valid      [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005};
int a_t_8_valid_get  [VALID_TABLE_SIZE] = { 1001, 1002,    0, 2001, 5001, 5002, 5003, 5004, 5005};
int b_t_8_valid      [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005};
int b_t_8_valid_get  [VALID_TABLE_SIZE] = { 1001, 1002,    0, 2001, 5001, 5002, 5003, 5004, 5005};
int ip1_m_valid      [VALID_TABLE_SIZE] = { 1001, 1002, 1003, 2001, 5001, 5002, 5003, 5004, 5005};
int ip1_t_valid      [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0, 5002, 5003, 5004, 5005};
int ip1_t_valid_get  [VALID_TABLE_SIZE] = { 1001, 1002,    0, 2001, 5001, 5002, 5003, 5004, 5005};
int ref_name_valid   [VALID_TABLE_SIZE] = { 1001, 1002, 1003,    0, 5001, 5002, 5003, 5004, 5005};
int dhm_valid        [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0,    0,    0,    0, 5005};
int dht_valid        [VALID_TABLE_SIZE] = {    0,    0,    0,    0,    0,    0,    0,    0, 5005};

int is_valid(TVGrid *self, int *table_valid)
{
  int k;
  for( k = 0; k < VALID_TABLE_SIZE; k++){
    if(self->vcode == table_valid[k]){
      return(1);
    }
  }
  return 0;
}

int C_is_valid(TVGrid *self, char *valid_table_name)
{
  if(! self){
    printf("ERROR (vgd) : in C_is_valid vgrid descriptor not constructed\n");
    return(VGD_MISSING);
  }
  if( strcmp(valid_table_name, "SELF") == 0 ){
    return(self->valid);
  } else if( strcmp(valid_table_name, "ptop_out_8_valid") == 0 ){
    return(is_valid(self,              ptop_out_8_valid));
  } else if( strcmp(valid_table_name, "ptop_8_valid")     == 0 ){
    return(is_valid(self,              ptop_8_valid));
  } else if( strcmp(valid_table_name, "pref_8_valid")     == 0 ){
    return(is_valid(self,              pref_8_valid));
  } else if( strcmp(valid_table_name, "rcoef1_valid")     == 0 ){
    return(is_valid(self,              rcoef1_valid));
  } else if( strcmp(valid_table_name, "rcoef2_valid")     == 0 ){
    return(is_valid(self,              rcoef2_valid));
  } else if( strcmp(valid_table_name, "a_m_8_valid")      == 0 ){
    return(is_valid(self,              a_m_8_valid));
  } else if( strcmp(valid_table_name, "b_m_8_valid")      == 0 ){
    return(is_valid(self,              b_m_8_valid));
  } else if( strcmp(valid_table_name, "a_t_8_valid")      == 0 ){
    return(is_valid(self,              a_t_8_valid));
  } else if( strcmp(valid_table_name, "a_t_8_valid_get")  == 0 ){
    return(is_valid(self,              a_t_8_valid_get));
  } else if( strcmp(valid_table_name, "b_t_8_valid")      == 0 ){
    return(is_valid(self,              b_t_8_valid));
  } else if( strcmp(valid_table_name, "b_t_8_valid_get")  == 0 ){
    return(is_valid(self,              b_t_8_valid_get));
  } else if( strcmp(valid_table_name, "ip1_m_valid")      == 0 ){
    return(is_valid(self,              ip1_m_valid));
  } else if( strcmp(valid_table_name, "ip1_t_valid")      == 0 ){
    return(is_valid(self,              ip1_t_valid));
  } else if( strcmp(valid_table_name, "ip1_t_valid_get")  == 0 ){
    return(is_valid(self,              ip1_t_valid_get));
  } else if( strcmp(valid_table_name, "ref_name_valid")   == 0 ){
    return(is_valid(self,              ref_name_valid));
  } else if( strcmp(valid_table_name, "dhm_valid")        == 0 ){
    return(is_valid(self,              dhm_valid));
  } else if( strcmp(valid_table_name, "dht_valid")        == 0 ){
    return(is_valid(self,              dht_valid));
  } else {
    printf("Warning : in C_is_valid, valid_table_name '%s' does not exist\n",valid_table_name);
    return(0);
  }
}

int is_required_double(TVGrid *self, double *ptr, int *table_valid, char *message) {
  if( is_valid(self,table_valid)) {
    if (! ptr) {
      printf("ERROR (vgd): %s is a required constructor entry\n", message);
      return(0);
    }
  } else {
    if (ptr) {
      printf("ERROR (vgd): %s is not a required constructor entry\n", message);
      return(0);
    }
  }
  return(1);
}
int is_required_float(TVGrid *self, float *ptr, int *table_valid, char *message) {
  if( is_valid(self,table_valid)) {
    if (! ptr) {
      printf("ERROR (vgd): %s is a required constructor entry\n", message);
      return(0);
    }
  } else {
    if (ptr) {
      printf("ERROR (vgd): %s is not a required constructor entry\n", message);
      return(0);
    }
  }
  return(1);
}

double c_get_error(char *key) {
  printf("ERROR: Attempt to retrieve invalid key %s\n",key);
  return(VGD_MISSING);
}

void C_table_shape(TVGrid *self, int **tshape) {
  (*tshape)[0] = self->table_ni;
  (*tshape)[1] = self->table_nj;
  (*tshape)[2] = self->table_nk;
}

void my_copy_double(double *aa, double **bb, int ind){
  while (ind--) {
    (*bb)[ind] = aa[ind];
  }
}

void my_copy_int(int *aa, int **bb, int ind){
  while (ind--) {
    (*bb)[ind] = aa[ind];
  }
}

int same_vec_i(int *vec1, int n1, int *vec2, int n2) {
  int i;
  if(vec1) {
    if (vec2) {
      if ( n1 == n2 ) {
	for(i = 0; i < n1; i++) {
	  if ( vec1[i] != vec2[i] ) return(-1);
	}
      } else {
	// Vectors are not the same size.
	return(-2);
      }
    } else {
      // vec2 not allocated
      return(-3);
    }
  }
  // Vector are the same or are not allocated.
  return(0);
}

int c_convip_Level2IP(float level, int kind) {

  int    mode=2,flag=0, IP; 
  char   format; 
  
  // Convertir niveau reel en ip1a
  f77name(convip)(&IP,&level,&kind,&mode,&format,&flag);
    
  return(IP);
}

int c_convip_Level2IP_old_style(float level, int kind) {

  int    mode=3,flag=0, IP; 
  char   format; 
  
  // Convertir niveau reel en ip1a
  f77name(convip)(&IP,&level,&kind,&mode,&format,&flag);
    
  return(IP);
}

float c_convip_IP2Level(int IP,int *kind) {

   int    mode=-1,flag=0;
   float  level=0.0;
   char   format;

   /*Convertir en niveau reel*/
    f77name(convip)(&IP,&level,kind,&mode,&format,&flag);

   return(level);
}

int my_fstprm(int key,TFSTD_ext *ff) {
  int fstprm;
  //var->ip1 = 62;
  STR_INIT(ff->typvar,MAXSTR_TYPVAR);
  STR_INIT(ff->nomvar,MAXSTR_NOMVAR);
  STR_INIT(ff->etiket,MAXSTR_ETIKET);
  STR_INIT(ff->grtyp, MAXSTR_GRTYP);
 
  if( c_fstprm(key,
	       &ff->dateo,  &ff->deet,   &ff->npas, 
	       &ff->ni,     &ff->nj,     &ff->nk,
	       &ff->nbits,  &ff->datyp,  
	       &ff->ip1,    &ff->ip2,    &ff->ip3,
	        ff->typvar,  ff->nomvar,  ff->etiket,
	        ff->grtyp,  &ff->ig1,    &ff->ig2,    &ff->ig3, &ff->ig4,
	       &ff->swa,    &ff->lng,    &ff->dltf,   &ff->ubc,
	       &ff->extra1, &ff->extra2, &ff->extra3) < 0 ) {
    printf("(vgd) cannot fstprm for fstkey %d\n",key);
    return(VGD_ERROR);
  }
  return(VGD_OK);
}

int correct_kind_and_version(int key, int kind, int version, TFSTD_ext *var, int *status) {
  
  *status=0;
  if( my_fstprm(key, var) == VGD_ERROR ) {
    printf("(vgd) ERROR: in correct_kind_and_version, with my_fstprm on key %d\n",key);
    return(VGD_ERROR);
  }
  if(kind != -1 && version != -1) {
    if(var->ig1 != kind*1000 + version ) {
      return(VGD_OK);
    }
  } else {
    if(kind != -1) {
      // Get kind from fst vcode (ig1)
      if(round( (float) var->ig1 / 1000.) != kind) {
	return(VGD_OK);
      }
    }
    if(version != -1) {
      // Get version from fst vcode (ig1)
      if(var->ig1-round( (float) var->ig1 / 1000.) != version) {
	return(VGD_OK);
      }
    }
  }
  // If we reach this point, we have a match
  *status = 1;
  return(VGD_OK);

}

int load_toctoc(TVGrid *self, TFSTD_ext var, int key) {

  int table_size, istat, ni, nj, nk;

  self->table_ni = var.ni;
  self->table_nj = var.nj;
  self->table_nk = var.nk;

  table_size = self->table_ni * self->table_nj * self->table_nk;
  self->table = malloc ( table_size * sizeof(double) );
  if(! self->table ) {
    printf("In load_toctoc: ERROR allocating table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  istat = c_fstluk(self->table, key, &ni, &nj, &nk);
  if(istat < 0) {
    printf("(vgd) ERROR in load_toctoc problem with fstluk\n");
    free(self->table);
    return(VGD_ERROR);
  }
  if(fstd_init(self) == VGD_ERROR) {
    printf("(vgd) in load_toctoc, problem creating record information");
  }
  self->rec.dateo        = var.dateo;
  self->rec.deet         = var.deet;
  self->rec.npas         = var.npas;
  self->rec.nbits        = var.nbits;
  self->rec.datyp        = var.datyp;
  self->rec.ip1          = var.ip1;
  self->rec.ip2          = var.ip2;
  self->rec.ip3          = var.ip3;
  strcpy(self->rec.typvar, var.typvar);
  strcpy(self->rec.nomvar, var.nomvar);
  strcpy(self->rec.etiket, var.etiket);
  strcpy(self->rec.grtyp,  var.grtyp);
  self->rec.ig1          = var.ig1;
  self->rec.ig2          = var.ig2;
  self->rec.ig3          = var.ig3;
  self->rec.ig4          = var.ig4;

  return(VGD_OK);
}

int vgdcmp(TVGrid vgd1, TVGrid vgd2) {

  // Check each element of the structure (except FST attributes) for equality
  if (vgd1.vcode != vgd2.vcode)                   return(-1);
  if (vgd1.kind != vgd2.kind)                     return(-2);
  if (vgd1.version != vgd2.version)               return(-3);
  if (strcmp(vgd1.ref_name, vgd2.ref_name) != 0 ) return(-4);
  if (vgd1.ptop_8 != vgd2.ptop_8)                 return(-5);
  if (vgd1.pref_8 != vgd2.pref_8)                 return(-6);
  if (vgd1.rcoef1 != vgd2.rcoef1)                 return(-7);
  if (vgd1.rcoef2 != vgd2.rcoef2)                 return(-8);

   // Check pointer associations and values
  if(same_vec_i(vgd1.ip1_m, vgd1.nl_m, vgd2.ip1_m, vgd2.nl_m) != 0) return (-9);
   /* if (.not.same_vec(vgd1.ip1_t,vgd2.ip1_t)) return */
   /* if (.not.same_vec(vgd1.a_m_8,vgd2.a_m_8)) return */
   /* if (.not.same_vec(vgd1.b_m_8,vgd2.b_m_8)) return */
   /* if (.not.same_vec(vgd1.a_t_8,vgd2.a_t_8)) return */
   /* if (.not.same_vec(vgd1.b_t_8,vgd2.b_t_8)) return */
   /* if (.not.same_vec(vgd1.table,vgd2.table)) return */

  return(0);
}

double comp_diag_a_height(double pref_8, float height) {
  float RGASD       =    0.287050000000E+03;
  float GRAV        =    0.980616000000E+01;
  float TCDK        =    0.273150000000E+03;
  return log(pref_8) - GRAV*height/(RGASD*TCDK);
}

/*----------------------------------------------------------------------------
 * Nom      : <VDG_FindIp1Idx>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Trouver l'index d'un ip1 dans une liste d'ip1
 *
 * Parametres :
 *  <Ip1>   : Paramètres de l'application
 *  <Lst>   : La référence verticale
 *  <Size>  : Header RPN
 *
 * Retour   : L'index de l'ip1 dans la liste ou -1 si pas trouvé
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int VGD_FindIp1Idx(int Ip1,int *Lst,int Size) {
   int idx=0;

   while( Size-- ) {
      if( *Lst++ == Ip1 )
         return idx;
      ++idx;
   }

   return(-1);
}

/*----------------------------------------------------------------------------
 * Nom      : <MemInit>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Initialise un array de double à une valeur donnée
 *
 * Parametres :
 *  <Arr>   : L'array à initialiser
 *  <Val>   : La valeur à laquelle initialiser l'array
 *  <Size>  : La taille de l'array
 *
 * Retour   : 
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
void VGD_MemInit(double *Arr,double Val,int Size) {
   while( Size-- )
      *Arr++ = Val;
}

int C_print_desc(TVGrid *self, int *sout, int *convip) {
  int k, ip1, kind;
  if(! self ) {
    printf("In C_print_desc: vgrid structure not constructed\n");
    return(VGD_ERROR);
  } else {
    if(! self->valid) {
      printf("In C_print_desc: vgrid structure is not valid\n");
      return(VGD_ERROR);
    }
    if(sout && *sout != 6){
      printf("In C_print_desc : please implement stdout option %d\n",*sout);
      return(VGD_ERROR);
    }
    if(convip){
      //TODO
      printf("In C_print_desc : please implement convip option %d in C_print_desc\n",*sout);
      return(VGD_ERROR);
    }
    
    // Create horizontal rule
    char *hr = {"-------------------------------------------------------"};

    // Dump general descriptor information
    printf("-- Vertical Grid Descriptor Information --\n");
    //printf("  ip1=%d",(self->rec)->ip1);
    //printf("  ip2=',self%rec%ip2
    //printf("',trim(hr)
    printf("Vcode=%d\n",self->vcode);
    
    //printf("  Descriptor Nomvar: %s\n",trim(self%rec%nomvar)
    printf("  level kind =%2d, level version = %3d\n", self->kind ,self->version);
    if( is_valid(self, ptop_8_valid) )
      printf("  ptop=%f Pa\n",self->ptop_8);
    if( is_valid(self, pref_8_valid) )
      printf("  pref=%f Pa\n",self->pref_8);
    if( is_valid(self, rcoef1_valid) )
      printf("  rcoef1=%f\n",self->rcoef1);
    if( is_valid(self, rcoef2_valid) )
      printf("  rcoef2=%f\n",self->rcoef2);
    if( is_valid(self,ref_name_valid) )
      printf("  Surface field nomvar %s\n",self->ref_name);
    
    switch(self->vcode) {
    case 1001:
      printf("  Number of sigma levels %d\n",self->nk);
      printf("  Equation to compute hydrostatic pressure (pi): pi = B * P0*100\n");
      break;
    case 1002:
      printf("C_print_desc 1002 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 2001:
      printf("C_print_desc 2001 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 1003:
      printf("C_print_desc 1003 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 5001:
      printf("C_print_desc 5001 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 5002:
      printf("C_print_desc 5002 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 5003:
      printf("C_print_desc 5003 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 5004:
      printf("C_print_desc 5004 TODO!!!!!!!!! \n");
      return(VGD_ERROR);
      break;
    case 5005:
       printf("  Number of hybrid levels (momentum/thermo levels) %d\n", self->nl_m );
      ip1=self->ip1_m[self->nl_m-1];
      printf("  Diagnostic momentum level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      ip1=self->ip1_t[self->nl_t-1];
      printf("  Diagnostic thermo   level (ip1=%d) at %f m Above Ground Level\n",ip1,c_convip_IP2Level(ip1,&kind));
      printf("  Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)\n");
      break;
    default:
      printf("Invalid kind or version in fstd_init: kind=%d, version=%d\n",self->kind,self->version);
      return(VGD_ERROR);
    }

    if(convip){
      printf("C_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR); 
    }
     
    // TODO add format
    
    if (is_valid(self, ip1_m_valid) ) {
      printf("  Momentum levels ip1, A, B:\n");
      for ( k = 0; k < self->nl_m; k++) {
	printf("%d %f %f\n",self->ip1_m[k],self->a_m_8[k],self->b_m_8[k]);
      }
    }
    if (is_valid(self, ip1_t_valid) ) {
      printf("  Thermodynamic levels ip1, A, B:\n");
      for ( k = 0; k < self->nl_t; k++) {
	printf("%d %f %f\n",self->ip1_t[k],self->a_t_8[k],self->b_t_8[k]);
      }
    }
    
    return(VGD_OK);
  }
}

/*----------------------------------------------------------------------------
 * Nom      : <c_vgd_construct>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Initialise et retourne une structure de type TVGrid
 *
 * Parametres :
 *
 * Retour   : Une structure initialisée de type TVGrid
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
TVGrid* c_vgd_construct() {

   TVGrid *vgrid = malloc(sizeof(TVGrid));

   if( vgrid ) {
      vgrid->ptop_8        = VGD_MISSING;
      vgrid->pref_8        = VGD_MISSING;      
      vgrid->table         = NULL;
      vgrid->table_ni      = 0;
      vgrid->table_nj      = 0;
      vgrid->table_nk      = 0;
      vgrid->a_m_8         = NULL;
      vgrid->b_m_8         = NULL;
      vgrid->a_t_8         = NULL;
      vgrid->b_t_8         = NULL;
      vgrid->ip1_m         = NULL;
      vgrid->ip1_t         = NULL;      
      vgrid->nl_m          = 0;
      vgrid->nl_t          = 0;
      vgrid->dhm           = VGD_MISSING;
      vgrid->dht           = VGD_MISSING;
      vgrid->ref_name      = strdup("None");
      vgrid->rcoef1        = VGD_MISSING;
      vgrid->rcoef2        = VGD_MISSING;
      vgrid->nk            = 0;
      vgrid->ip1           = 0;
      vgrid->ip2           = 0;
      vgrid->unit          = 0;
      vgrid->vcode         = 0;
      vgrid->kind          = 0;
      vgrid->version       = 0;
      vgrid->match_ipig    = 0;
      vgrid->valid         = 0;

      vgrid->rec.fstd_initialized = 0;
      strcpy(vgrid->rec.etiket,"            ");
      strcpy(vgrid->rec.nomvar,"    ");
      strcpy(vgrid->rec.typvar,"  ");
   }

   return(vgrid);
}

/*----------------------------------------------------------------------------
 * Nom      : <VGD_Free>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Libère la mémoire d'une structure de type TVGrid
 *
 * Parametres :
 *    <VGrid>  : Structure dont il faut libérer la mémoire
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
void C_vgd_free(TVGrid **VGrid) {
   if( *VGrid ) {

      FREE((*VGrid)->table);
      FREE((*VGrid)->a_m_8);
      FREE((*VGrid)->b_m_8);
      FREE((*VGrid)->a_t_8);
      FREE((*VGrid)->b_t_8);
      FREE((*VGrid)->ip1_m);
      FREE((*VGrid)->ip1_t);
      FREE((*VGrid)->ref_name);

      free(*VGrid);
      *VGrid = NULL;

   }
}

/*----------------------------------------------------------------------------
 * Nom      : <get_version_info>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Retrieve kind and version information from the table
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *    <Kind>   : Kind of the vertical coord
 *    <Version>: Version of the vertical coord
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int get_version_info(TVGrid *VGrid,int *Kind,int *Version) {
   *Kind    = (int)round(VGrid->table[0]);
   *Version = (int)round(VGrid->table[1]);

   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <C_set_vcode_i>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Set and check the vertical code
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *    <Kind>   : Kind of the vertical coord
 *    <Version>: Version of the vertical coord
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int C_set_vcode_i(TVGrid *VGrid,int Kind,int Version) {

   if( Kind>MAX_VKIND || Kind<0 || Version>999 || Version<0 ) {
      fprintf(stderr,"Invalid kind or version in C_set_vcode_i: kind=%d, version=%d\n",Kind,Version);
      return(VGD_ERROR);
   }
   //printf("dans C_set_vcode_i, Kind = %d, Version = %d\n", Kind, Version);
   VGrid->vcode = Kind*1000 + Version;
   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <C_set_vcode>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Set and check the vertical code
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int C_set_vcode(TVGrid *VGrid) {
   int err,kind,version;

   if( !VGrid->table ) {
      fprintf(stderr,"C_set_vcode called before constructor\n");
      return(VGD_ERROR);
   }

   if( (err=get_version_info(VGrid,&kind,&version)) != VGD_OK ) {
      fprintf(stderr,"Cannot decode table to read kind and version\n");
      return(err);
   }

   return C_set_vcode_i(VGrid,kind,version);
}

/*----------------------------------------------------------------------------
 * Nom      : <fstd_init>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Initialize common elements of the fstd record
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int fstd_init(TVGrid *VGrid) {
   TFSTD *h = &VGrid->rec;
   int err;

   if( h->fstd_initialized )
      return(VGD_OK);

   h->ig2=h->ig3=h->ig4=0;

   err=C_set_vcode(VGrid);

   switch(VGrid->vcode) {
      case 1001:
         strcpy(h->etiket,"ETA_GEMV3");
         break;
      case 1002:
         strcpy(h->etiket,"ETA_GEMV3");
         h->ig2=(int)round(VGrid->ptop_8*10.0);
         break;
      case 2001:
         strcpy(h->etiket,"PRESSURE");
         break;
      case 1003:
         strcpy(h->etiket,"HYBNORM_GEM3");
         h->ig2=(int)round(VGrid->ptop_8*10.0);
         h->ig3=(int)roundf(VGrid->rcoef1*100.0f);
         break;
      case 5001:
         strcpy(h->etiket,"HYB_GEMV3");
         h->ig2=(int)round(VGrid->ptop_8*10.0);
         h->ig3=(int)roundf(VGrid->rcoef1*100.0f);
         break;
      case 5002:
      case 5003:
      case 5004:
         strcpy(h->etiket,"STG_CP_GEMV4");
         h->ig2=(int)round(VGrid->ptop_8*10.0);
         h->ig3=(int)roundf(VGrid->rcoef1*100.0f);
         h->ig4=(int)roundf(VGrid->rcoef2*100.0f);
         break;
      default:
         fprintf(stderr,"Invalid kind or version in fstd_init: kind=%d, version=%d\n",VGrid->kind,VGrid->version);
         return(VGD_ERROR);
   }

   strcpy(h->nomvar,"!!");
   strcpy(h->typvar,"X");
   strcpy(h->grtyp,"X");

   h->dateo       = 0;
   h->deet        = 0;
   h->npas        = 0;
   h->datyp       = 5;
   h->nbits       = 64;
   h->ip3         = 0;
   h->ig1         = VGrid->vcode;
   h->fstd_initialized = 1;

   return(VGD_OK);
}

// Note the added 'NbLevels' parameter because 'size(Levels)' is not available in C ('sizeof(Levels)' would return the size of the 'Levels' pointer in bytes (aka 8 bytes on a x86_64 system))
// Also note the added 'NIJ' required to correctly set 'Levels'.
int compute_pressure_1001_8(TVGrid* VGrid,double *SfcField,int *Ip1List,double *Levels,int NbLevels,int NIJ,char InLog) {
   int k,*idxs,ij,ijk;
   double lvl;

   // Is this temporary table really necessary? (Can Levels be modified halfway trough before an error is encountered or is data integrity the goal here?)
   idxs = malloc(NbLevels*sizeof(int));
   if( !idxs ) {
      fprintf(stderr,"Could not allocate memory for idxs in compute_pressure_1001_8\n");
      return(VGD_ERROR);
   }

   // Find ip1 indexes
   for(k=0; k<NbLevels; ++k) {
      if( (idxs[k]=VGD_FindIp1Idx(Ip1List[k],VGrid->ip1_m,VGrid->nk)) == -1 ) {
         fprintf(stderr,"Cannot find ip1 %d in compute_pressure_1001_8\n",Ip1List[k]);
         free(idxs);
         return(VGD_ERROR);
      }
   }

   // Compute pressure
   for(k=0,ijk=0; k<NbLevels; ++k) {
      for(ij=0; ij<NIJ; ++ij,++ijk) {
         lvl = VGrid->a_m_8[idxs[k]] + VGrid->b_m_8[idxs[k]]*SfcField[ij];
         Levels[ijk] = InLog ? log(lvl) : lvl;
      }
   }

   free(idxs);
   return(VGD_OK);
}

int C_new_build_vert(TVGrid **self, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, 
		     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t)
{

  char cvcode[4];
  int errorInput = 0;
  
  // Check if self is constructed
  if(! *self){
    *self = c_vgd_construct();
    if(! *self){
      printf("ERROR (vgd): in C_new_build_vert, problem with c_vgd_construct");
      return (VGD_ERROR);
    }
  }    
  //TODO ajout tests if self deja valid

  // Initializations
  (*self)->valid      = 1;
  (*self)->kind       = kind;
  (*self)->version    = version;
  (*self)->unit       = -1;
  (*self)->match_ipig = 1;
  (*self)->nk         = nk;
  (*self)->nl_m       = nl_m;
  (*self)->nl_t       = nl_t;
  (*self)->rec.ip1    = ip1;
  (*self)->rec.ip2    = ip2;

  if(C_set_vcode_i(*self, kind, version) == VGD_ERROR)  {
    printf("ERROR (vgd): in C_new_build_vert, problem with C_set_vcode_i");
    return (VGD_ERROR);
  }
  // Check for required inputs
  int missingInput = 0;
  if( is_valid( *self, ptop_8_valid) ) {
    if(ptop_8) {
      (*self)->ptop_8 = *ptop_8;
    } else {
      printf("ptop_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, pref_8_valid)) {
    if(pref_8){
      (*self)->pref_8 = *pref_8;
    } else {
      printf("pref_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, rcoef1_valid)) {
    if(rcoef1){
      (*self)->rcoef1 = *rcoef1;
    } else {
      printf("rcoef1 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, rcoef2_valid)) {
    if(rcoef2){
      (*self)->rcoef2 = *rcoef2;
    } else {
      printf("rcoef2 is a required constructor entry\n");
      missingInput = 1;
    }
  }

  if(is_valid( *self, a_m_8_valid)) {
    if(a_m_8){
      free((*self)->a_m_8);
      (*self)->a_m_8 = malloc( nl_m * sizeof(double) );
      if(! (*self)->a_m_8){ 
	printf("ERROR (vgd): in C_new_build_vert, problem allocating a_m_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_m_8, &((*self)->a_m_8), nl_m);
    } else {
      printf("a_m_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(is_valid( *self, b_m_8_valid)) {
    if(b_m_8){
      free((*self)->b_m_8);
      (*self)->b_m_8 = malloc( nl_m * sizeof(double) );
      if(! (*self)->b_m_8) {
	printf("ERROR (vgd): in C_new_build_vert, problem allocating b_m_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_m_8, &((*self)->b_m_8), nl_m);
    } else {
      printf("b_m_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(is_valid( *self, a_t_8_valid)) {
    if(a_t_8){
      free((*self)->a_t_8);
      (*self)->a_t_8 = malloc( nl_t * sizeof(double) );
      if(! (*self)->a_t_8) {
	printf("ERROR (vgd): in C_new_build_vert, problem allocating a_t_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_t_8, &((*self)->a_t_8), nl_t);
    } else {
      printf("a_t_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(is_valid( *self, b_t_8_valid)) {
    if(b_t_8){
      free((*self)->b_t_8);
      (*self)->b_t_8 = malloc( nl_t * sizeof(double) );
      if(! (*self)->b_t_8) {
	printf("ERROR (vgd): in C_new_build_vert, problem allocating b_t_8\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_t_8, &((*self)->b_t_8), nl_t);
    } else {
      printf("b_t_8 is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(is_valid( *self, ip1_m_valid)) {
    if(ip1_m){
      free((*self)->ip1_m);
      (*self)->ip1_m = malloc( nl_m * sizeof(double) );
      if(! (*self)->ip1_m) {
	printf("problem allocating ip1_m in C_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_m, &((*self)->ip1_m), nl_m);
    } else {
      printf("ip1_m is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if(is_valid( *self, ip1_t_valid)) {
    if(ip1_t){
      free((*self)->ip1_t);
      (*self)->ip1_t = malloc( nl_t * sizeof(double) );
      if(! (*self)->ip1_t) {
	printf("ERROR (vgd): in C_new_build_vert, problem allocating ip1_t\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_t, &((*self)->ip1_t), nl_t);
    } else {
      printf("ip1_t is a required constructor entry\n");
      errorInput = 1;
    }
  }
  if (errorInput > 0) {
    return (VGD_ERROR);
  }
  int ier;
  // Fill table with version-specific encoder
  switch((*self)->vcode) {
  case 1001:
    strcpy(cvcode,"1001");
    ier = c_encode_vert_1001(self,nk);
    break;
  case 1002:
    strcpy(cvcode,"1002");
    ier = c_encode_vert_1002(self,nk);    
    break;
  case 2001:
    ier = c_encode_vert_2001(self,nk);
    break;
  case 1003:
    break;
  case 5001:
    strcpy(cvcode,"5001");
    ier = c_encode_vert_5001(self,nk);
    break;
  case 5002:
    strcpy(cvcode,"5002");
    ier = c_encode_vert_5002_5003_5004_5005(self);
    break;
  case 5003:
    strcpy(cvcode,"5003");
    ier = c_encode_vert_5002_5003_5004_5005(self);
    break;
  case 5004:
    strcpy(cvcode,"5004");
    ier = c_encode_vert_5002_5003_5004_5005(self);
    break;
  case 5005:
    strcpy(cvcode,"5005");
    ier = c_encode_vert_5002_5003_5004_5005(self);
    break;
  default:
    fprintf(stderr,"IN C_new_build_vert: invalid kind or version : kind=%d, version=%d\n",kind,version);
    return(VGD_ERROR);
  }

  if(ier == VGD_ERROR) {
    printf("ERROR (vgd): in C_new_build_vert, problem with encode_vert_%s\n",cvcode);
    free(cvcode);
    return(VGD_ERROR);
  }

  return(VGD_OK);

}

int c_encode_vert_1001(TVGrid **self,int nk){
  
  int skip = 2, table_size;

  free( (*self)->table );
  (*self)->table_ni = 3;
  (*self)->table_nj = nk+skip;
  (*self)->table_nk = 1;
  table_size = (*self)->table_ni * (*self)->table_nj * (*self)->table_nk;
  (*self)->table = malloc ( table_size * sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_1001: ERROR allocating table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"P0");

  //TODO : voir Ron pour error = flip_transfer(self%ref_name,for_char_8)
  //Fill header
  (*self)->table[0] = (*self)->kind;
  (*self)->table[1] = (*self)->version;
  (*self)->table[2] = skip;
  //(*self)->table[3] = for_char_8;
  (*self)->table[3] = 0.;
  (*self)->table[4] = 0.;
  (*self)->table[5] = 0.;
  
  int k, ind = 6;
  for ( k = 0; k < nk; k++){
    (*self)->table[ind  ] = (*self)->ip1_m[k];
    (*self)->table[ind+1] = (*self)->a_m_8[k];
    (*self)->table[ind+2] = (*self)->b_m_8[k];
    ind = ind + 3;
  }

  (*self)->valid = 1;

  return(VGD_OK);
}

int c_encode_vert_1002(TVGrid **self,int nk){
  
  int skip = 2, table_size;
  
  free( (*self)->table );
  (*self)->table_ni = 3;
  (*self)->table_nj = nk+skip;
  (*self)->table_nk = 1;
  table_size = (*self)->table_ni * (*self)->table_nj * (*self)->table_nk;
  (*self)->table = malloc ( table_size * sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_1002: ERROR allocating table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"P0");

  //TODO : voir Ron pour error = flip_transfer(self%ref_name,for_char_8)
  //Fill header
  (*self)->table[0] = (*self)->kind;
  (*self)->table[1] = (*self)->version;
  (*self)->table[2] = skip;
  //(*self)->table[3] = for_char_8;
  (*self)->table[3] = 0.;
  (*self)->table[4] = 0.;
  (*self)->table[5] = 0.;
  
  int k, ind = 6;
  for ( k = 0; k < nk; k++){
    (*self)->table[ind  ] = (*self)->ip1_m[k];
    (*self)->table[ind+1] = (*self)->a_m_8[k];
    (*self)->table[ind+2] = (*self)->b_m_8[k];
    ind = ind + 3;
  }

  (*self)->valid = 1;

  return(VGD_OK);
}

int c_encode_vert_2001(TVGrid **self,int nk){
  
  int skip = 1, table_size;
  
  free( (*self)->table );
  (*self)->table_ni = 3;
  (*self)->table_nj = nk+skip;
  (*self)->table_nk = 1;
  table_size = (*self)->table_ni * (*self)->table_nj * (*self)->table_nk;
  (*self)->table = malloc ( table_size * sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_2001: ERROR allocating table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"");

  //TODO : voir Ron pour error = flip_transfer(self%ref_name,for_char_8)
  //Fill header
  (*self)->table[0] = (*self)->kind;
  (*self)->table[1] = (*self)->version;
  (*self)->table[2] = skip;
  
  int k, ind = 3;
  for ( k = 0; k < nk; k++){
    (*self)->table[ind  ] = (*self)->ip1_m[k];
    (*self)->table[ind+1] = (*self)->a_m_8[k];
    (*self)->table[ind+2] = (*self)->b_m_8[k];
    ind = ind + 3;
  }

  (*self)->valid = 1;

  return(VGD_OK);
}

int c_encode_vert_5001(TVGrid **self,int nk){
  int skip = 3, table_size;

  free( (*self)->table );
  (*self)->table_ni = 3;
  (*self)->table_nj = nk+skip;
  (*self)->table_nk = 1;
  table_size = (*self)->table_ni * (*self)->table_nj * (*self)->table_nk;
  (*self)->table = malloc ( table_size * sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_5001: ERROR allocating table of bouble of size %d\n",table_size );
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"P0");

  //TODO : voir Ron pour error = flip_transfer(self%ref_name,for_char_8)
  //Fill header
  (*self)->table[0] = (*self)->kind;
  (*self)->table[1] = (*self)->version;
  (*self)->table[2] = skip;

  (*self)->table[3] = (*self)->ptop_8;
  (*self)->table[4] = (*self)->pref_8;
  (*self)->table[5] = (*self)->rcoef1;
  
  //(*self)->table[6] = for_char_8;
  (*self)->table[6] = 0.;
  (*self)->table[7] = 0.;
  (*self)->table[8] = 0.;

  int k, ind = 9;
  for ( k = 0; k < nk; k++){
    (*self)->table[ind  ] = (*self)->ip1_m[k];
    (*self)->table[ind+1] = (*self)->a_m_8[k];
    (*self)->table[ind+2] = (*self)->b_m_8[k];
    ind = ind + 3;
  }

  (*self)->valid = 1;

  return(VGD_OK);
}

int c_encode_vert_5002_5003_5004_5005(TVGrid **self){

  //printf("c_encode_vert_5002_5003_5004_5005, (*self)->nk = %d, (*self)->nl_m = %d, (*self)->nl_t = %d\n",(*self)->nk, (*self)->nl_m, (*self)->nl_t);

  int skip = 3, table_size;
  free( (*self)->table );
  (*self)->table_ni = 3;
  (*self)->table_nj = (*self)->nl_m + (*self)->nl_t + skip;
  (*self)->table_nk = 1;
  table_size = (*self)->table_ni * (*self)->table_nj * (*self)->table_nk;
  (*self)->table = malloc ( table_size * sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_5002_5003_5004_5005: ERROR allocating table of bouble of size %d\n", table_size);
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"P0");

  //TODO : voir Ron pour error = flip_transfer(self%ref_name,for_char_8)
  //Fill header
  (*self)->table[0] = (*self)->kind;
  (*self)->table[1] = (*self)->version;
  (*self)->table[2] = skip;
  (*self)->table[3] = (*self)->ptop_8;
  (*self)->table[4] = (*self)->pref_8;
  (*self)->table[5] = (*self)->rcoef1;  
  (*self)->table[6] = (*self)->rcoef2;
  //(*self)->table[7] = for_char_8;
  (*self)->table[7] = 0.;
  (*self)->table[8] = 0.;

  int k, ind = 9;
  for ( k = 0; k < (*self)->nl_m; k++){
    (*self)->table[ind  ] = (*self)->ip1_m[k];
    (*self)->table[ind+1] = (*self)->a_m_8[k];
    (*self)->table[ind+2] = (*self)->b_m_8[k];
    ind = ind + 3;
  }
  for ( k = 0; k < (*self)->nl_t; k++){
    (*self)->table[ind  ] = (*self)->ip1_t[k];
    (*self)->table[ind+1] = (*self)->a_t_8[k];
    (*self)->table[ind+2] = (*self)->b_t_8[k];
    ind = ind + 3;
  }

  (*self)->valid = 1;

  return(VGD_OK);
}

int c_vgrid_genab_1001(float *hyb, int nk, float **hybm, double **a_m_8, double **b_m_8, int **ip1_m)
{

  // Andre Plante May 2015. 
  char ok = 1;
  int k,ip1, kind2, kind = 1;
  
  *hybm = malloc( nk*sizeof(float) );
  if(! *hybm){
    printf("\tIn c_vgrid_genab_1001, malloc error with *hybm\n");
    return(VGD_ERROR);
  }

  *a_m_8 = malloc( nk*sizeof(double) );
  if(! *a_m_8){
    printf("\tIn c_vgrid_genab_1001, malloc error with *a_m_8\n");
    return(VGD_ERROR);
  }

  *b_m_8 = malloc( nk*sizeof(double) );
  if(! *b_m_8){
    printf("\tIn c_vgrid_genab_1001, malloc error with *b_m_8\n");
    return(VGD_ERROR);
  }

  *ip1_m = malloc( nk*sizeof(int) );
  if(! *ip1_m){
    printf("\tIn c_vgrid_genab_1001, malloc error with *ip1_m\n");
    return(VGD_ERROR);
  }
  
  if(hyb[nk-1] != 1.){
    printf("WRONG SPECIFICATION OF SIGMA VERTICAL LEVELS: SIGMA(NK) MUST BE 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hyb[k] <= hyb[k-1]){
      printf("WRONG SPECIFICATION OF SIGMA VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hyb[k]);
    }
    return(VGD_ERROR);
  }

  for ( k = 0; k < nk; k++){
    (*a_m_8)[k]=0.;
    // Go back and forth to ip1 in order to make sure hyb value is encodable.
    ip1 = c_convip_Level2IP(hyb[k],1);
    (*b_m_8)[k] = (double) c_convip_IP2Level(ip1,&kind2);
    (*ip1_m)[k] = ip1;
  }

  return(VGD_OK);
  
}

int c_vgrid_genab_1002(float *etauser, int nk, double *ptop_8, double **a_m_8, double **b_m_8, int **ip1_m)
{
  // Andre Plante May 2015.   
  char ok=1;
  int k;

  *a_m_8 = malloc( nk*sizeof(double) );
  if(! *a_m_8){
    printf("\tIn c_vgrid_genab_1002, malloc error with *a_m_8\n");
    return(VGD_ERROR);
  }
  *b_m_8 = malloc( nk*sizeof(double) );
  if(! *b_m_8){
    printf("\tIn c_vgrid_genab_1002, malloc error with *b_m_8\n");
    return(VGD_ERROR);
  }
  *ip1_m = malloc( nk*sizeof(int) );
  if(! *ip1_m){
    printf("\tIn c_vgrid_genab_1002, malloc error with *ip1_m\n");
    return(VGD_ERROR);
  }

  if(etauser[nk-1] != 1.){
    printf("WRONG SPECIFICATION OF ETA VERTICAL LEVELS: ETA(NK) MUST BE 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(etauser[k] <= etauser[k-1]){
      printf(" WRONG SPECIFICATION OF ETA VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", etauser[k]);
    }
    return(VGD_ERROR);
  }

  if( *ptop_8 <= 0.) {
    printf("ERROR in c_vgrid_genab_1002: ptop = %f must be greater than zero\n", *ptop_8);
    return(VGD_ERROR);
  }

  int ip1, kind2;
  float eta;
  for ( k = 0; k < nk; k++){
    ip1 = c_convip_Level2IP_old_style(etauser[k],1);
    eta = c_convip_IP2Level(ip1,&kind2);
    (*ip1_m)[k] = ip1;
    (*a_m_8)[k] = (1. - eta) * (*ptop_8);
    (*b_m_8)[k] = eta;
    //printf("etauser[k] = %f, *ptop_8 = %f, ip1 = %d, A = %f, B = %f\n",etauser[k], *ptop_8, (*ip1_m)[k], (*a_m_8)[k], (*b_m_8)[k]);
  }

  return(VGD_OK);
}

int c_vgrid_genab_2001(float *pres, int nk, double **a_m_8, double **b_m_8, int **ip1_m)
{

  // Andre Plante May 2015. 
  char ok = 1;
  int k,ip1, kind2, kind = 1;
  
  *a_m_8 = malloc( nk*sizeof(double) );
  if(! *a_m_8){
    printf("\tIn c_vgrid_genab_2001, malloc error with *a_m_8\n");
    return(VGD_ERROR);
  }

  *b_m_8 = malloc( nk*sizeof(double) );
  if(! *b_m_8){
    printf("\tIn c_vgrid_genab_2001, malloc error with *b_m_8\n");
    return(VGD_ERROR);
  }

  *ip1_m = malloc( nk*sizeof(int) );
  if(! *ip1_m){
    printf("\tIn c_vgrid_genab_2001, malloc error with *ip1_m\n");
    return(VGD_ERROR);
  }
  
  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(pres[k] <= pres[k-1]){
      printf("WRONG SPECIFICATION OF PRESSURE VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", pres[k]);
    }
    return(VGD_ERROR);
  }

  for ( k = 0; k < nk; k++){
    (*a_m_8)[k] = pres[k] * 100.;
    (*b_m_8)[k] = 0.;
    // Go back and forth to ip1 in order to make sure pres value is encodable.
    (*ip1_m)[k] = c_convip_Level2IP(pres[k],2);
  }

  return(VGD_OK);
  
}

int c_vgrid_genab_5001(float *hybuser, int nk, float rcoef, double ptop_8, double pref_8, double **a_m_8, double **b_m_8, int **ip1_m)
{
  // Andre Plante May 2015. 
  char ok = 1;
  int k;
  int complet, ip1, kind2;
  float epsilon=1.0e-6;
  double hybtop = ptop_8 / pref_8;
  double hyb, pr1;
   
  *a_m_8 = malloc( nk*sizeof(double) );
  if(! *a_m_8){
    printf("\tIn c_vgrid_genab_5001, malloc error with *a_m_8\n");
    return(VGD_ERROR);
  }
  *b_m_8 = malloc( nk*sizeof(double) );
  if(! *b_m_8){
    printf("\tIn c_vgrid_genab_5001, malloc error with *b_m_8\n");
    return(VGD_ERROR);
  }
  *ip1_m = malloc( nk*sizeof(int) );
  if(! *ip1_m){
    printf("\tIn c_vgrid_genab_5001, malloc error with *ip1_m\n");
    return(VGD_ERROR);
  }

  if(hybuser[nk-1] != 1.){
    printf("WRONG SPECIFICATION OF HYB VERTICAL LEVELS: HYB(NK) MUST BE 1.0\n");
    ok=0;
  }
  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  if( ptop_8 <= 0.) {
    printf("ERROR in c_vgrid_genab_5001: ptop = %f must be greater than zero\n", ptop_8);
    return(VGD_ERROR);
  }

  if( ( ptop_8 - hybuser[0] * pref_8 ) / ptop_8 > epsilon ) {
    printf("ERROR in c_vgrid_genab_5001: ptop = %f is lower than first hyb level = %f\n", ptop_8, hybuser[0]*pref_8);
    return(VGD_ERROR);
  }

  ip1 = c_convip_Level2IP(hybtop,5);
  hybtop = (double) c_convip_IP2Level(ip1,&kind2);
  pr1 = 1./(1.-hybtop);
  
  // Find out if first level is at top
  if( abs( hybuser[0] - ptop_8 ) / pref_8 < epsilon) {
    complet = 1;
  } else {
    printf("NOTE: First hyb level is not at model top\n");
    complet = 0;
  }

  for ( k = 0; k < nk; k++){
    ip1 = c_convip_Level2IP(hybuser[k],5);
    hyb = c_convip_IP2Level(ip1,&kind2);
    (*ip1_m)[k] = ip1;
    (*b_m_8)[k] = pow( (hyb - hybtop) * pr1, rcoef);
    (*a_m_8)[k] = pref_8 * ( hyb - (*b_m_8)[k] );
  }
  if(complet) {
    (*b_m_8)[0] = 0.;
    (*a_m_8)[0] = ptop_8;
  }
    
  return(VGD_OK);
}

int c_vgrid_genab_5002_5003(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, int tlift)
{
  // Andre Plante May 2015.
  
  // Processing option
  if( ! ( tlift == 0 || tlift == 1 ) ){
    printf("ERROR in c_vgrid_genab_5002_5003: wrong value given to tlift, expecting 0 (for false) or 1 (for true), got %d\n",tlift);
    return(VGD_ERROR);
  }

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok;
  int k;
  float *hybm, hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1;  
  
  //printf("nk %d, (*nl_m) %d, (*nl_t) %d, rcoef1 %f, rcoef2 %f, ptop_8 %f, pref_8 %f\n",nk, (*nl_m), (*nl_t), rcoef1, rcoef2, ptop_8, pref_8);

  *nl_m = nk + 1;
  *nl_t = nk + 2;

  *PP_a_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("\tIn c_vgrid_genab_5002_5003, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);
  if ( ptop_8 <= 0. ) {
    printf("ERROR in c_vgrid_genab_5002_5003: ptop_8 must be > 0, got %f\n", ptop_8);
    return(VGD_ERROR);
  }
  ztop_8  = log(ptop_8);

  // Checking vertical layering

  //    Check range
  hybtop = ptop_8 / pref_8;
  if( hybuser[nk-1] >= 1. ) {
    printf("ERROR in c_vgrid_genab_5002_5003: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("ERROR in c_vgrid_genab_5002_5003: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
    return(VGD_ERROR);
  }

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  // Momentum levels
  pr1 = 1. / (zsrf_8 - ztop_8);
  for( k = 0; k < nk; k++ ) {
    zeta_8  = zsrf_8 + log(hybuser[k]);
    lamba_8  = ( zeta_8 - ztop_8 ) * pr1;
    rcoef  = rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8;
    b_m_8[k] = pow(lamba_8, rcoef);
    a_m_8[k] = zeta_8;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;

  // Thermodynamic levels    
  for( k = 1; k < nk; k++ ) {
    b_t_8[k] = 0.5 * ( b_m_8[k] + b_m_8[k-1] );
    a_t_8[k] = 0.5 * ( a_m_8[k] + a_m_8[k-1] );
  }
  // Special thermo levels
  b_t_8[0]    = 0.5 * ( b_m_8[0]    + 0.    );
  b_t_8[nk]   = 0.5 * ( b_m_8[nk-1] + 1.    );
  b_t_8[nk+1] = 1.;
  a_t_8[0]    = 0.5 * ( a_m_8[0]    + ztop_8);
  a_t_8[nk]   = 0.5 * ( a_m_8[nk-1] + zsrf_8);
  a_t_8[nk+1] = zsrf_8;

  if( tlift ){
    a_t_8[nk]   = a_m_8[nk-1];
    b_t_8[nk]   = b_m_8[nk-1];
  }

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);
  
  ip1_t[0]    = c_convip_Level2IP( sqrt( hybtop     * hybuser[0]   ), 5 );
  for(k = 1; k < nk; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrt( hybuser[k] * hybuser[k-1] ), 5 );
  }
  if( tlift ){
    ip1_t[nk]   = c_convip_Level2IP( hybuser[nk-1] , 5 );
  } else {
    ip1_t[nk]   = c_convip_Level2IP( sqrt( hybuser[nk-1]*1.0 ), 5 );
  }
  ip1_t[nk+1] = c_convip_Level2IP(1.,5);
  
  return(VGD_OK);

}

int c_vgrid_genab_5004(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double ptop_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t)
{
  // Andre Plante May 2015.
  
  // Processing option

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok;
  int k;
  float *hybm, hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1, zetau_8, zeta2_8, l_ptop_8;  
  
  //printf("nk %d, (*nl_m) %d, (*nl_t) %d, rcoef1 %f, rcoef2 %f, ptop_8 %f, pref_8 %f\n",nk, (*nl_m), (*nl_t), rcoef1, rcoef2, ptop_8, pref_8);

  *nl_m = nk + 1;
  *nl_t = nk + 1;

  *PP_a_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("\tIn c_vgrid_genab_5004, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);

  if ( lrint(ptop_8) == -2 || lrint(ptop_8) == -1 ) {
    // Auto compute ptop and make B(1) = 0
    zetau_8 = zsrf_8 + log(hybuser[0]);
    zeta2_8 = zsrf_8 + log(hybuser[1]);
    ztop_8  = 0.5 * ( 3. * zetau_8 - zeta2_8);
    l_ptop_8 = exp(ztop_8);
    if( lrint(ptop_8) == -1 ) {
      // Compute B(1) from ztop, B(1) != 0
      zetau_8 = ztop_8;
    }
  } else if (ptop_8 <= 0.) {
    printf("ERRO In c_vgrid_genab_5004: ptop_8 must be > 0, got %f\n",ptop_8);
    return(VGD_ERROR);
  } else {
    // Take B(1) from user's ztop
    l_ptop_8 = ptop_8;
    ztop_8  = log(ptop_8);
    zetau_8 = ztop_8;
  }

  // Checking vertical layering

  //    Check range
  hybtop = l_ptop_8 / pref_8;
  if( hybuser[nk-1] >= 1. ) {
    printf("ERROR in c_vgrid_genab_5004: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("ERROR in c_vgrid_genab_5004: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
    return(VGD_ERROR);
  }

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  // Momentum levels
  pr1 = 1. / (zsrf_8 - zetau_8);
  for( k = 0; k < nk; k++ ) {
    zeta_8  = zsrf_8 + log(hybuser[k]);
    lamba_8  = ( zeta_8 - zetau_8 ) * pr1;
    rcoef  = rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8;
    b_m_8[k] = pow(lamba_8, rcoef);
    a_m_8[k] = zeta_8;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;

  // Thermodynamic levels    
  for( k = 0; k < nk; k++ ) {
    b_t_8[k] = 0.5 * ( b_m_8[k+1] + b_m_8[k] );
    a_t_8[k] = 0.5 * ( a_m_8[k+1] + a_m_8[k] );
  }
  // Special thermo levels
  b_t_8[nk] = 1.;
  a_t_8[nk] = zsrf_8;

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);    
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);

  for(k = 0; k < nk-1; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrt( hybuser[k+1] * hybuser[k] ), 5 );
  }
  ip1_t[nk-1] = c_convip_Level2IP( sqrt( 1. * hybuser[nk-1] ), 5 );
  ip1_t[nk]   = c_convip_Level2IP(1.,5);
  
  return(VGD_OK);

}

int c_vgrid_genab_5005(float *hybuser, int nk, int *nl_m, int *nl_t, float rcoef1, float rcoef2, double **ptop_out_8, double pref_8, double **PP_a_m_8, double **PP_b_m_8, int **PP_ip1_m, double **PP_a_t_8, double **PP_b_t_8, int **PP_ip1_t, float dhm, float dht)
{
  // Andre Plante May 2015.

  // Define local pointers pointing to "pointer to pointer" to simplify equation below
  double *a_m_8, *b_m_8, *a_t_8, *b_t_8;
  int *ip1_m, *ip1_t;
    
  char ok;
  int k;
  float *hybm, hybtop, rcoef;
  double zsrf_8, ztop_8, zeta_8, lamba_8, pr1, zetau_8, zeta2_8;
  
  *nl_m = nk + 2;
  *nl_t = nk + 2;

  *PP_a_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_a_m_8){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_a_m_8\n");
    return(VGD_ERROR);
  }
  *PP_b_m_8 = malloc( (*nl_m)*sizeof(double) );
  if(! *PP_b_m_8){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_b_m_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_m = malloc( (*nl_m)*sizeof(int) );
  if(! *PP_ip1_m){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_ip1_m\n");
    return(VGD_ERROR);
  }
  *PP_a_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_a_t_8){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_a_t_8\n");
    return(VGD_ERROR);
  }
  *PP_b_t_8 = malloc( (*nl_t)*sizeof(double) );
  if(! *PP_b_t_8){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_b_t_8\n");
    return(VGD_ERROR);
  }
  *PP_ip1_t = malloc( (*nl_t)*sizeof(int) );
  if(! *PP_ip1_t){
    printf("\tIn c_vgrid_genab_5005, malloc error with *PP_ip1_t\n");
    return(VGD_ERROR);
  }

  a_m_8 = *PP_a_m_8;
  b_m_8 = *PP_b_m_8;
  ip1_m = *PP_ip1_m;
  a_t_8 = *PP_a_t_8;
  b_t_8 = *PP_b_t_8;
  ip1_t = *PP_ip1_t;

  zsrf_8  = log(pref_8);
  
  // Auto compute ptop and make B(0) = 0
  zetau_8 = zsrf_8 + log(hybuser[0]);
  zeta2_8 = zsrf_8 + log(hybuser[1]);
  ztop_8  = 0.5 * ( 3. * zetau_8 - zeta2_8);
  (**ptop_out_8) = exp(ztop_8);

  // Checking vertical layering

  //    Check range
  hybtop = (**ptop_out_8) / pref_8;
  if( hybuser[nk-1] >= 1. ) {
    printf("ERROR in c_vgrid_genab_5005: hyb must be < 1.0, got %f\n", hybuser[nk-1]);
    return(VGD_ERROR);
  }
  if( hybuser[0] <= hybtop ) {
    printf("ERROR in c_vgrid_genab_5005: hyb must be > %f, got %f\n", hybtop, hybuser[0]);
    return(VGD_ERROR);
  }

  //Check monotonicity
  for ( k = 1; k < nk; k++){
    if(hybuser[k] <= hybuser[k-1]){
      printf(" WRONG SPECIFICATION OF HYB VERTICAL LEVELS: LEVELS MUST BE MONOTONICALLY INCREASING\n");
      ok=0;
      break;
    }
  }
  if(! ok){
    printf("   Current choice:\n");
    for ( k = 0; k < nk; k++){
      printf("   %f\n", hybuser[k]);
    }
    return(VGD_ERROR);
  }

  // Momentum levels
  pr1 = 1. / (zsrf_8 - zetau_8);
  for( k = 0; k < nk; k++ ) {
    zeta_8  = zsrf_8 + log(hybuser[k]);
    lamba_8  = ( zeta_8 - zetau_8 ) * pr1;
    rcoef  = rcoef2 - ( rcoef2 - rcoef1 ) * lamba_8;
    b_m_8[k] = pow(lamba_8, rcoef);
    a_m_8[k] = zeta_8;
  }
  a_m_8[nk] = zsrf_8;
  b_m_8[nk] = 1.;
  // Integrating the hydrostatic eq with T=0C
  // ln[p(z=dhm)] = ln(ps) - g/(Rd*T)*dhm
  // s = ln(ps) - ln(pref)
  // ln[p(z=dhm)] = ln(pref) - g/(Rd*T)*dhm + s
  // => B=1, A = ln(pref) - g/(Rd*T)*dhm
  // We take T at 0C
  a_m_8[nk+1] = comp_diag_a_height(pref_8,dhm);
  b_m_8[nk+1] = 1.;

  // Thermodynamic levels    
  for( k = 0; k < nk; k++ ) {
    b_t_8[k] = 0.5 * ( b_m_8[k+1] + b_m_8[k] );
    a_t_8[k] = 0.5 * ( a_m_8[k+1] + a_m_8[k] );
  }
  // Special thermo levels
  b_t_8[nk]   = 1.;
  a_t_8[nk]   = zsrf_8;
  a_t_8[nk+1] = comp_diag_a_height(pref_8,dht);
  b_t_8[nk+1] = 1.;

  // Compute ip1 values
  for(k = 0; k < nk; k++ ) {
    ip1_m[k] = c_convip_Level2IP(hybuser[k],5);    
  }
  ip1_m[nk] = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_m[nk+1] = c_convip_Level2IP(dhm,4);

  for(k = 0; k < nk-1; k++ ) {
    ip1_t[k]  = c_convip_Level2IP( sqrt( hybuser[k+1] * hybuser[k] ), 5 );
  }
  ip1_t[nk-1] = c_convip_Level2IP( sqrt( 1. * hybuser[nk-1] ), 5 );
  ip1_t[nk]   = c_convip_Level2IP(1.,5);
  // Encoding kind= 4       : M  [metres] (height with respect to ground level)
  ip1_t[nk+1] = c_convip_Level2IP(dht,4);
  
  return(VGD_OK);

}

int C_get_int(TVGrid *self, char *key, int **value, int *quiet)
{  
  if(! self){
    printf("ERROR (vgd): in c_get_int, null vgrid descriptor\n");
    return (VGD_ERROR);
  }
  if(! *value){
    printf("ERROR (vgd): in c_get_int, value is a NULL pointer\n");
    return(VGD_ERROR);
  }
  if (strcmp(key, "NL_M") == 0){
    **value = self->nl_m;
  } else if (strcmp(key, "NL_T") == 0){
    **value = self->nl_t;
  } else if (strcmp(key, "KIND") == 0){
    **value = self->kind;
  } else if (strcmp(key, "VERS") == 0){
    **value = self->version;
  } else if (strcmp(key, "IP_1") == 0){
    **value = self->rec.ip1;
  } else if (strcmp(key, "DIPM") == 0){
    **value = self->ip1_m[self->nl_m-1];
  } else if (strcmp(key, "DIPT") == 0){
    **value = self->ip1_t[self->nl_t-1];
  } else {
    printf("Invalid key %s given to gd_get (int)\n",key);
    return(VGD_ERROR);
  }
  
  return(VGD_OK);

}

int C_get_int_1d(TVGrid *self, char *key, int **value, int *quiet)
{
  
  if( strcmp(key, "VIPM") == 0 || strcmp(key, "VIP1") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_m * sizeof(int));
      if(! *value){
	printf("In c_get_int_1d problem allocating %d int\n",self->nl_m);
	return(VGD_ERROR);
      }
    }
    my_copy_int(self->ip1_m, value, self->nl_m);
  } else  if( strcmp(key, "VIPT") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_t * sizeof(int));
      if(! *value){
	printf("In c_get_int_1d problem allocating %d int\n",self->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_int(self->ip1_t, value, self->nl_t);
  }else{
    printf("In c_get_int_1d: invalid key '%s'\n",key);
    return(VGD_ERROR);
  }
  
  return(VGD_OK);

}

int C_get_real(TVGrid *self, char *key, float **value, int *quiet)
{

  if(! self){
    printf("ERROR (vgd): in C_get_real, null vgrid descriptor\n");
    return (VGD_ERROR);
  }
  if(! *value){
    printf("ERROR (vgd): in C_get_real, value is a NULL pointer\n");
    return(VGD_ERROR);
  }  

  if( strcmp(key, "RC_1" ) == 0 ){
    if( is_valid(self,rcoef1_valid) ){
      **value = self->rcoef1;
    } else {
      **value = (float) c_get_error(key);
    }
  } else  if( strcmp(key, "RC_2" ) == 0 ){
    if( is_valid(self,rcoef2_valid) ){
      **value = self->rcoef2;
    } else {
      **value = (float) c_get_error(key);
    }
  } else  if( strcmp(key, "DHM " ) == 0 ){
    if( is_valid(self,dhm_valid) ){
      **value = self->dhm;
    } else {
      **value = (float) c_get_error(key);
    }
  } else  if( strcmp(key, "DHT " ) == 0 ){
    if( is_valid(self,dht_valid) ){
      **value = self->dht;
    } else {
      **value = (float) c_get_error(key);
    }
  } else {
    printf("ERROR (vgd): in C_get_real, invalid key '%s'\n",key);
    return(VGD_ERROR);
  }
  return(VGD_OK);

}

int C_get_real8_1d(TVGrid *self, char *key, double **value, int *quiet)
{
  if( strcmp(key, "CA_M") == 0 || strcmp(key, "COFA") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_m * sizeof(double));
      if(! *value){
	printf("In C_get_real8_1d problem allocating %d double\n",self->nl_m);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->a_m_8, value, self->nl_m);
  } else if( strcmp(key, "CB_M") == 0 || strcmp(key, "COFB") == 0 ) {
    if(! *value){
      (*value) = malloc(self->nl_m * sizeof(double));
      if(! *value){
	printf("In C_get_real8_1d problem allocating %d double\n",self->nl_m);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->b_m_8, value, self->nl_m);
  } else if( strcmp(key, "CA_T") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_t * sizeof(double));
      if(! *value){
	printf("In C_get_real8_1d problem allocating %d double\n",self->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->a_t_8, value, self->nl_t);
  } else if( strcmp(key, "CB_T") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_t * sizeof(double));
      if(! *value){
	printf("In C_get_real8_1d problem allocating %d double\n",self->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->b_t_8, value, self->nl_t);
  } else {
    printf("In C_get_real8_1d: invalid key '%s'\n",key);
    return(VGD_ERROR);
  }
    
  
  return(VGD_OK);

}

int C_get_real8_3d(TVGrid *self, char *key, double **value, int *quiet)
{
  //TODO : ajout tests validite self

  int table_size = self->table_ni * self->table_nj * self->table_nk;
  if( strcmp(key, "VTBL") == 0 ){
    if(! *value){
      (*value) = malloc( table_size * sizeof(double));
      if(! *value){
	printf("In C_get_real8_3d problem allocating %d double\n",table_size);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->table, value, table_size);
  } else {
    printf("In C_get_real8_3d: invalid key '%s'\n",key);
    return(VGD_ERROR);
  }

  return(VGD_OK);
}


int C_new_gen(TVGrid **self, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, int *stdout_unit, float *dhm, float *dht)
{

  float *hybm = NULL;
  double *a_m_8 = NULL, *b_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL, l_ptop;
  int *ip1_m = NULL, *ip1_t = NULL, tlift, errorInput;

  if(*self){
    C_vgd_free(self);
  }

  *self = c_vgd_construct();
  if(! *self){
    printf("ERROR (vgd): in C_new_gen, null pointer returned by c_vgd_construct\n");
    return (VGD_ERROR);
  }

  if(C_set_vcode_i(*self, kind, version) == VGD_ERROR)  {
    printf("ERROR (vgd): in C_new_gen, ERROR with C_set_vcode_i");
    return (VGD_ERROR);
  }

  //TODO get better error handling like in new_build
  errorInput = 0;
  errorInput = errorInput + is_required_double((*self), ptop_8,     ptop_8_valid,     "ptop_8"    );
  errorInput = errorInput + is_required_double((*self), ptop_out_8, ptop_out_8_valid, "ptop_out_8");
  errorInput = errorInput + is_required_double((*self), pref_8,     pref_8_valid,     "pref_8"    );
  errorInput = errorInput + is_required_float ((*self), rcoef1,     rcoef1_valid,     "rcoef1"    );
  errorInput = errorInput + is_required_float ((*self), rcoef2,     rcoef2_valid,     "rcoef2"    );
  errorInput = errorInput + is_required_float ((*self), dhm,        dhm_valid,        "dhm"       );
  errorInput = errorInput + is_required_float ((*self), dht,        dht_valid,        "dht"       );  

  if (errorInput != 7 ) {
    return(VGD_ERROR);
  }

  int nk = -1, nl_m = -1, nl_t = -1;

  switch((*self)->vcode) {
  case 1001:	
    nk   = size_hyb;
    nl_m = size_hyb;
    nl_t = size_hyb;
    if(c_vgrid_genab_1001(hyb, size_hyb, &hybm, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR ) {
      free(hybm);
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
    if(c_vgrid_genab_1002(hyb, size_hyb, ptop_8, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR ) {
      free(hybm);
      free(a_m_8);
      free(b_m_8);
      free(ip1_m);
      return(VGD_ERROR);
    }
    break;
  case 1003:
    fprintf(stderr,"ERROR (vgd): in C_new_gen kind=%d, version=%d\n cannot be generated, please use kind 1 of version 2\n",kind,version);
    return(VGD_ERROR);
    break;
  case 2001:
    nk   = size_hyb;
    nl_m = size_hyb;
    nl_t = -1;
    if(c_vgrid_genab_2001(hyb, size_hyb, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR ) {
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
    if(c_vgrid_genab_5001(hyb, size_hyb, *rcoef1, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m) == VGD_ERROR ) {
      free(a_m_8);
      free(b_m_8);
      free(ip1_m);
      return(VGD_ERROR);
    }
    break;
  case 5002:
    nk   = size_hyb;
    tlift = 0;
    if(c_vgrid_genab_5002_5003(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, tlift) == VGD_ERROR ) {
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
    if(c_vgrid_genab_5002_5003(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, tlift) == VGD_ERROR ) {
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
    if(c_vgrid_genab_5004(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, *ptop_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t) == VGD_ERROR ) {
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
    if(c_vgrid_genab_5005(hyb, size_hyb, &nl_m, &nl_t, *rcoef1, *rcoef2, &ptop_out_8, *pref_8, &a_m_8, &b_m_8, &ip1_m, &a_t_8, &b_t_8, &ip1_t, *dhm, *dht) == VGD_ERROR ) {
      free(a_m_8);
      free(b_m_8);
      free(ip1_m);
      free(a_t_8);
      free(b_t_8);
      free(ip1_t);
      return(VGD_ERROR);
    }
    break;
  default:
    printf("ERROR (vgd): in C_new_gen, invalid kind or version, kind = %d, version = %d\n",kind,version);
    return(VGD_ERROR);
  }

  if( VGD_ERROR == C_new_build_vert(self,kind,version,nk,ip1,ip2,ptop_8,pref_8,rcoef1,rcoef2,a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,nl_m,nl_t) ) {
    fprintf(stderr,"ERROR (vgd): in C_new_gen, problem with new_build_vert for kind = %d, version = %d\n",kind,version);
    return(VGD_ERROR);
  }

  free(hybm);
  free(a_m_8);
  free(b_m_8);
  free(a_t_8);
  free(b_t_8);
  free(ip1_m);  
  free(ip1_t);  

  return (VGD_OK);
}

int c_vgd_legacy(TVGrid **self, int unit, int kind) {
  printf("c_vgd_legacy TODO\n");
  return(VGD_ERROR);
}

int C_new_read(TVGrid **self, int unit, char *format, int *ip1, int *ip2, int *kind, int *version) {

  int l_ip1 = -1, l_ip2 = -1, l_kind = -1, l_version = -1, error;
  int i, ni, nj, nk;
  int fstinl;
  int toc_found, count, nkeyList = MAX_DESC_REC;
  int keyList[nkeyList], status;
  TFSTD_ext var;
  TVGrid *self2;

  if(*self){
    C_vgd_free(self);
  }
  
  *self = c_vgd_construct();
  if(! *self){
    printf("ERROR (vgd): in C_new_read, null pointer returned by c_vgd_construct\n");
    return (VGD_ERROR);
  }
  
  if(ip1) {
    l_ip1     = *ip1;
    (*self)->match_ipig = 1;
    if(ip2) {
      l_ip2     = *ip2;
    } else {
      printf("(vgd) ERROR: in C_new_read, expecting optional value ip2\n");      
      return (VGD_ERROR);
    }
  }
  if(ip2 && !ip1){
    printf("(vgd) ERROR: in C_new_read, expecting optional value ip1\n");      
    return (VGD_ERROR);
  }
  if(kind)    l_kind    = *kind;
  if(version) l_version = *version;	
  
  //printf("Unit = %d, format = %s, ip1 = %d, ip1 = %d, kind = %d, version = %d\n",unit, format, l_ip1, l_ip2 , l_kind, l_version);
  
  //==============================
  if (strcmp(format, "FST") == 0){
    //----------------------------

    error = c_fstinl(unit, &ni, &nj, &nk, -1, " ", l_ip1, l_ip2, -1, " ", ZNAME, keyList, &count, nkeyList);
    if (error < 0) {
      printf("(vgd) ERROR: in C_new_read, with fstinl on nomvar !!");
      return(VGD_ERROR);
    }
    if(count == 0){
      printf("(vgd) cannot find %s with the following ips: ip1=%d, ip2=%d\n", ZNAME, l_ip1, l_ip2);
      if((*self)->match_ipig) {
	(*self)->vcode = -1;
	return(VGD_ERROR);
      }
      printf("(vgd) Trying to construct vgrid descriptor from legacy encoding (PT,HY ...)\n");
      printf("in C_new_read, call to c_vgd_legacy TODO");
      if(c_vgd_legacy(self,unit,l_kind) == VGD_ERROR){
	return(VGD_ERROR);
      }
      if(fstd_init(*self) == VGD_ERROR) {
	printf("(vgd) in C_new_read, problem creating record information");
      }
    }
    // Loop on all !! found
    toc_found = 0;
    for( i=0; i < count; i++) {     
      // Check if kind and version match, skip the !! if not.
      if( correct_kind_and_version(keyList[i], l_kind, l_version, &var, &status) == VGD_ERROR) {
	(*self)->valid = 0;
	return(VGD_ERROR);
      }
      if( status != 1) {
	continue;
      }
      // If we reached this stage then the toc satisfy the selection criteria but it may not be the only one.
      if(! toc_found) {
	toc_found = 1;
	if( load_toctoc(*self,var,keyList[i]) == VGD_ERROR ) {
	  printf("(vgd) ERROR: in C_new_read, cannot load !!\n");
	  return(VGD_ERROR);
	}
	continue;
      }
      // If we get at this point this means that there are more than one toc satisfying the selection criteria.
      // We load then all to check if they are the same. If not, we return with an error message.
      self2 = c_vgd_construct();
      if( my_fstprm(keyList[i], &var) == VGD_ERROR ) {
	printf("(vgd) ERROR: in C_new_read, with my_fstprm on keyList[i] = %d\n",keyList[i]);
	return(VGD_ERROR);
      }
      if( load_toctoc(self2,var,keyList[i]) == VGD_ERROR ) {
	printf("(vgd) ERROR: in C_new_read, cannot load !!\n");
	return(VGD_ERROR);
      }
      status = vgdcmp(**self,*self2);
      if ( status != 0 ){
	printf("(vgd) ERROR: in C_new_read, found different entries in vertical descriptors after search on ip1 = %d, ip2 = %d, kind = %d, version = %d, status code is %d\n",l_ip1,l_ip2,l_kind,l_version,status);
      }
      // Ce C_vgd_free n'est pas correct car cela devarait etre **
      //C_vgd_free(&self2);  
    return(VGD_ERROR);
  } // Loop in !! 
    
  } else if (strcmp(format, "BIN") == 0){
    printf("(vgd) ERROR: in C_new_read, TODO format = \"BIN\"\n");
    return(VGD_ERROR);
  } else {
    printf("(vgd) ERROR: in C_new_read, wrong value given to format, expecting FST or BIN got %s\n", format);
    return(VGD_ERROR);
  }
}
