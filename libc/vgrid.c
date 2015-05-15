#include "vgrid.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "rpnmacros.h"


// Constants
#define MAX_DESC_REC 10000
#define MAX_VKIND    100

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

int c_is_valid(TVGrid *self, char *valid_table_name)
{
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
    printf("Warning : in c_is_valid, valid_table_name '%s' does not exist\n",valid_table_name);
    return(0);
  }
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

int c_convip_Level2IP(float level, int kind) {

  int    mode=2,flag=0, IP; 
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

int c_print_desc(TVGrid *self, int *stdout, int *convip) {
  if(! self ) {
    printf("In c_print_desc: vgrid structure not constructed\n");
    return(VGD_ERROR);
  } else {
    if(! self->valid) {
      printf("In c_print_desc: vgrid structure is not valid\n");
      return(VGD_ERROR);
    }
    if(stdout && *stdout != 6){
      printf("In c_print_desc : please implement stdout option %d\n",*stdout);
      return(VGD_ERROR);
    }
    if(convip){
      
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
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 2001:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 1003:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 5001:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 5002:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 5003:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    case 5004:
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR);
      break;
    default:
      printf("Invalid kind or version in fstd_init: kind=%d, version=%d\n",self->kind,self->version);
      return(VGD_ERROR);
    }
    
    if(convip){
      printf("c_print_desc TODO!!!!!!!!! ");
      return(VGD_ERROR); 
    }
 
    int k;
    if (is_valid(self, ip1_m_valid) ) {
      printf("  Momentum levels ip1, A, B:\n");
      for ( k = 0; k < self->nk; k++) {
	printf("%d %f %f\n",self->ip1_m[k],self->a_m_8[k],self->b_m_8[k]);
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
      strcpy(vgrid->rec.ETIKET,"            ");
      strcpy(vgrid->rec.NOMVAR,"    ");
      strcpy(vgrid->rec.TYPVAR,"  ");
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
void c_vgd_free(TVGrid **VGrid) {
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
 * Nom      : <c_set_vcode_i>
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
int c_set_vcode_i(TVGrid *VGrid,int Kind,int Version) {
   if( Kind>MAX_VKIND || Kind<0 || Version>999 || Version<0 ) {
      fprintf(stderr,"Invalid kind or version in c_set_vcode_i: kind=%d, version=%d\n",Kind,Version);
      return(VGD_ERROR);
   }

   //printf("dans c_set_vcode_i, Kind = %d, Version = %d\n", Kind, Version);

   VGrid->vcode = Kind*1000 + Version;

   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <c_set_vcode>
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
int c_set_vcode(TVGrid *VGrid) {
   int err,kind,version;

   if( !VGrid->table ) {
      fprintf(stderr,"c_set_vcode called before constructor\n");
      return(VGD_ERROR);
   }

   if( (err=get_version_info(VGrid,&kind,&version)) != VGD_OK ) {
      fprintf(stderr,"Cannot decode table to read kind and version\n");
      return(err);
   }

   return c_set_vcode_i(VGrid,kind,version);
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

   h->IG2=h->IG3=h->IG4=0;

   err=c_set_vcode(VGrid);

   switch(VGrid->vcode) {
      case 1001:
         strcpy(h->ETIKET,"ETA_GEMV3");
         break;
      case 1002:
         strcpy(h->ETIKET,"ETA_GEMV3");
         h->IG2=(int)round(VGrid->ptop_8*10.0);
         break;
      case 2001:
         strcpy(h->ETIKET,"PRESSURE");
         break;
      case 1003:
         strcpy(h->ETIKET,"HYBNORM_GEM3");
         h->IG2=(int)round(VGrid->ptop_8*10.0);
         h->IG3=(int)roundf(VGrid->rcoef1*100.0f);
         break;
      case 5001:
         strcpy(h->ETIKET,"HYB_GEMV3");
         h->IG2=(int)round(VGrid->ptop_8*10.0);
         h->IG3=(int)roundf(VGrid->rcoef1*100.0f);
         break;
      case 5002:
      case 5003:
      case 5004:
         strcpy(h->ETIKET,"STG_CP_GEMV4");
         h->IG2=(int)round(VGrid->ptop_8*10.0);
         h->IG3=(int)roundf(VGrid->rcoef1*100.0f);
         h->IG4=(int)roundf(VGrid->rcoef2*100.0f);
         break;
      default:
         fprintf(stderr,"Invalid kind or version in fstd_init: kind=%d, version=%d\n",VGrid->kind,VGrid->version);
         return(VGD_ERROR);
   }

   strcpy(h->NOMVAR,"!!");
   strcpy(h->TYPVAR,"X");
   strcpy(h->GRTYP,"X");

   h->DATEO       = 0;
   h->DEET        = 0;
   h->NPAS        = 0;
   h->DATYP       = 5;
   h->NBITS       = 64;
   h->IP3         = 0;
   h->IG1         = VGrid->vcode;
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

int c_new_gen(TVGrid **self, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int *ip1, int *ip2, int *stdout_unit, float *dhm, float *dht)
{

  float *hybm = NULL;
  double *a_m_8 = NULL, *b_m_8 = NULL, *a_t_8 = NULL, *b_t_8 = NULL;
  int *ip1_m = NULL, *ip1_t = NULL;

  //printf("IN c_new_gen avant c_vgd_construct\n");
  //printf("IN c_new_gen   self %p\n",  self);
  //printf("IN c_new_gen  *self %p\n", *self);
  if(! *self){
    *self = c_vgd_construct();
    if(! *self){
      printf("In c_new_gen, ERROR with c_vgd_construct");
      return (VGD_ERROR);
    }
  }
  //printf("IN c_new_gen apres c_vgd_construct\n");
  //printf("IN c_new_gen   self %p\n",  self);
  //printf("IN c_new_gen  *self %p\n", *self);
  //printf("IN c_new_gen **self %f\n",**self);
  //printf("==============================================\n");

  if(c_set_vcode_i(*self, kind, version) == VGD_ERROR)  {
    printf("In c_new_gen, ERROR with c_set_vcode_i");
    return (VGD_ERROR);
  }
  //c_vgd_print(*self);

  //printf("self->vcode = %d\n",(*self)->vcode);

  // TODO : ajout test is_valid sur argument optionel

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
  case 1002:
    break;
  case 2001:
    break;
  case 1003:
    break;
  case 5001:
    break;
  case 5002:
  case 5003:
  case 5004:
    break;
  default:
    fprintf(stderr,"Invalid kind or version in c_new_gen: kind=%d, version=%d\n",kind,version);
    return(VGD_ERROR);
  }
  if( VGD_ERROR == c_new_build_vert(self,kind,version,nk,ip1,ip2,ptop_8,pref_8,rcoef1,rcoef2,a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,nl_m,nl_t) ) {
    fprintf(stderr,"In c_new_gen : problem with new_build_vert for: kind=%d, version=%d\n",kind,version);
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

int c_new_build_vert(TVGrid **self, int kind, int version, int nk, int *ip1, int *ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, 
		     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t)
{
  char* cvcode;

  // Check if self is constructed
  if(! *self){
    *self = c_vgd_construct();
    if(! *self){
      printf("In c_new_gen, ERROR with c_vgd_construct");
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

  if(c_set_vcode_i(*self, kind, version) == VGD_ERROR)  {
    printf("In c_new_build_vert, ERROR with c_set_vcode_i");
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
      if(! (*self)->a_m_8) {
	printf("problem allocating a_m_8 in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_m_8, &((*self)->a_m_8), nl_m);
    } else {
      printf("a_m_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, b_m_8_valid)) {
    if(b_m_8){
      free((*self)->b_m_8);
      (*self)->b_m_8 = malloc( nl_m * sizeof(double) );
      if(! (*self)->b_m_8) {
	printf("problem allocating b_m_8 in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_m_8, &((*self)->b_m_8), nl_m);
    } else {
      printf("b_m_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, a_t_8_valid)) {
    if(a_t_8){
      free((*self)->a_t_8);
      (*self)->a_t_8 = malloc( nl_m * sizeof(double) );
      if(! (*self)->a_t_8) {
	printf("problem allocating a_t_8 in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_double(a_t_8, &((*self)->a_t_8), nl_m);
    } else {
      printf("a_t_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, b_t_8_valid)) {
    if(b_t_8){
      free((*self)->b_t_8);
      (*self)->b_t_8 = malloc( nl_m * sizeof(double) );
      if(! (*self)->b_t_8) {
	printf("problem allocating b_t_8 in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_double(b_t_8, &((*self)->b_t_8), nl_m);
    } else {
      printf("b_t_8 is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, ip1_m_valid)) {
    if(ip1_m){
      free((*self)->ip1_m);
      (*self)->ip1_m = malloc( nl_m * sizeof(double) );
      if(! (*self)->ip1_m) {
	printf("problem allocating ip1_m in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_m, &((*self)->ip1_m), nl_m);
    } else {
      printf("ip1_m is a required constructor entry\n");
      missingInput = 1;
    }
  }
  if(is_valid( *self, ip1_t_valid)) {
    if(ip1_t){
      free((*self)->ip1_t);
      (*self)->ip1_t = malloc( nl_m * sizeof(double) );
      if(! (*self)->ip1_t) {
	printf("problem allocating ip1_t in c_new_build_vert\n");
	return(VGD_ERROR);
      }
      my_copy_int(ip1_t, &((*self)->ip1_t), nl_m);
    } else {
      printf("ip1_t is a required constructor entry\n");
      missingInput = 1;
    }
  }

  if (missingInput)
    return (VGD_ERROR);

  int ier;
  // Fill table with version-specific encoder
  switch((*self)->vcode) {
  case 1001:
    cvcode = strdup("1001");
    ier = c_encode_vert_1001(self,nk);
    break;
  case 1002:
    break;
  case 2001:
    break;
  case 1003:
    break;
  case 5001:
    break;
  case 5002:
  case 5003:
  case 5004:
    break;
  default:
    fprintf(stderr,"IN c_new_build_vert: invalid kind or version : kind=%d, version=%d\n",kind,version);
    return(VGD_ERROR);
  }

  if(ier == VGD_ERROR)
    printf("problem with encode_vert_%s\n",cvcode);

  free(cvcode);

  return(VGD_OK);

}

int c_encode_vert_1001(TVGrid **self,int nk){
  
  int skip = 2;
  free( (*self)->table );
  (*self)->table = malloc ( 3*(nk+skip)*sizeof(double) );
  if(! (*self)->table ) {
    printf("In c_encode_vert_1001: ERROR allocating table of bouble of size %d\n",3*(nk+skip) );
    return(VGD_ERROR);
  }
  strcpy((*self)->ref_name,"P0");

  //TODO : voir run pour error = flip_transfer(self%ref_name,for_char_8)
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

int c_vgrid_genab_1001(float *hyb, int nk, float **hybm, double **a_m_8, double **b_m_8, int **ip1_m)
{

  // Andre Plante May 2015. 

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
  
  int k,ip1, kind2, kind = 1;

  int ok=1;
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

int c_get_int(TVGrid *self, char *key, int **value, int *quiet)
{
  if(! *value){
    printf("In c_get_int value is a NULL pointer\n");
    return(VGD_ERROR);
  }
  if (strcmp(key, "NL_M") == 0){
    **value = self->nl_m;    
  } else if (strcmp(key, "NL_T") == 0){
    **value = self->nl_t;    
  } else {
    printf("Invalid key %s given to gd_get (int)",key);
    return(VGD_ERROR);	       
  }
  
  return(VGD_OK);

}

int c_get_int_1d(TVGrid *self, char *key, int **value, int *quiet)
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

int c_get_real8_1d(TVGrid *self, char *key, double **value, int *quiet)
{
  if( strcmp(key, "CA_M") == 0 || strcmp(key, "COFA") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_m * sizeof(double));
      if(! *value){
	printf("In c_get_real8_1d problem allocating %d double\n",self->nl_m);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->a_m_8, value, self->nl_m);
  } else if( strcmp(key, "CB_M") == 0 || strcmp(key, "COFB") == 0 ) {
    if(! *value){
      (*value) = malloc(self->nl_m * sizeof(double));
      if(! *value){
	printf("In c_get_real8_1d problem allocating %d double\n",self->nl_m);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->b_m_8, value, self->nl_m);
  } else if( strcmp(key, "CA_T") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_t * sizeof(double));
      if(! *value){
	printf("In c_get_real8_1d problem allocating %d double\n",self->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->a_t_8, value, self->nl_t);
  } else if( strcmp(key, "CB_T") == 0 ){
    if(! *value){
      (*value) = malloc(self->nl_t * sizeof(double));
      if(! *value){
	printf("In c_get_real8_1d problem allocating %d double\n",self->nl_t);
	return(VGD_ERROR);
      }
    }
    my_copy_double(self->b_t_8, value, self->nl_t);
  } else {
    printf("In c_get_real8_1d: invalid key '%s'\n",key);
    return(VGD_ERROR);
  }
    

  return(VGD_OK);

}
