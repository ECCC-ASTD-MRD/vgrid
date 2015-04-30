#include <vgrid.h>
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

/*----------------------------------------------------------------------------
 * Nom      : <VGD_New>
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
TVGrid* VGD_New() {
   TVGrid *vgrid = malloc(sizeof(TVGrid));

   if( vgrid ) {
      vgrid->ptop_8        = VGD_MISSING;
      vgrid->pref_8        = VGD_MISSING;
      vgrid->table         = NULL;
      vgrid->a_m_8         = NULL;
      vgrid->b_m_8         = NULL;
      vgrid->a_t_8         = NULL;
      vgrid->b_t_8         = NULL;
      vgrid->ip1_m         = NULL;
      vgrid->ip1_t         = NULL;
      vgrid->rcoef1        = VGD_MISSING;
      vgrid->rcoef2        = VGD_MISSING;
      vgrid->initialized   = 0;
      vgrid->valid         = 0;
      vgrid->ip1           = 0;
      vgrid->ip2           = 0;
      vgrid->ref_name      = strdup("None");

      vgrid->rec.initialized = 0;
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
void VGD_Free(TVGrid *VGrid) {
   if( VGrid ) {
      FREE(VGrid->table);
      FREE(VGrid->a_m_8);
      FREE(VGrid->b_m_8);
      FREE(VGrid->a_t_8);
      FREE(VGrid->b_t_8);
      FREE(VGrid->ip1_m);
      FREE(VGrid->ip1_t);
      FREE(VGrid->ref_name);

      free(VGrid);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <VGD_Fstprm>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Get info about a record
 *
 * Parametres :
 *    <H>   : [OUT] Record information
 *    <Key> : Key to a FST file record
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int VGD_Fstprm(TFSTD *H,int Key) {
   int err;
   double nhours;

   err=c_fstprm(Key,&H->DATEO,&H->DEET,&H->NPAS,&H->NI,&H->NJ,&H->NK,&H->NBITS,&H->DATYP,
         &H->IP1,&H->IP2,&H->IP3,H->TYPVAR,H->NOMVAR,H->ETIKET,H->GRTYP,&H->IG1,&H->IG2,
         &H->IG3,&H->IG4,&H->SWA,&H->LNG,&H->DLTF,&H->UBC,&H->EX1,&H->EX2,&H->EX3);
   if( err ) {
      fprintf(stderr,"Cannot c_fstprm for key %d\n",Key);
      return(VGD_ERROR);
   }

   nhours = H->DEET*H->NPAS/3600.0;
   err = f77name(incdatr)(&H->DATEV,&H->DATEO,&nhours);
   if( err ) {
      fprintf(stderr,"Couldn't get DateV for dateo(%d),deet(%d),npas(%d),nhours(%f) (incdatr)\n",H->DATEO,H->DEET,H->NPAS,nhours);
      return(VGD_ERROR);
   }

   return(VGD_OK);
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
 * Nom      : <set_vcode_i>
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
int set_vcode_i(TVGrid *VGrid,int Kind,int Version) {
   if( Kind>MAX_VKIND || Kind<0 || Version>999 || Version<0 ) {
      fprintf(stderr,"Invalid kind or version in set_vcode_i: kind=%d, version=%d\n",Kind,Version);
      return(VGD_ERROR);
   }

   VGrid->vcode = Kind*1000 + Version;

   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <set_vcode>
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
int set_vcode(TVGrid *VGrid) {
   int err,kind,version;

   if( !VGrid->table ) {
      fprintf(stderr,"set_vcode called before constructor\n");
      return(VGD_ERROR);
   }

   if( (err=get_version_info(VGrid,&kind,&version)) != VGD_OK ) {
      fprintf(stderr,"Cannot decode table to read kind and version\n");
      return(err);
   }

   return set_vcode_i(VGrid,kind,version);
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

   if( h->initialized )
      return(VGD_OK);

   h->IG2=h->IG3=h->IG4=0;

   err=set_vcode(VGrid);

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
   h->initialized = 1;

   return(VGD_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <new_read>
 * Creation : Avril 2015 - E. Legault-Ouellet - CMC/CMOE
 *
 * But      : Coordinate constructor - read from a file and initialize instance
 *
 * Parametres :
 *    <VGrid>  : The grid structure
 *    <Format> : File format ('fst' or 'bin') default is 'fst'
 *    <Ip1>    : Ip1 value of the desired descriptor
 *    <Ip2>    : Ip2 value of the desired descriptor
 *    <Kind>   : Level kind requested by user.
 *
 * Retour   :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int new_read(TVGrid *VGrid,int Unit,char *Format,int Ip1,int Ip2,int Kind) {
   TFSTD* h=&VGrid->rec;
   int key,err,lkind,version;

   VGrid->valid      = 0;
   VGrid->unit       = Unit;
   VGrid->match_ipig = Ip1>=0;

   if( !Format || strcmp(Format,"FST")==0 ) {
      // RPN standard file input
      key=c_fstinf(Unit,&h->NI,&h->NJ,&h->NK,-1,"",Ip1,Ip2,-1,"","!!");
      if( key < 0 ) {
         fprintf(stderr,"Cannot find !! with the following: ip1=%d ip2=%d\n",Ip1,Ip2);
         if( VGrid->match_ipig ) {
            VGrid->vcode = -1;
            return(VGD_ERROR);
         }

         // TODO: call vgd_legacy

         if( (err=fstd_init(VGrid)) < 0 ) {
            fprintf(stderr,"Problem creating record information\n");
            return(VGD_ERROR);
         }
      } else {
         VGrid->table = malloc(h->NI*h->NJ*h->NK*sizeof(double));
         if( !VGrid->table ) {
            fprintf(stderr,"Unable to allocate memory for VGrid->table\n");
            return(VGD_ERROR);
         }

         err=c_fstluk(VGrid->table,key,&h->NI,&h->NJ,&h->NK);
         if( err < 0 ) {
            fprintf(stderr,"Problem with c_fstluk on !!\n");
            return(VGD_ERROR);
         }

         err=fstd_init(VGrid);
         if( err < 0 ) {
            fprintf(stderr,"Problem creating record information\n");
            return(VGD_ERROR);
         }

         err=c_fstprm(key,&h->DATEO,&h->DEET,&h->NPAS,&h->NI,&h->NJ,&h->NK,&h->NBITS,&h->DATYP,
               &h->IP1,&h->IP2,&h->IP3,h->TYPVAR,h->NOMVAR,h->ETIKET,h->GRTYP,&h->IG1,&h->IG2,
               &h->IG3,&h->IG4,&h->SWA,&h->LNG,&h->DLTF,&h->UBC,&h->EX1,&h->EX2,&h->EX3);

         if( Ip1==-1 && Ip2==-1 ) {
            int keyList[MAX_DESC_REC],nkeys;

            // Check for multiple (different) descriptors
            err=c_fstinl(Unit,&h->NI,&h->NJ,&h->NK,-1,"",Ip1,Ip2,-1,"!!",keyList,&nkeys,MAX_DESC_REC);
            if( err < 0 ) {
               fprintf(stderr,"Problem retrieving !! keys for ip1,ip2: %d,%d\n",Ip1,Ip2);
               VGrid->vcode = -1;
               return(VGD_ERROR);
            }

            if( nkeys > 1 ) {
               TFSTD l_h;
               double *l_table;
               int i;

               l_table = malloc(h->NI*h->NJ*h->NK*sizeof(double));
               if( !l_table ) {
                  fprintf(stderr,"Unable to allocate l_table\n");
                  return(VGD_ERROR);
               }
               
               for(i=1; i<nkeys; ++i) {
                  err=VGD_Fstprm(&l_h,keyList[i]);
                  if( err < 0 ) {
                     fprintf(stderr,"Problem with VGD_Fstprm in new_read\n");
                     VGrid->vcode = -1;
                     free(l_table);
                     return(VGD_ERROR);
                  }

                  // Make sure the dimensions are the same (else memory corruption could occur while doing the next fstluk)
                  if( l_h.NI!=h->NI || l_h.NJ!=h->NJ || l_h.NK!=h->NK ) {
                     fprintf(stderr,"Found different entry dimensions in vertical descriptors after wildcard (-1) search\n");
                     free(l_table);
                     return(VGD_ERROR);
                  }

                  err=c_fstluk(l_table,keyList[i],&l_h.NI,&l_h.NJ,&l_h.NK);
                  if( err < 0 ) {
                     fprintf(stderr,"Problem with c_fstluk on l_table i=%d\n",i);
                     free(l_table);
                     return(VGD_ERROR);
                  }

                  if( memcmp(l_table,VGrid->table,h->NI*h->NJ*h->NK*sizeof(double))!=0 ) {
                     fprintf(stderr,"Found different entries in vertical descriptors after wildcard (-1) search\n");
                     free(l_table);
                     return(VGD_ERROR);
                  }
               }

               free(l_table);
            }
         }
      }
   } else if( strcmp(Format,"BIN")==0 ) {
      // TODO : Check to see if the unit handle is passed from fortran code or C code. The former might be a problem...
      // TODO : implementation
      fprintf(stderr,"BIN format is unimplemented\n");
      return(VGD_ERROR);
   } else {
      fprintf(stderr,"Invalid constructor format request %s\n",Format);
      return(VGD_ERROR);
   }

   err=get_version_info(VGrid,&lkind,&version);
   if( lkind!=Kind && Kind!=-1 ) {
      fprintf(stderr,"Cannot find requested kind=%d in file\n",Kind);
      return(VGD_ERROR);
   }

   VGrid->valid = 1;
   if( VGrid->vcode >= 0 ) {
      return(VGD_OK);
   } else {
      fprintf(stderr,"WARNING: unitilisatized descriptor\n");
      return(VGD_ERROR);
   }
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
      if( (idxs[k]=VGD_FindIp1Idx(Ip1List[k],VGrid->ip1_m,VGrid->m_nb)) == -1 ) {
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
