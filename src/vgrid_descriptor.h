// libdescrip - Vertical grid descriptor library for FORTRAN programming
// Copyright (C) 2016  Direction du developpement des previsions nationales
//                     Centre meteorologique canadien
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include <stdint.h>
#ifndef VGRID_DESCRIPTOR_H
#define VGRID_DESCRIPTOR_H

#define VGD_LEN_NAME 5
#define VGD_LEN_ETIK 13
#define VGD_LEN_TYPVAR 3
#define VGD_LEN_GRTYP  2

typedef struct VGD_TFSTD {
   int   dateo;                 // date d'origine du champs
   int   deet;                  // duree d'un pas de temps
   int   npas;                  // pas de temps
   int   nbits;                 // nombre de bits du champs
   int   datyp;                 // type de donnees
   int   ip1,ip2,ip3;           // specificateur du champs
   int   ig1,ig2,ig3,ig4;       // descripteur de grille
   char  typvar[VGD_LEN_TYPVAR]; // type de variable
   char  nomvar[VGD_LEN_NAME]; // nom de la variable
   char  etiket[VGD_LEN_ETIK]; // etiquette du champs
   char  grtyp[VGD_LEN_GRTYP];   // type de grilles
   char  fstd_initialized;      // if the fstd struct is initialized
} VGD_TFSTD;

typedef struct VGD_TFSTD_ext {
   int   dateo;               // date d'origine du champs
   int   datev;               // date de validitee du champs
   int   deet;                // duree d'un pas de temps
   int   npas;                // pas de temps
   int   nbits;               // nombre de bits du champs
   int   datyp;               // type de donnees
   int   ip1,ip2,ip3;         // specificateur du champs
   int   ni,nj,nk;            // dimensions
   int   ig1,ig2,ig3,ig4;     // descripteur de grille
   int   swa;
   int   lng;
   int   dltf;
   int   ubc;
   int   extra1,extra2,extra3;
   char  typvar[VGD_LEN_TYPVAR]; // type de variable
   char  nomvar[VGD_LEN_NAME]; // nom de la variable
   char  etiket[VGD_LEN_ETIK]; // etiquette du champs
   char  grtyp[VGD_LEN_GRTYP];   // type de grilles
} VGD_TFSTD_ext;
#endif // VGRID_DESCRIPTOR_H
