
#ifndef VGRID_H
#define VGRID_H

#define VGD_OK       0
#define VGD_ERROR    -1
#define VGD_MISSING  -9999.

typedef struct TFSTD {
   //int   FID;                 // File ID (file unit) dont provient le champ
   //int   KEY;                 // Cle du champs
   int   DATEO;               // Date d'origine du champs
   int   DATEV;               // Date de validitee du champs
   int   DEET;                // Duree d'un pas de temps
   int   NPAS;                // Pas de temps
   int   NBITS;               // Nombre de bits du champs
   int   DATYP;               // Type de donnees
   int   IP1,IP2,IP3;         // Specificateur du champs
   int   NI,NJ,NK;            // Dimensions
   int   IG1,IG2,IG3,IG4;     // Descripteur de grille
   int   SWA;
   int   LNG;
   int   DLTF;
   int   UBC;
   int   EX1,EX2,EX3;
   char  TYPVAR[3];           // Type de variable
   char  NOMVAR[5];           // Nom de la variable
   char  ETIKET[13];          // Etiquette du champs
   char  GRTYP[2];            // Type de grilles
   char  initialized;         // If the struct is initialized
} TFSTD;

typedef struct TVGrid {
   TFSTD    rec;           // RPN standard file header
   double   ptop_8;        // Top level pressure (Pa)
   double   pref_8;        // Reference pressure (Pa)
   double   *table;        // Complete grid descriptor record
   double   *a_m_8;        // A-coefficients for momentum levels
   double   *b_m_8;        // B-coefficients for momentum levels
   double   *a_t_8;        // A-coefficients for thermodynamic levels
   double   *b_t_8;        // B-coefficients for thermodynamic levels
   int      *ip1_m;        // ip1 values for momentum levels
   int      *ip1_t;        // ip1 values for momentum levels
   char*    ref_name;      // Reference field name
   float    rcoef1;        // Rectification coefficient
   float    rcoef2;        // Rectification coefficient
   int      m_nb;          // Number of momentum levels
   int      ip1;           // ip1 value given to the 3D descriptor
   int      ip2;           // ip2 value given to the 3D descriptor
   int      unit;          // file unit associated with this 3D descriptor
   int      vcode;         // Vertical coordinate code
   int      kind;          // Vertical coordinate code
   int      version;       // Vertical coordinate code
   char     initialized;   // initialization status of the structure
   char     match_ipig;    // do ip/ig matching for records
   char     valid;         // Validity of structure
} TVGrid;

#endif // VGRID_H
