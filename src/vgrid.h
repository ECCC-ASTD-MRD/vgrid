
#ifndef VGRID_H
#define VGRID_H

#define VGD_OK       0
#define VGD_ERROR    -1
#define VGD_MISSING  -9999.
#define VGD_MAXSTR_NOMVAR 5
#define VGD_MAXSTR_TYPVAR 3
#define VGD_MAXSTR_ETIKET 13
#define VGD_MAXSTR_GRTYP  2

typedef struct VGD_TFSTD {
   int   dateo;                 // date d'origine du champs
   int   deet;                  // duree d'un pas de temps
   int   npas;                  // pas de temps
   int   nbits;                 // nombre de bits du champs
   int   datyp;                 // type de donnees
   int   ip1,ip2,ip3;           // specificateur du champs
   int   ig1,ig2,ig3,ig4;       // descripteur de grille
   char  typvar[VGD_MAXSTR_TYPVAR]; // type de variable
   char  nomvar[VGD_MAXSTR_NOMVAR]; // nom de la variable
   char  etiket[VGD_MAXSTR_ETIKET]; // etiquette du champs
   char  grtyp[VGD_MAXSTR_GRTYP];   // type de grilles
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
   char  typvar[VGD_MAXSTR_TYPVAR]; // type de variable
   char  nomvar[VGD_MAXSTR_NOMVAR]; // nom de la variable
   char  etiket[VGD_MAXSTR_ETIKET]; // etiquette du champs
   char  grtyp[VGD_MAXSTR_GRTYP];   // type de grilles
} VGD_TFSTD_ext;

typedef struct vgrid_descriptor {
  VGD_TFSTD rec;          // RPN standard file header
  double   ptop_8;        // Top level pressure (Pa)
  double   pref_8;        // Reference pressure (Pa)
  double   *table;        // Complete grid descriptor record
  int      table_ni;      //    ni size of table
  int      table_nj;      //    nj size of table
  int      table_nk;      //    nk size of table
  double   *a_m_8;        // A-coefficients for momentum levels  
  double   *b_m_8;        // B-coefficients for momentum levels
  double   *a_t_8;        // A-coefficients for thermodynamic levels
  double   *b_t_8;        // B-coefficients for thermodynamic levels
  int      *ip1_m;        // ip1 values for momentum levels
  int      *ip1_t;        // ip1 values for momentum levels
  int      nl_m;          // Number of momentum      level (size of a_m_8, b_m_8 and ip1_m)
  int      nl_t;          // Number ot thermodynamic level (size of a_t_8, b_t_8 and ip1_t)
  float    dhm;           // Diag level Height (m) for Momentum variables UU,VV
  float    dht;           // Diag level Height (t) for Thermo variables TT,HU, etc
  char*    ref_name;      // Reference field name
  float    rcoef1;        // Rectification coefficient
  float    rcoef2;        // Rectification coefficient
  int      nk;            // Number of momentum levels
  int      ip1;           // ip1 value given to the 3D descriptor
  int      ip2;           // ip2 value given to the 3D descriptor
  int      unit;          // file unit associated with this 3D descriptor
  int      vcode;         // Vertical coordinate code
  int      kind;          // Vertical coordinate code
  int      version;       // Vertical coordinate code
  char     match_ipig;    // do ip/ig matching for records
  char     valid;         // Validity of structure
} vgrid_descriptor;

#endif // VGRID_H
