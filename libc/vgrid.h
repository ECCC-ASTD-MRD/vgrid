
#ifndef VGRID_H
#define VGRID_H

#define VGD_OK       0
#define VGD_ERROR    -1
#define VGD_MISSING  -9999.
int ALLOW_RESHAPE = 0;

typedef struct TFSTD {
   //int   FID;                 // File ID (file unit) dont provient le champ
   //int   KEY;                 // Cle du champs
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
   int   ex1,ex2,ex3;
   char  typvar[3];           // type de variable
   char  nomvar[5];           // nom de la variable
   char  etiket[13];          // etiquette du champs
   char  grtyp[2];            // type de grilles
   char  fstd_initialized;    // if the fstd struct is initialized
} TFSTD;

typedef struct TVGrid {
  TFSTD    rec;           // RPN standard file header
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
} TVGrid;

int c_new_gen(TVGrid **self, int kind, int version, float *hyb, int size_hyb, float *rcoef1, float *rcoef2,
	      double *ptop_8, double *pref_8, double *ptop_out_8,
	      int ip1, int ip2, int *stdout_unit, float *dhm, float *dht);

int c_new_build_vert(TVGrid **self, int kind, int version, int nk, int ip1, int ip2, double *ptop_8, double *pref_8, float *rcoef1, float *rcoef2, 
		     double *a_m_8, double *b_m_8, double *a_t_8, double *b_t_8, int *ip1_m, int *ip1_t, int nl_m, int nl_t);

int c_vgrid_genab_1001(float *hyb, int nk, float **hybm, double **a_m_8, double **b_m_8, int **ip1_m);


#endif // VGRID_H
