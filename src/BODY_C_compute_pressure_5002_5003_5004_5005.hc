
  double *aa_8, *bb_8, *s_8, lvl;
  int ij, k, ijk, ind, l_in_log, l_dpidpis, kind;
  float hyb;

  l_in_log = 0;
  if(in_log){
    l_in_log = *in_log;
  }

  l_dpidpis=0;
  if(dpidpis){
    l_dpidpis = *dpidpis;
  }

  aa_8 = malloc(nk*sizeof(double));
  if(! aa_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate aa_8 of bouble of size %d\n", proc_name, nk);
    return(VGD_ERROR);
  }  
  bb_8 = malloc(nk*sizeof(double));
  if(! bb_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate bb_8 of bouble of size %d\n", proc_name, nk);
    free(aa_8);
    return(VGD_ERROR);
  }

  for(k=0; k < nk; k++) {
    if( (ind = VGD_FindIp1Idx( ip1_list[k], self->ip1_m, self->nl_m) ) != -1 ) {
      aa_8[k] = self->a_m_8[ind];
      bb_8[k] = self->b_m_8[ind];
    } else {
      if( (ind = VGD_FindIp1Idx( ip1_list[k], self->ip1_t, self->nl_t) ) != -1 ) {
	aa_8[k] = self->a_t_8[ind];
	bb_8[k] = self->b_t_8[ind];
      } else {
	printf("(Cvgd) ERROR in %s, cannot find ip1 %d in vgrid descriptor.\n", proc_name,ip1_list[k]);
	free(aa_8);
	free(bb_8);  	
	return(VGD_ERROR);	
      }
    }
  }
  s_8 = malloc(ni*nj*sizeof(double));
  if(! s_8 ) {
    printf("(Cvgd) ERROR in %s, cannot allocate s_8 of bouble of size %dx%d\n", proc_name, ni,nj);
    free(aa_8);
    free(bb_8);
    return(VGD_ERROR);
  }
  for(ij=0; ij < ni*nj; ij++) {
    s_8[ij] = log(sfc_field[ij]/self->pref_8);
  }

  for(k=0, ijk=0; k < nk; k++) {
    for(ij=0; ij < ni*nj; ij++, ijk++) {
      lvl = aa_8[k] + bb_8[k]*s_8[ij];
      levels[ijk] = l_in_log ? lvl : exp(lvl);
    }
  }
  //Force surface pressure to be equal to sfc_field
  //Needed by assimilation section.  
  if(! l_in_log) {
    for(k=0; k < nk; k++) {
      hyb = c_convip_IP2Level(ip1_list[k],&kind);
      if(fabs(hyb - 1.) < .000001 && kind == 5) {
  	ijk=k*ni*nj;
  	for(ij=0; ij < ni*nj; ij++, ijk++) {
  	  levels[ijk] = sfc_field[ij];
  	}
      }
    }
  }

  if( l_dpidpis ){
    if( l_in_log ){
      printf("(Cvgd) ERROR: in %s, cannot get dpidpis in log\n", proc_name);
      free(s_8);
      free(aa_8);
      free(bb_8);
      return(VGD_ERROR);
    }
    for(k=0, ijk=0; k < nk; k++) {
      for(ij=0; ij < ni*nj; ij++, ijk++) {
  	levels[ijk] = bb_8[k]*levels[ijk]/sfc_field[ij];
      }
    }
  }
  
  free(s_8);
  free(aa_8);
  free(bb_8);

  return(VGD_OK);
