! * libdescrip - Vertical grid descriptor library for FORTRAN programming
! * Copyright (C) 2016  Direction du developpement des previsions nationales
! *                     Centre meteorologique canadien
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
program tests
  use vGrid_Descriptors, only: vgd_new,vgd_get,vgd_dpidpis,VGD_OK
  use Unit_Testing, only: ut_report
  

  implicit none

  integer :: stat,lu=0,ni,nj,nk,nkk,fnom,fstouv,fstfrm,fstlir,fstinf,k,lutxt=69,ip1,ip2
  integer, parameter :: i0=20,j0=10
  integer, dimension(:), pointer :: ip1_list
  integer :: i,j,tid,ii,jj
  integer, external :: omp_get_thread_num,omp_get_num_threads
  real, dimension(:), pointer :: dpidpis_profil
  real(kind=8), dimension(:), pointer :: dpidpis_profil_8
  real, dimension(:,:,:), pointer :: dpidpis_3d,dpidpis_3d_from_prof
  real(kind=8), dimension(:,:,:), pointer :: dpidpis_3d_8,dpidpis_3d_from_prof_8
  real :: epsilon=1.e-5
  real, dimension(:,:), pointer :: p0,px
  real(kind=8), dimension(:,:), pointer ::  p0_8
  real, dimension(:), pointer :: p00
  real(kind=8), dimension(:), pointer :: p00_8
  real :: w1
  integer :: vgdid
  logical :: ok
  real(kind=8), dimension(:), pointer :: coef_b

  nullify(dpidpis_profil,dpidpis_profil_8,ip1_list,dpidpis_3d,dpidpis_3d_8,dpidpis_3d_from_prof,dpidpis_3d_from_prof_8,p0,px,p0_8,p00,p00_8,coef_b)

  stat=fnom(lu,"data/dm_5002_from_model_run","RND+R/O",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom'
     call exit(1)
  endif
  stat=fstouv(lu,'RND')
  if(stat.lt.0)then
     print*,'No record in RPN file'
     call exit(1)
  endif
  open(unit=lutxt,file='data/dm_5002_ips.txt',status='OLD')
  read(lutxt,*) ip1,ip2
  close(lutxt)

  ! Get dpidpis
  stat = vgd_new(vgdid,unit=lu,format="fst",ip1=ip1,ip2=ip2)

  stat = vgd_get(vgdid,key='CB_T - vertical B coefficient (t)',value=coef_b)

  stat = vgd_get(vgdid,key='VIPT - level ip1 list (t)'        ,value=ip1_list)

  stat = fstinf(lu,ni,nj,nk,-1,' ',-1,-1,-1,' ',"UU")

  nk=size(ip1_list)
  allocate(p0(ni,nj),p00(ni*nj),p00_8(ni*nj),p0_8(ni,nj),px(ni,nj),dpidpis_3d_from_prof(ni,nj,nk),dpidpis_3d_from_prof_8(ni,nj,nk))

  stat = fstlir(p0,lu,ni,nj,nkk,-1,'',-1,-1,-1,'','P0')
  stat = fstlir(p00,lu,ni,nj,nkk,-1,'',-1,-1,-1,'','P0')
  p0  = p0*100. !mb to Pa
  p0_8=p0
  p00 = p00*100. !mb to Pa
  p00_8=p00

  stat = vgd_dpidpis(vgdid,sfc_field=p0  ,ip1_list=ip1_list,dpidpis=dpidpis_3d)  
  stat = vgd_dpidpis(vgdid,sfc_field=p0_8,ip1_list=ip1_list,dpidpis=dpidpis_3d_8)  

!$omp parallel private(tid,dpidpis_profil,dpidpis_profil_8,ii,jj,i,k,stat) shared(ni,nj,nk,vgdid,p00,ip1_list,lu)
  tid=omp_get_thread_num()
  if (tid .eq. 0) then
     print*,'number of threads=',omp_get_num_threads()
  endif
  nullify(dpidpis_profil,dpidpis_profil_8)
  allocate(dpidpis_profil(size(ip1_list)),dpidpis_profil_8(size(ip1_list)))
!$omp do
  do i=1,ni*nj
     !print*,'i=',i
     stat = vgd_dpidpis(vgdid,sfc_field=p00(i),ip1_list=ip1_list,dpidpis=dpidpis_profil)         
     if(stat.ne.VGD_OK)then
        print*,'ERROR: problem with vgd_dpidpis profil for i=',i
        stat=fstfrm(lu)
        call exit(1)
     endif
     stat = vgd_dpidpis(vgdid,sfc_field=p00_8(i),ip1_list=ip1_list,dpidpis=dpidpis_profil_8)         
     if(stat.ne.VGD_OK)then
        print*,'ERROR: problem with vgd_dpidpis profil real(kind=8) for i=',i
        stat=fstfrm(lu)
        call exit(1)
     endif
     ii=mod(i,ni)
     if(ii.eq.0)ii=ni
     jj=(i-ii)/ni+1
     !print*,'ii,jj=',ii,jj
     do k=1,nk
        dpidpis_3d_from_prof  (ii,jj,k)=dpidpis_profil  (k)
        dpidpis_3d_from_prof_8(ii,jj,k)=dpidpis_profil_8(k)
     enddo
  enddo
!$omp enddo
  deallocate(dpidpis_profil,dpidpis_profil_8)
!$omp end parallel

  if(associated(dpidpis_profil))deallocate(dpidpis_profil)
  stat = vgd_dpidpis(vgdid,sfc_field=p0(i0,j0),ip1_list=ip1_list,dpidpis=dpidpis_profil)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_dpidpis'
     stat=fstfrm(lu)
     call exit(1)
  endif
  
  if(associated(dpidpis_profil_8))deallocate(dpidpis_profil_8)
  stat = vgd_dpidpis(vgdid,sfc_field=p0_8(i0,j0),ip1_list=ip1_list,dpidpis=dpidpis_profil_8)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_dpidpis real(kind=8)'
     stat=fstfrm(lu)
     call exit(1)
  endif
  
  OK=.true.
  do k=1,nk
     do j=1,nj
        do i=1,ni
           if(dpidpis_3d_from_prof(i,j,k).ne.dpidpis_3d(i,j,k))then
              print*,'Problem : OpenMP do not reproduce result for real'
              print*,'problem in i,k,j',i,k,j,dpidpis_3d_from_prof(i,j,k),dpidpis_3d(i,j,k)
              OK=.false.
              exit
           endif
           if(dpidpis_3d_from_prof_8(i,j,k).ne.dpidpis_3d_8(i,j,k))then
              print*,'Problem : OpenMP do not reproduce result for real(kind=8)'
              print*,'problem in i,k,j',i,k,j,dpidpis_3d_from_prof_8(i,j,k),dpidpis_3d_8(i,j,k)
              OK=.false.
              exit
           endif
        enddo
     enddo
  enddo

  do k=1,size(coef_b)
     stat = fstlir(px,lu,ni,nj,nkk,-1,'',ip1_list(k),-1,-1,'','PX')
     w1=coef_b(k)*px(i0,j0)*100./p0(i0,j0)
     print*,w1,dpidpis_profil(k)
     if(abs(dpidpis_profil(k)- w1)/w1>epsilon)then
        print*,'OUPS'
        OK=.false.
     endif
  enddo
  
  deallocate(dpidpis_profil,dpidpis_profil_8)
  deallocate(p0,p00,p0_8,P00_8,px,dpidpis_3d,dpidpis_3d_8,dpidpis_3d_from_prof,dpidpis_3d_from_prof_8,ip1_list,coef_b)
  
!  stat = vgd_free(d)
  
  stat=fstfrm(lu)

  call ut_report(ok,message='Grid_Descriptors::vgd_dpidpis level calculation status')
  
end program tests
