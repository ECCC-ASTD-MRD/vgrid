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
program heights_readref
  
  use vGrid_Descriptors, only: vgd_putopt, VGD_OK, VGD_ERROR
  use Unit_Testing, only: ut_report
  
  implicit none
  integer :: stat, ier, check_heights_readref
  
  stat = VGD_OK
  
  ier = vgd_putopt("ALLOW_SIGMA", .true.)
  
  ier = check_heights_readref('data/dm_5005_from_model_run_with_GZ_VT', "THERMO")
  ier = check_heights_readref('data/dm_5005_from_model_run_with_GZ_VT', "MOMENTUM")
  !ier = check_heights_readref('data/dm_5005_from_model_run_with_GZ_VT', "THERMO-AND-MOMENTUM")
  if( ier == VGD_ERROR ) stat = VGD_ERROR
  
  call ut_report(stat,'Grid_Descriptors, vgd_new')
  
end program heights_readref
!====================================================================
!====================================================================
!====================================================================
integer function check_heights_readref(F_fst_S, F_type_S) result(status)
  
  use vGrid_Descriptors, only: vgd_putopt, VGD_ERROR, VGD_OK
  
  implicit none  
  
  character(len=*) :: F_fst_S, F_type_S
  
  ! Local variables
  integer, save :: lu=10   
  integer :: ier, fnom, fstouv, fstfrm, fstinl, fstprm, count, i
  integer :: read_and_compare
  integer, dimension(:), pointer :: ip1s, ip1sub, fstkeys
  ! Variable for fstprm, sorry...
  integer ::dateo, datyp, deet, dltf, extra1, extra2, extra3, ig1,&
       ig2, ig3, ig4, ip2, ip3, lng, nbits,&
       ni,  nj, nk, npas, swa, ubc
  character(len=12) :: etiket
  character(len=4)  :: nomvar
  character(len=2)  :: typvar
  character(len=1)  :: grtyp
  
  status=VGD_ERROR   
  
  nullify(ip1s, ip1sub, fstkeys)
  
  ier=vgd_putopt('ALLOW_RESHAPE',.true.)
  
  lu=lu+1
  
  ier = fnom(lu,F_fst_S,"RND+R/O",0)
  if(ier.lt.0)then
     print*,'ERROR with fnom on ',trim(F_fst_S)
     return
  endif
  ier = fstouv(lu,'RND')
  if(ier.le.0)then
     print*,'No record in RPN file ',trim(F_fst_S),ier
     return
  endif
  allocate(fstkeys(ier))
  
  select case (trim(F_type_S))
  case ("THERMO")
     nomvar="TT"
  case ("MOMENTUM")
     nomvar="UU"
  case ("THERMO-AND-MOMENTUM")
     nomvar="GZ"
  case DEFAULT
     print*,'Wront value for parameter F_fst_S possibilities are THERMO, MOMENTUM, got ',trim(F_type_S)
     return
  end select
  
  ! Get variable ip1 list
  ier = fstinl(lu,ni,nj,nk,-1,' ',-1,-1,-1,' ',nomvar, fstkeys, count, size(fstkeys))
  if ( count <= 0 )then
     print*,'ERROR: in test heights_readref cannot find any ',trim(nomvar)
     return
  endif
  
  allocate(ip1s(count))
  allocate(ip1sub(count/3))
  
  do i=1,count
     ier = fstprm(fstkeys(i),dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1s(i), &
          ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,  &
          ubc,extra1,extra2,extra3)
     if(modulo(i,3) == 0)then
        ip1sub(i/3) = ip1s(i)
     endif
  enddo
  
  ! Read GZ to compare
  print*,'   Testing '//trim(F_type_S)//' full set for '//trim(F_fst_S)
  if( read_and_compare(lu,ip1s,ip2) == VGD_ERROR )then
     return
  endif
  print*,'   Testing '//trim(F_type_S)//' subset for '//trim(F_fst_S)
  if( read_and_compare(lu,ip1sub,ip2) == VGD_ERROR )then
     return
  endif
  
  ier=fstfrm(lu)
  
  print*,trim(F_fst_S),' is OK'
  
  status=VGD_OK
  
end function check_heights_readref

!====================================================================
!====================================================================
!====================================================================

integer function read_and_compare(lu,ip1s,ip2) result(status)
  use vGrid_Descriptors, only: vgrid_descriptor, vgd_new, vgd_heights, vgd_putopt, VGD_ERROR, VGD_OK
  
  implicit none
  
  integer :: lu, ip2
  integer, dimension(:), intent(in) :: ip1s

  ! Local variables
  integer :: kind, i, j, k, ni, nj, nk, key, ier
  integer :: fstinf, fstluk
  type(vgrid_descriptor) :: vgd
  real, dimension(:,:), pointer :: work
  real, dimension(:,:,:), pointer :: heights
  real :: val, tol
  character(len=0) :: blk_S

  status = VGD_ERROR
  
  nullify(work, heights)
  
  ! Get vertical grid descriptor
  ier = vgd_new(vgd,unit=lu,format="fst")
  if(ier == VGD_ERROR )then
     print*,'Problem getting vertical grid descriptor'
     return
  endif

  if ( vgd_heights(vgd,lu,ip1s,heights) == VGD_ERROR )then
     print*,'ERROR in test heights_readref s/r read_and_compare with vgd_heights'
     return
  endif
  
  ! Read GZ to compare
  allocate( work( size(heights,dim=1), size(heights,dim=2) ) )
  
  do k=1, size(ip1s)
     call convip(ip1s(k),val,kind,-1,blk_S,.false.)
     if( kind == 4) then
        work=val
     else
        key = fstinf(lu,ni,nj,nk,-1,' ',ip1s(k),ip2,-1,' ',"GZ")
        if ( ni /= size(heights,dim=1) .or. nj /= size(heights,dim=2) .or. nk /= 1)then
           print*,'ERROR: in test heights_readref s/r read_and_compare size error on GZ for ip1, ip2=',ip1s(k),ip2
           return
        endif
        if ( key < 0 )then         
           print*,'ERROR: in test heights_readref s/r read_and_compare cannot find any GZ for ip1, ip2=',ip1s(k),ip2
           return
        endif
        if( fstluk(work, key, ni, nj, nk) < 0 )then
           print*,'ERROR: in test heights_readref with fstluk on GZ'
           return
        endif
        work=work*10.
     endif
     do j=1, nj
        do i=1, ni
           if(   work(i,j) < 10.)then
              tol=0.1
           elseif(work(i,j) < 100.)then
              tol=1.0
           elseif(work(i,j) < 1000.)then
              tol=10.
           elseif(work(i,j) < 100000.)then
              tol=100.
           else
              print*,'ERROR: in test heights_readref s/r read_and_compare, enter range for value',work(i,j)
              return
           endif
           if(abs(work(i,j)-heights(i,j,k)) > tol )then
              print*,'Probleme avec heights, pas dans les limites tollerees'
              print*,work(i,j),'vs',heights(i,j,k),i,j,k
              return
           endif
        end do
     end do
  end do
  
  deallocate(work, heights)
  
  status = VGD_OK
  
end function read_and_compare
