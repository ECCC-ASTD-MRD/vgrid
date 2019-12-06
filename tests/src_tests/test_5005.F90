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
program constructor_build_all
  use Vgrid_Descriptors, only: Vgd_new,VGD_ERROR,vgd_putopt,read_vgrid_from_file
  use Unit_Testing, only: ut_report

  implicit none

  integer :: vgdid
  integer, parameter :: lu=10
  integer :: stat,i,check_build
  integer :: fnom,fstouv,fstfrm,fclos
  logical :: ok

  integer, parameter :: nfiles=1
  character(len=200), dimension(nfiles) :: files=(/&
       "data/dm_5004_from_model_run           "&
       /)

  stat = vgd_putopt("ALLOW_SIGMA",.true.)

  ok=.true.

  do i=1,nfiles
     print*,'======================================================'
     print*,'file = ',trim(files(i))
     stat=fnom(lu+i,files(i),"RND",0)
     if(stat.lt.0)then
        print*,'ERROR with fnom on file ',trim(files(i))
        call exit(1)
     endif
     stat=fstouv(lu+i,'RND')
     if(stat.le.0)then
        print*,'No record in RPN file ',trim(files(i))
        call exit(1)
     endif

     if(read_vgrid_from_file(vgdid,unit=lu+i,format="fst",ip1=-1,ip2=-1) == VGD_ERROR)then
        print*,'ERROR: problem with vgd_new on file ',trim(files(i))
        stat=fstfrm(lu+i)
        call exit(1)
     endif
     stat=fstfrm(lu+i)  
     stat=fclos(lu+i)
!!!!     if( check_build(vgdid) == VGD_ERROR) ok=.false.     
!     if( vgd_free(vgdid) == VGD_ERROR)then
!        print*,'Error with vgd_free(vgdid) on file ',trim(files(i))
!        call exit(1)
!     endif

  end do

  call ut_report(ok,'Grid_Descriptors, vgd_get, LOGP 5002')  

end program constructor_build_all

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_get, VGD_ERROR, VGD_OK
  
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: check_build_1001_2001_5999, check_build_1002, check_build_4001, check_build_5001, check_build_5002, check_build_5005, check_build_5100, check_build_21001, check_build_21002
  integer :: vcode

  istat = VGD_ERROR

  ! Get Vcode
  if( vgd_get(vgdid,'vcode',vcode) == VGD_ERROR )return
  print*,'Testing build for Vcode ', vcode

  select case (vcode)
  case (1001,2001)
     if(check_build_1001_2001_5999(vgdid) == VGD_ERROR) return
  case (1002)
     if(check_build_1002(vgdid) == VGD_ERROR) return
  case (4001)
     if(check_build_4001(vgdid) == VGD_ERROR) return
  case (5001)
     if(check_build_5001(vgdid) == VGD_ERROR) return
  case (5002)
     if(check_build_5002(vgdid) == VGD_ERROR) return
  case (5003)
     if(check_build_5002(vgdid) == VGD_ERROR) return
  case (5004)
     if(check_build_5002(vgdid) == VGD_ERROR) return
  case (5005)
     if(check_build_5005(vgdid) == VGD_ERROR) return
  case (5100)
     if(check_build_5100(vgdid) == VGD_ERROR) return
  case (5999)
     if(check_build_1001_2001_5999(vgdid) == VGD_ERROR) return
  case (21001)
     if(check_build_21001(vgdid) == VGD_ERROR) return
  case (21002)
     if(check_build_21002(vgdid) == VGD_ERROR) return
  case DEFAULT
     print*,'ERROR, no function for checking Vcode ',vcode
     print*,'       please add this function in test'
     return
  end select

  istat = VGD_OK

  return
  
end function check_build

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function get_hyb_from_ip1(F_hyb,F_ip1,F_kind) result(istat)

  use Vgrid_Descriptors, only: VGD_OK

  implicit none
  
  integer, intent(in) :: F_kind
  integer, dimension(:), intent(in) :: F_ip1
  real, dimension(:),intent(inout) :: F_hyb
  
  ! Local varibales
  integer :: k
  
  do k=1,size(F_ip1)
     call CONVIP( F_ip1(k), F_hyb(k), F_kind, -1, "", .false.)
  enddo

  istat = VGD_OK

  return

end function get_hyb_from_ip1
!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_1001_2001_5999(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
 
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
  integer :: vgdid2
  
  nullify(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m), &
       ip1=-1,           &
       ip2=-1,           &
       a_m_8=a_m_8,         &
       b_m_8=b_m_8,         &
       ip1_m=ip1_m)         &
       == VGD_ERROR) return

  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif

  deallocate(ip1_m, a_m_8, b_m_8)

  istat = VGD_OK

  return

end function check_build_1001_2001_5999

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_1002(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
 
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m
  real(kind=8) :: ptop_8
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
  integer :: vgdid2
  
  nullify(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PTOP - top level pressure"        , ptop_8) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m), &
              ip1=-1,           &
              ip2=-1,           &
              ptop_8=ptop_8,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              ip1_m=ip1_m)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_1002

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_4001(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
 
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
  integer :: vgdid2
  
  nullify(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m), &
              ip1=-1,           &
              ip2=-1,           &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              ip1_m=ip1_m)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_4001

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_5001(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
  
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m
  real :: rcoef1
  real(kind=8) :: ptop_8, pref_8
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8
  integer :: vgdid2
  
  nullify(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PTOP"                             , ptop_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PREF"                             , pref_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m), &
              ip1=-1,           &
              ip2=-1,           &
              ptop_8=ptop_8,     &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              ip1_m=ip1_m)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, a_m_8, b_m_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_5001

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_5002(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print,VGD_LEN_NAME, VGD_ERROR, VGD_OK
  
   
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version
  integer, dimension(:), pointer :: ip1_m, ip1_t
  real :: rcoef1, rcoef2
  real(kind=8) :: ptop_8, pref_8
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
  real(kind=8), dimension(:,:,:), pointer :: table
  integer :: vgdid2
  character(len=VGD_LEN_NAME) :: rfld_S

  nullify(ip1_m, ip1_t, a_m_8, b_m_8, a_t_8, b_t_8, table)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_T - vertical A coefficient (t)", a_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_T - vertical B coefficient (t)", b_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPT - level ip1 list (t)"        , ip1_t) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PTOP"                             , ptop_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PREF"                             , pref_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_2"                             , rcoef2) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m)-1, &
              ip1=-1,            &
              ip2=-1,            &
              ptop_8=ptop_8,     &
              pref_8=pref_8,     &
              rcoef1=rcoef1,     &
              rcoef2=rcoef2,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              a_t_8=a_t_8,       &
              b_t_8=b_t_8,       &
              ip1_m=ip1_m,       &
              ip1_t=ip1_t)       &
              == VGD_ERROR) return

  if( vgd_get(vgdid,"VTBL", table) == VGD_ERROR ) return
  print*,'table(2,3,1)',table(2,3,1)
  deallocate(table)
  if( vgd_get(vgdid2,"VTBL", table) == VGD_ERROR ) return
  print*,'table(2,3,1)',table(2,3,1)

  if( vgd_get(vgdid,"RFLD",rfld_S ) == VGD_ERROR ) return
  print*,'rfld_S<',rfld_S,'>'
  if( vgd_get(vgdid2,"RFLD",rfld_S ) == VGD_ERROR ) return
  print*,'rfld_S<',rfld_S,'>'

  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     !ier = vgd_print(vgdid)
     !ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, ip1_t, a_m_8, b_m_8, a_t_8, b_t_8, table)
  
  istat = VGD_OK
  
  return
  
end function check_build_5002
!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_5005(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
   
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m, ip1_t
  real :: rcoef1, rcoef2
  real(kind=8) :: pref_8
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, a_t_8, b_t_8
  integer :: vgdid2
  
  nullify(ip1_m, ip1_t, a_m_8, b_m_8, a_t_8, b_t_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_T - vertical A coefficient (t)", a_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_T - vertical B coefficient (t)", b_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPT - level ip1 list (t)"        , ip1_t) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PREF"                             , pref_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_2"                             , rcoef2) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m)-2, &
              ip1=-1,            &
              ip2=-1,            &
              pref_8=pref_8,     &
              rcoef1=rcoef1,     &
              rcoef2=rcoef2,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              a_t_8=a_t_8,       &
              b_t_8=b_t_8,       &
              ip1_m=ip1_m,       &
              ip1_t=ip1_t)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, ip1_t, a_m_8, b_m_8, a_t_8, b_t_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_5005

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_5100(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
  
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local varibales
  integer :: kind, version
  integer, dimension(:), pointer :: ip1_m, ip1_t
  real :: rcoef1, rcoef2, rcoef3, rcoef4
  real(kind=8) :: pref_8
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
  integer :: vgdid2
  
  nullify(ip1_m, ip1_t, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_M - vertical C coefficient (m)", c_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_T - vertical A coefficient (t)", a_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_T - vertical B coefficient (t)", b_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_T - vertical C coefficient (t)", c_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPT - level ip1 list (t)"        , ip1_t) == VGD_ERROR ) return
  if( vgd_get(vgdid,"PREF"                             , pref_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_2"                             , rcoef2) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_3"                             , rcoef3) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_4"                             , rcoef4) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m)-2, &
              ip1=-1,            &
              ip2=-1,            &
              pref_8=pref_8,     &
              rcoef1=rcoef1,     &
              rcoef2=rcoef2,     &
              rcoef3=rcoef3,     &
              rcoef4=rcoef4,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              c_m_8=c_m_8,       &
              a_t_8=a_t_8,       &
              b_t_8=b_t_8,       &
              c_t_8=c_t_8,       &
              ip1_m=ip1_m,       &
              ip1_t=ip1_t)       &
              == VGD_ERROR) return

  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     !ier = vgd_print(vgdid)
     !ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, ip1_t, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_5100
!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_21001(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
  
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local variables
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m, ip1_t
  real :: rcoef1, rcoef2, rcoef3, rcoef4
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8
  integer :: vgdid2
  
  nullify(ip1_m, ip1_t, a_m_8, b_m_8, c_m_8, b_m_8, a_t_8, b_t_8, c_t_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_M - vertical C coefficient (m)", c_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_T - vertical A coefficient (t)", a_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_T - vertical B coefficient (t)", b_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_T - vertical C coefficient (t)", c_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPT - level ip1 list (t)"        , ip1_t) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_2"                             , rcoef2) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_3"                             , rcoef3) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_4"                             , rcoef4) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m)-2, &
              ip1=-1,            &
              ip2=-1,            &
              rcoef1=rcoef1,     &
              rcoef2=rcoef2,     &
              rcoef3=rcoef3,     &
              rcoef4=rcoef4,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              c_m_8=c_m_8,       &
              a_t_8=a_t_8,       &
              b_t_8=b_t_8,       &
              c_t_8=c_t_8,       &
              ip1_m=ip1_m,       &
              ip1_t=ip1_t)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, ip1_t, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_21001

!=============================================================
!=============================================================
!=============================================================
!=============================================================

integer function check_build_21002(vgdid) result(istat)

  use Vgrid_Descriptors, only: vgd_new, vgd_get, vgd_print, VGD_ERROR, VGD_OK
  
   
  implicit none
  integer, intent(in) :: vgdid
  
  ! Local variables
  integer :: kind, version, ier
  integer, dimension(:), pointer :: ip1_m, ip1_t, ip1_w
  real :: rcoef1, rcoef2, rcoef3, rcoef4
  real(kind=8), dimension(:), pointer :: a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, a_w_8, b_w_8, c_w_8
  integer :: vgdid2
  
  nullify(ip1_m, ip1_t, ip1_w, a_m_8, b_m_8, c_m_8, b_m_8, a_t_8, b_t_8, c_t_8, a_w_8, b_w_8, c_w_8)
  
  istat = VGD_ERROR
  
  if( vgd_get(vgdid,"KIND", kind) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VERS", version) == VGD_ERROR ) return  
  if( vgd_get(vgdid,"CA_M - vertical A coefficient (m)", a_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_M - vertical B coefficient (m)", b_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_M - vertical C coefficient (m)", c_m_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_T - vertical A coefficient (t)", a_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_T - vertical B coefficient (t)", b_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_T - vertical C coefficient (t)", c_t_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CA_W - vertical A coefficient (w)", a_w_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CB_W - vertical B coefficient (w)", b_w_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"CC_W - vertical C coefficient (w)", c_w_8) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPM - level ip1 list (m)"        , ip1_m) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPT - level ip1 list (t)"        , ip1_t) == VGD_ERROR ) return
  if( vgd_get(vgdid,"VIPW - level ip1 list (w)"        , ip1_w) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_1"                             , rcoef1) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_2"                             , rcoef2) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_3"                             , rcoef3) == VGD_ERROR ) return
  if( vgd_get(vgdid,"RC_4"                             , rcoef4) == VGD_ERROR ) return

  if( vgd_new(vgdid2,kind,version,size(ip1_m)-2, &
              ip1=-1,            &
              ip2=-1,            &
              rcoef1=rcoef1,     &
              rcoef2=rcoef2,     &
              rcoef3=rcoef3,     &
              rcoef4=rcoef4,     &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              c_m_8=c_m_8,       &
              a_t_8=a_t_8,       &
              b_t_8=b_t_8,       &
              c_t_8=c_t_8,       &
              a_w_8=a_w_8,       &
              b_w_8=b_w_8,       &
              c_w_8=c_w_8,       &
              ip1_m=ip1_m,       &
              ip1_t=ip1_t,       &
              ip1_w=ip1_w)       &
              == VGD_ERROR) return
  
  if(.not. vgdid2 == vgdid)then
     print*,'ERROR : build descriptor not equal has the one read from the file see vgd_print output below'
     ier = vgd_print(vgdid)
     ier = vgd_print(vgdid2)
     return
  endif
  
  deallocate(ip1_m, ip1_t, ip1_w, a_m_8, b_m_8, c_m_8, a_t_8, b_t_8, c_t_8, a_w_8, b_w_8, c_w_8)
  
  istat = VGD_OK
  
  return
  
end function check_build_21002

