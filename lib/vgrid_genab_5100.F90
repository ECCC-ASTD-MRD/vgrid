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
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
module vdescript_5100
   ! SLEVE-like coordinate (see Schar et al 2002)
   implicit none
   private
   
   ! Define class variables
#include "vgrid.hf"
   
   ! Define and publicize interfaces
   public :: vgrid_genab_5100
   interface reallocate
      module procedure PRIV_reallocate4
      module procedure PRIV_reallocate8
      module procedure PRIV_reallocateI
   end interface reallocate
   
contains
   !
   !**s/r vgrid_genab_5100  - Generates GEM model vertical grid parameters A, B and hyb
   !                      for momentun and thermodymic levels
   !
   subroutine vgrid_genab_5100 (F_hybuser, F_rcoef, F_pref_8, &
        F_am_8, F_bm_8, F_cm_8, F_at_8, F_bt_8, F_ct_8,&
        F_ip1_m, F_ip1_t, F_err, ptop_out_8, dhm, dht, avg_L)
      
      ! Modifs 
      
      use utils, only: comp_diag_a
      
      implicit none
      !      
      ! Arguments
      integer, intent(out) :: F_err
      real, dimension(:), intent(in) :: F_hybuser                ! user specification for vertical layering hyb
      real, dimension(:), intent(in) :: F_rcoef                  ! user specification for rcoef
      real*8 , optional, intent(out):: ptop_out_8                ! computed ptop if F_ptop_8 < 0
      real*8 , intent(in) :: F_pref_8                            ! user specification for reference pressure
      
      real*8, dimension(:), pointer :: F_am_8, F_bm_8, F_cm_8   ! model As and Bs on momentum levels
      real*8, dimension(:), pointer :: F_at_8, F_bt_8, F_ct_8   ! model As and Bs on thermodynamic levels
      integer, dimension(:), pointer :: F_ip1_m,F_ip1_t         ! ip1 values for momentum and thermodynamic levels
      real, intent(in) :: dhm, dht                              ! Diag levels Height (m) for Momentum/Thermo levels
      logical, optional :: avg_L                                ! Take thermo parameters as averge of momentum ones.
      
      ! Local variables
      integer :: k, status, nk
      logical :: wronghyb, my_avg_L
      real*8  :: pr1, ztop_8, zsrf_8, lamba_8,zeta_8, zeta1_8,zeta2_8, l_ptop_8
      real*8, parameter  :: lamba_8_ep = 1.e-6
      real, dimension(:), pointer :: hybm, hybt             ! model hyb values
      real*8, dimension(:), pointer :: at_8, bt_8
      real :: rcoefL, rcoef
      integer, dimension(:), pointer :: ip1_t
      character(len=100) :: func_name
      real, parameter :: RGASD       =    0.287050000000E+03
      real, parameter :: GRAV        =    0.980616000000E+01
      real, parameter :: TCDK        =    0.273150000000E+03
      !     __________________________________________________________________
      
      func_name='vgrid_genab_5100'
      
      F_err = VGD_ERROR
      
      nullify(hybm,hybt,at_8,bt_8,ip1_t)
      
      my_avg_L=.true.
      if(present(avg_L))my_avg_L=avg_L

      ! Set size of the problem
      nk = size(F_hybuser)
      if (size(F_rcoef) /= 2) then
         write(for_msg,*) 'In '//trim(func_name)//' Size of F_rcoef should be 2, it is ',size(F_rcoef)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif

      ! Check monotonicity
      wronghyb = .false.
      do k=2, NK
         if (F_hybuser(k).le.F_hybuser(k-1)) wronghyb = .true.
      enddo
      if (wronghyb) then
         write(for_msg,*)'WRONG SPECIFICATION OF HYB VERTICAL LEVELS'
         write(for_msg,*)'LEVELS MUST BE MONOTONICALLY INCREASING'
         write(for_msg,*)'      FROM HYB(1) ---- ERROR ----'
         write(for_msg,*)'      Current choice:'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         write(for_msg,*)'MOMENTUM LEVELS'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         do k=1, NK
            write(for_msg,*) F_hybuser(k),k
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         end do
         return            
      endif
      
      ! Check and allocate as required
      status = reallocate(F_am_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_bm_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_cm_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_at_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_bt_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_ct_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_ip1_m,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(F_ip1_t,1,nk+2)
      if (status /= VGD_OK) return
      
      ! Allocate working thermo arrays
      status = reallocate(hybm,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(hybt,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(at_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(bt_8,1,nk+2)
      if (status /= VGD_OK) return
      status = reallocate(ip1_t,1,nk+2)
      if (status /= VGD_OK) return
      
      zsrf_8  = log(F_pref_8)
      ! Compute ptop
      zeta1_8 = zsrf_8+log(F_hybuser(1)*1.d0)
      zeta2_8 = zsrf_8+log(F_hybuser(2)*1.d0)
      ztop_8=0.5d0*(3.d0*zeta1_8-zeta2_8)
      l_ptop_8=exp(ztop_8)
      !
      !     Momentum levels
      !
      pr1 = 1.0d0/(zsrf_8 - zeta1_8)
      do k = 1, Nk
         zeta_8  = zsrf_8+log(F_hybuser(k)*1.d0)
         ! Since rcoef may be big we limit lamba_8 to avoid floating point overflow
         lamba_8 = max(lamba_8_ep,(zeta_8- zeta1_8)*pr1)
         rcoefL  = F_rcoef(1) * ( 1.d0 - lamba_8 )
         rcoef   = F_rcoef(2) * ( 1.d0 - lamba_8 )
         F_am_8(k) = zeta_8
         F_bm_8(k) = lamba_8 ** rcoef
         F_cm_8(k) = lamba_8 ** rcoefL - F_bm_8(k)
         ! Since rcoef* may be big we limit B and C to avoid floating point overflow
         if(F_bm_8(k) < 1.e-16)F_bm_8(k) = 0.d0
         if(F_cm_8(k) < 1.e-16)F_cm_8(k) = 0.d0
      enddo
      
      F_am_8(Nk+1) = zsrf_8
      F_bm_8(Nk+1) = 1.d0 
      F_cm_8(Nk+1) = 0.d0 
 
      ! Integrating the hydrostatic eq with T=0C
      ! ln[p(z=dhm)] = ln(ps) - g/(Rd*T)*dhm
      ! s = ln(ps) - ln(pref)
      ! ln[p(z=dhm)] = ln(pref) - g/(Rd*T)*dhm + s
      ! => B=1, A = ln(pref) - g/(Rd*T)*dhm
      ! We take T at 0C
      !F_am_8(Nk+2) = log(F_pref_8) - GRAV*dhm/(RGASD*TCDK)
      F_am_8(Nk+2)  = comp_diag_a(F_pref_8,dhm)
      F_bm_8(Nk+2)  = 1.d0 
      F_cm_8(Nk+2) = 0.d0 
      
      !     Thermodynamic levels
      
      do k = 1, Nk
         F_at_8(k)  = 0.5d0*( F_am_8(k)  + F_am_8(k+1)  ) 
         zeta_8  = F_at_8(k)
         ! Since rcoef may be big we limit lamba_8 to avoid floating point overflow
         if(my_avg_L)then
            F_bt_8(k) = 0.5d0*( F_bm_8(k) + F_bm_8(k+1) ) 
            F_ct_8(k) = 0.5d0*( F_cm_8(k) + F_cm_8(k+1) ) 
         else
            lamba_8 = max(lamba_8_ep,(zeta_8- zeta1_8)*pr1)
            rcoefL  = F_rcoef(1) * ( 1.d0 - lamba_8 )
            rcoef   = F_rcoef(2) * ( 1.d0 - lamba_8 )
            F_bt_8(k) = lamba_8 ** rcoef
            F_ct_8(k) = lamba_8 ** rcoefL - F_bt_8(k)
         endif
         ! Since rcoef* may be big we limit B and C to avoid floating point overflow         
         if(F_bt_8(k) < 1.e-16)F_bt_8(k) = 0.d0
         if(F_ct_8(k) < 1.e-16)F_ct_8(k) = 0.d0
      enddo
      F_at_8(Nk+1) = zsrf_8
      F_bt_8(Nk+1) = 1.d0
      F_ct_8(Nk+1) = 0.d0
      F_at_8(Nk+2) = comp_diag_a(F_pref_8,dht)
      F_bt_8(Nk+2) = 1.d0
      F_ct_8(Nk+2) = 0.d0
      
      hybm(1:NK) = F_hybuser(1:NK)
      do k = 1, NK-1
         hybt(k) = sqrt(hybm(k)*hybm(k+1))
      enddo
      hybt(NK) = sqrt(hybm(NK))
      
      hybt(NK+1) = 1.0
      hybt(NK+2) = 1.0
      hybm(NK+1) = 1.0
      hybm(NK+2) = 1.0
      
      ! Compute ip1 values
      do k=1,nk+1
         call convip(F_ip1_m(k),hybm(k),5,2,'',.false.)
         call convip(F_ip1_t(k),hybt(k),5,2,'',.false.)
      enddo
      
      ! Encoding kind= 4       : M  [metres] (height with respect to ground level)
      call convip(F_ip1_m(nk+2),dhm,4,2,'',.false.)
      call convip(F_ip1_t(nk+2),dht,4,2,'',.false.)
      
      if(present(ptop_out_8))ptop_out_8=l_ptop_8

      deallocate(hybm,hybt,at_8,bt_8,ip1_t)

      F_err = VGD_OK 
    !     __________________________________________________________________
      return
   end subroutine vgrid_genab_5100
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function PRIV_reallocate4(v,low,high) result(status)
    ! Allocate space for outputs
    real, dimension(:), pointer :: v            !vector to allocate
    integer, intent(in) :: low,high             !bounds for vector
    integer :: status                           !return status of function
    
    ! Internal variables
    integer :: check

    ! Deallocate and re-allocate with the correct size
    status = VGD_ERROR
    if (associated(v)) then
       deallocate(v,stat=check)
       if (check /= 0) then
          write(for_msg,*) 'in deallocate() from vdescript_5002::reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'in allocate() from vdescript_5002::reallocate()'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    status = VGD_OK
    return
  end function PRIV_reallocate4

  function PRIV_reallocate8(v,low,high) result(status)
    ! Allocate space for outputs
    real*8, dimension(:), pointer :: v          !vector to allocate
    integer, intent(in) :: low,high             !bounds for vector
    integer :: status                           !return status of function
    
    ! Internal variables
    integer :: check

    ! Deallocate and re-allocate with the correct size
    status = VGD_ERROR
    if (associated(v)) then
       deallocate(v,stat=check)
       if (check /= 0) then
          write(for_msg,*) 'in deallocate() from vgrid_genab_5002::reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'in allocate() from vgrid_genab_5002::reallocate()'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    status = VGD_OK
    return
  end function PRIV_reallocate8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function PRIV_reallocateI(v,low,high) result(status)
    ! Allocate space for outputs
    integer, dimension(:), pointer :: v         !vector to allocate
    integer, intent(in) :: low,high             !bounds for vector
    integer :: status                           !return status of function
    
    ! Internal variables
    integer :: check

    ! Deallocate and re-allocate with the correct size
    status = VGD_ERROR
    if (associated(v)) then
       deallocate(v,stat=check)
       if (check /= 0) then
          write(for_msg,*) 'in deallocate() from vdescript_5002::reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'in allocate() from vdescript_5002::reallocate()'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    status = VGD_OK
    return
  end function PRIV_reallocateI

end module vdescript_5100

