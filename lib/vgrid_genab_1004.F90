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
module vdescript_1004

  implicit none
  private

  ! Define class variables
#include "vgrid.hf"

  ! Define and publicize interfaces
  public :: vgrid_genab_1004
  interface reallocate
     module procedure PRIV_reallocate4
     module procedure PRIV_reallocate8
     module procedure PRIV_reallocateI
  end interface

contains

   subroutine vgrid_genab_1004 (F_etasef,F_etatop,F_ptop,F_a_8,F_b_8,F_err)

      !    Using etasef coordinate generate A=0 and B=convip(...,F_etasef,...)
      
      implicit none
      !      
      ! Arguments
      integer, intent(out) :: F_err
      real, intent(in) :: F_etatop               ! value from E1
      real, intent(in) :: F_ptop                 ! value from PT
      real, dimension(:), intent(in) :: F_etasef    ! user specification for vertical layering etasef
      real*8, dimension(:), pointer :: F_a_8, F_b_8 ! model As and Bs
      
      ! Local variables
      real    :: eta1
      integer :: k, status, nk
      logical :: monotone=.true.,wrongsig=.false.

      !     __________________________________________________________________
      
      F_err = VGD_ERROR

      ! Set size of the problem
      nk = size(F_etasef)
      
      ! Check and allocate as required
      status = reallocate(F_a_8,1,nk)
      if (status /= VGD_OK) return
      status = reallocate(F_b_8,1,nk)
      if (status /= VGD_OK) return
      !
      if(F_etasef(nk).ne.1.d0)then
         wrongsig = .true.
         write(for_msg,1000)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
      endif
      do k=2, nk
         if (F_etasef(k).le.F_etasef(k-1))monotone = .false.
      end do
      if(.not.monotone)then
         wrongsig = .true.
         write(for_msg,1004)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
      endif
      if (wrongsig) then
         write(for_msg,1002)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         do k=1, nk
            write (for_msg,*) F_etasef(k),k
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
         end do
         return
      endif
      !
      eta1 = 1./(1.-F_etatop)
      do k=1,nk
         F_b_8(k)=(F_etasef(k)- F_etatop)*eta1
         F_a_8(k)=F_ptop* (1.0d0 - F_b_8(k))
      end do
      !
      F_err = VGD_OK
      return

1000  format (/' ===> WRONG SPECIFICATION OF etasef VERTICAL LEVELS:'/&
           '      etasef(NK) MUST BE 1.0')
      
1004  format (/' ===> WRONG SPECIFICATION OF etasef VERTICAL LEVELS:'/&
           '      LEVELS MUST BE MONOTONICALLY INCREASING')
1002  format (/'      Current choice:')

   end subroutine vgrid_genab_1004
   
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
          write(for_msg,*) 'in deallocate() from vdescript_1004::reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'in allocate() from vdescript_1004::reallocate()'
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
          write(for_msg,*) 'in deallocate() from vgrid_genab_1004:reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'in allocate() from vgrid_genab_1004::reallocate()'
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
          write(for_msg,*) 'in deallocate() from vdescript_1004::reallocate()'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif
    allocate(v(low:high),stat=check)
    if (check /= 0) then
       write(for_msg,*) 'Error in allocate() from vdescript_1004::reallocate()'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    status = VGD_OK
    return
  end function PRIV_reallocateI

end module vdescript_1004

