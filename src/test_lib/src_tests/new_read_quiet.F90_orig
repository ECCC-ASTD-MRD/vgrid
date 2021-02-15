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
!================================================================================================
program tests
#include <msg.h>
  use vGrid_Descriptors, only: vgrid_descriptor, vgd_free, vgd_new,vgd_levels
  use Unit_Testing, only: ut_report


  implicit none

  integer :: lu=10,fnom,fstouv,fclos,ier,k
  integer, parameter :: nfiles=5
  ! Test files thar generate lots of error messages
  character(len=200), dimension(nfiles) :: files=(/&
       "data/dm_all_sorts_of_toc",&
       "dm_2001_5001_from_editfst",&
       "dm_2001_from_editfst",&
       "dm_1001_just_p0",&
       "dm_1001_from_model_run"/)
  logical :: ok = .false.
  type(vgrid_descriptor) :: vgd
  
  call msg_verbosity(MSG_DEBUG)

  do k=1,nfiles

     lu = lu+1
     ier=fnom(lu,files(1),"RND+R/O",0)
     if(ier.lt.0)then
        print*,'(Test) ERROR with fnom on file ',files(k)
        call exit(1)
     endif
     ier=fstouv(lu,'RND')
     if(ier.lt.0)then
        print*,'(Test) No record in RPN file ',files(k)
        call exit(1)
     endif
     
     ! Suppress (Cvgd) and (vgd) messages for vgd_new
     
     ier = vgd_new(vgd,unit=lu,format="fst",ip1=-1,ip2=-1, quiet=.true.)

     ier = vgd_free(vgd)
     
     ier=fclos(lu)

  end do

  ok = .true.
  
  call ut_report(ok,message='Grid_Descriptors::vgd_levels level calculation status')
     
end program tests
