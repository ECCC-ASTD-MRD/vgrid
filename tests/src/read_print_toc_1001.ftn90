!/* libdescrip - Vertical grid descriptor library for FORTRAN programming
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
program constructor
  use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_print,VGD_OK
  use Unit_Testing, only: ut_report

  implicit none

  type(vgrid_descriptor) :: d
  integer, parameter :: lu=10
  integer :: stat
  integer :: fnom,fstouv,fstfrm
  logical :: ok

  stat=fnom(lu,"data/dm_1001_from_model_run_plus_toc","RND",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom'
     call abort
  endif
  stat=fstouv(lu,'RND')
  if(stat.le.0)then
     print*,'No record in RPN file'
     call abort
  endif

  ! Construct a new set of 3D coordinate descriptors
  stat = vgd_new(d,unit=lu,format="fst")
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_new'
     stat=fstfrm(lu)
     call abort
  endif
  stat = vgd_print(d)

  call ut_report(stat,'Grid_Descriptors, vgd_new, vgd_print 1001')

  stat=fstfrm(lu)

end program constructor
