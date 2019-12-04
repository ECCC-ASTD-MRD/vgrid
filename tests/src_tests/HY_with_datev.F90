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
program constructor

  ! Object: construct vgrid object

  use vGrid_Descriptors, only: vgd_new, VGD_OK
  use Unit_Testing, only: ut_report

  implicit none

  integer :: vgdid
  integer :: lu=10,lutxt=69
  integer :: stat,ip1,ip2
  integer :: fnom,fstouv,fstfrm
  logical :: ok

  stat=fnom(lu,"data/sample_out_lewis_small.fstd","RND",0)
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
  print*,'The following error on optional ip2 is expected'
  stat = vgd_new(vgdid,lu,"fst")
  
  call ut_report(stat,'Grid_Descriptors, vgd_new, vgd_get CA')

  stat=fstfrm(lu)

end program constructor
