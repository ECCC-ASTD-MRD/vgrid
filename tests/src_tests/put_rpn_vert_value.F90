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
  use vGrid_Descriptors, only: vgd_new,vgd_put,vgd_get,VGD_MISSING
  use Unit_Testing, only: ut_report

  implicit none

  integer :: vgdid
  integer :: stat,lu=0,date,lutxt=69,ip1,ip2
  
  integer, external :: fnom,fstouv,fclos,fstfrm

  ! Open test file
  stat=fnom(lu,"data/dm_5002_from_model_run","RND",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom'
     call abort()
  endif
  stat=fstouv(lu,'RND')
  if(stat.le.0)then
     print*,'No record in RPN file'
     call abort()
  endif
  open(unit=lutxt,file='data/dm_5002_ips.txt',status='OLD')
  read(lutxt,*) ip1,ip2
  close(lutxt)

  ! Construct a new set of 3D coordinate descriptors
  stat = vgd_new(vgdid,unit=lu,format="fst",ip1=ip1,ip2=ip2)

  ! Get information about the coordinate
  stat = vgd_put(vgdid,key='DATE - date stamp',value=2000)
  stat = vgd_get(vgdid,key='DATE - date stamp',value=date)
  call ut_report(date==2000,message='Grid_Descriptors::vgd_get put dimensional entry')

  ! Close files
  stat = fstfrm(lu)
  stat = fclos(lu)

end program tests
