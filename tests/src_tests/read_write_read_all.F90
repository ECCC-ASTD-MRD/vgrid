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
  use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_print,vgd_write,VGD_OK,operator(==),vgd_print, vgd_put, vgd_free
  use Unit_Testing, only: ut_report

  ! TODO: add other vcode than 5005

  implicit none

  type(vgrid_descriptor) :: d,d2
  integer, parameter :: lu=10,lu2=20,lutxt=69
  integer :: stat,ip1,ip2
  integer :: fnom,fstouv,fstfrm,fclos
  logical :: ok=.true.

  stat=fnom(lu,"data/dm_5005_from_model_run","RND",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom'
     call abort
  endif
  stat=fstouv(lu,'RND')
  if(stat.le.0)then
     print*,'No record in RPN file'
     call abort
  endif
  open(unit=lutxt,file='data/dm_5005_ips.txt',status='OLD')
  read(lutxt,*) ip1,ip2
  close(lutxt)

  stat=fnom(lu2,"data/toc_toc.rpn","RND",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom toc toc'
     call abort
  endif
  stat=fstouv(lu2,'RND')

  ! Construct a new set of 3D coordinate descriptors
  stat = vgd_new(d,unit=lu,format="fst",ip1=ip1,ip2=ip2)
  if(stat/=VGD_OK)ok=.false.
  stat = vgd_print(d)

  stat = vgd_put(d,key='DIPM Diag level ip1 for Momentum variables',value=75697472)
  stat = vgd_put(d,key='DIPT Diag level ip1 for Thermo variables',value=76946048)
  stat = vgd_print(d)

  stat = vgd_write(d,unit=lu2,format="fst")
  if(stat/=VGD_OK)ok=.false.

  stat=fstfrm(lu2)
  stat=fclos(lu2)
  stat=fnom(lu2,"data/toc_toc.rpn","RND",0)
  stat=fstouv(lu2,'RND')

  stat = vgd_new(d2,unit=lu2,format="fst")
  if(stat/=VGD_OK)ok=.false.

  if (.not. (d2 == d) )ok=.false.

  stat=vgd_print(d2)

  stat = vgd_free(d)
  stat = vgd_free(d2)

  stat=fstfrm(lu)
  stat=fstfrm(lu2)

  call ut_report(stat,'Grid_Descriptors, vgd_new, vgd_get CA')
  

end program constructor
