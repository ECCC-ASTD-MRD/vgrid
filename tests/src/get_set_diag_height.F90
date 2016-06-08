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
  use Vgrid_Descriptors, only: Vgrid_descriptor,Vgd_new,Vgd_get,Vgd_put,Vgd_print,VGD_OK,VGD_ERROR
  use Unit_Testing, only: ut_report

  implicit none

  type(vgrid_descriptor) :: d
  integer, parameter :: lu=10
  integer :: stat,ip1
  integer :: fnom,fstouv,fstfrm,fclos,kind,version
  logical :: ok=.true.
  real :: height
  integer, dimension(:), pointer :: ip1_list

  nullify(ip1_list)

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

  ! Construct a new set of 3D coordinate descriptors
  stat = vgd_new(d,unit=lu,format="fst",ip1=-1,ip2=-1)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_new'
     stat=fstfrm(lu)
     call abort
  endif
  stat = vgd_print(d)
  
  stat = vgd_get(d,key='DHM Diag level Height for Momentum variables',value=height)
  if(stat.ne.VGD_OK)ok=.false.
  print*,'Momentum Diag height=',height,' m AGL'
  stat = vgd_get(d,key='DHT Diag level Height for Thermo variables',value=height)
  if(stat.ne.VGD_OK)ok=.false.
  print*,'Thermo Diag height=',height,' m AGL'

  stat = vgd_get(d,key='DIPM Diag level ip1 for Momentum variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level ip1 for Momentum variables =',ip1
  stat = vgd_get(d,key='DIPT Diag level ip1 for Thermo variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level ip1 for Thermo variables =',ip1

  stat = vgd_get(d,key='VIPM - level ip1 list (m)',value=ip1_list)
  print*,'ip1_list M',ip1_list
  print*,'ANDRE'

  stat = vgd_get(d,key='VIPT - level ip1 list (m)',value=ip1_list)
  print*,'ip1_list T',ip1_list
  print*,'ANDRE'
 
  stat = vgd_put(d,key='DIPM Diag level ip1 for Momentum variables',value=99)
  if(stat==VGD_ERROR)OK=.false.
  stat = vgd_get(d,key='DIPM Diag level ip1 for Momentum variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  if(ip1.ne.99)then
     print*,'Problem with vgd_put on DIPM, put 99 got ',ip1
  endif

  stat = vgd_put(d,key='DIPT Diag level ip1 for Thermo variables',value=99)
  if(stat==VGD_ERROR)OK=.false.
  stat = vgd_get(d,key='DIPT Diag level ip1 for Thermo variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  if(ip1.ne.99)then
     print*,'Problem with vgd_put on DIPT, put 99 got ',ip1
  endif
  
  stat=fstfrm(lu)  
  stat=fclos(lu)

  call ut_report(ok,'Grid_Descriptors, vgd_get on DHM DHT')  

  stat=fstfrm(lu)

end program constructor
