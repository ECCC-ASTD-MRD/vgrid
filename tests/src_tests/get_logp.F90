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
  use Vgrid_Descriptors, only: Vgrid_descriptor,Vgd_free,Vgd_new,Vgd_get,Vgd_print,VGD_OK
  use Unit_Testing, only: ut_report

  implicit none

  type(vgrid_descriptor) :: d,d2
  integer, parameter :: lu=10
  integer :: stat
  integer :: fnom,fstouv,fstfrm,fclos
  logical :: ok,logp_L

  print*,'======================================================'
  print*,'Vcode 1002'
  stat=fnom(lu,"data/dm_1002_from_model_run_plus_toc","RND",0)
  if(stat.lt.0)then
     print*,'ERROR with fnom'
     call abort
  endif
  stat=fstouv(lu,'RND')
  if(stat.le.0)then
     print*,'No record in RPN file'
     call abort
  endif

  stat = vgd_new(d,unit=lu,format="fst",ip1=-1,ip2=-1)

  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_new'
     stat=fstfrm(lu)
     call abort
  endif
  !stat = vgd_print(d)
  stat = vgd_get(d,key='LOGP',value=logp_L)
  ok=.not.logp_L
  print*,'1002 is in log',logp_L    
  
  stat=fstfrm(lu)  
  stat=fclos(lu)

  print*,'======================================================'
  print*,'Vcode 5001'
  stat=fnom(lu,"data/dm_5001_from_model_run_plus_toc","RND",0)
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
  stat = vgd_free(d)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_free'
     stat=fstfrm(lu)
     call abort
  endif
  stat = vgd_new(d,unit=lu,format="fst",ip1=-1,ip2=-1)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_new'
     stat=fstfrm(lu)
     call abort
  endif
  !stat = vgd_print(d)
  stat = vgd_get(d,key='LOGP',value=logp_L)
  ok=.not.logp_L
  print*,'5001 is in log',logp_L    
  
  stat=fstfrm(lu)  
  stat=fclos(lu)

  print*,'======================================================'
  print*,' 5002'
  stat=fnom(lu,"data/dm_5002_from_model_run","RND",0)
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
  stat = vgd_free(d)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_free'
     stat=fstfrm(lu)
     call abort
  endif
  stat = vgd_new(d,unit=lu,format="fst",ip1=-1,ip2=-1)
  if(stat.ne.VGD_OK)then
     print*,'ERROR: problem with vgd_new'
     stat=fstfrm(lu)
     call abort
  endif
  !stat = vgd_print(d)

  stat = vgd_get(d,key='LOGP',value=logp_L)
  ok=logp_L
  print*,'5002 is in log',logp_L
 
  stat=fstfrm(lu)  
  stat=fclos(lu)

  call ut_report(ok,'Grid_Descriptors, vgd_get, LOGP 5002')  

end program constructor
