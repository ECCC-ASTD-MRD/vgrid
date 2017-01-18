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
  use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_get,vgd_print,VGD_ERROR
  use Unit_Testing, only: ut_report
  !
  implicit none
  !
  type(vgrid_descriptor) :: vgd,vgd2
  integer, parameter :: nk=9 ! including diag level
  integer :: stat,ip1
  integer, dimension(:), pointer :: ip1_m,ip1_t
  real*8, dimension(:), pointer :: a_m_8,b_m_8,a_t_8,b_t_8
  real :: height
  logical :: OK=.true.

  nullify(ip1_m,ip1_t,a_m_8,b_m_8,a_t_8,b_t_8)

  allocate(ip1_m(nk),ip1_t(nk),a_m_8(nk),b_m_8(nk),a_t_8(nk),b_t_8(nk))

  ip1_m=(/97618238,96758972,95798406,94560550,94831790,95102940,95299540,93423264,75597472/)
  a_m_8=(/2.30926271551059,5.66981194184163,8.23745285281583,9.84538165280926,10.7362879740149,11.1997204664634,11.4378785724517,11.51293,11.5116748020711/)
  b_m_8=(/0.000000000000000E+000,1.154429569962798E-003,0.157422392639441,0.591052504380263,0.856321652104870,0.955780377300956,0.991250207889939,1.00000000000000,1.00000000000000/)

  ip1_t=(/97698159,96939212,95939513,94597899,94877531,95139482,95323042,93423264,76746048/)
  a_t_8=(/2.89364884405945,6.15320066567627,8.55467550398551,10.0259661797048,10.8310952652232,11.2484934057893,11.4628969443959,11.51293,11.5126753323904/)
  b_t_8=(/5.767296480554498E-009,7.010292926951782E-003,0.227561997481228,0.648350006620964,0.878891216792279,0.963738779730914,0.994233214440677,1.00000000000000,1.00000000000000/)
  
  stat=vgd_new(vgd,kind=5,version=5,nk=nk-2,ip1=1,ip2=2,&
       pref_8=100000.d0,&
       rcoef1=1.,rcoef2=10.,&
       a_m_8=a_m_8,b_m_8=b_m_8,&
       a_t_8=a_t_8,b_t_8=b_t_8, &
       ip1_m=ip1_m,ip1_t=ip1_t)
  if(stat==VGD_ERROR)OK=.false.

  stat=vgd_print(vgd)
  if(stat==VGD_ERROR)OK=.false.

  stat = vgd_get(vgd,key='DHM Diag level Height for Momentum variables',value=height)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level Height for Momentum variables =',height

  stat = vgd_get(vgd,key='DHT Diag level Height for Thermo variables',value=height)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level Height for Thermo variables =',height
   
  stat = vgd_get(vgd,key='DIPM Diag level ip1 for Momentum variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level ip1 for Momentum variables =',ip1

  stat = vgd_get(vgd,key='DIPT Diag level ip1 for Thermo variables',value=ip1)
  if(stat==VGD_ERROR)OK=.false.
  print*,'Diag level ip1 for Thermo variables =',ip1
  !
  call ut_report(OK,'Grid_Descriptors:: vgd_new new_gen')
  !
end program constructor
