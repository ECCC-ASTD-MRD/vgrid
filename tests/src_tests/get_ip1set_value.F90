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
  use vGrid_Descriptors, only: vgd_new,vgd_get,VGD_ERROR
  use Unit_Testing, only: ut_report

  implicit none

  integer :: vgdid
  integer :: stat,i,ip1
  integer, dimension(:), allocatable :: ip1s
  real(kind=8), dimension(4) :: pres=(/1000.,925.,850.,700./),b=(/0.,0.,0.,0./)

  ! Construct a new set of 3D coordinate descriptors
  pres = pres*100. !convert mb to Pa
  allocate(ip1s(size(pres)))
  do i=1,size(ip1s)
     call convip(ip1s(i),pres(i),2,2,'',.false.)
  enddo
  stat = vgd_new(vgdid,kind=2,version=1,nk=size(pres),ip1=999,ip1_m=ip1s,a_m_8=pres,b_m_8=b)
  stat = vgd_get(vgdid,key='IP_1',value=ip1)
  call ut_report(ip1==999,'Grid_Descriptors::vgd_new vertical build initializer (2001) value')

end program constructor
