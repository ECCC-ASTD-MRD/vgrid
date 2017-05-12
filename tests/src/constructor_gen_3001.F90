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

  ! Revision : Andre Plante test on B instead of A since A not sensitive to rcoefs

  use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_get,vgd_print,VGD_ERROR
  use Unit_Testing, only: ut_report

  implicit none

  type(vgrid_descriptor) :: d
  integer :: stat,test_sleve
  real, dimension(100) :: hgts= &
   (/995., 985., 975., 965., 955., 945., 935., 925., 915., 905.,&
     895., 885., 875., 865., 855., 845., 835., 825., 815., 805.,&
     795., 785., 775., 765., 755., 745., 735., 725., 715., 705.,&
     695., 685., 675., 665., 655., 645., 635., 625., 615., 605.,&
     595., 585., 575., 565., 555., 545., 535., 525., 515., 505.,&
     495., 485., 475., 465., 455., 445., 435., 425., 415., 405.,&
     395., 385., 375., 365., 355., 345., 335., 325., 315., 305.,&
     295., 285., 275., 265., 255., 245., 235., 225., 215., 205.,&
     195., 185., 175., 165., 155., 145., 135., 125., 115., 105.,&
      95.,  85.,  75.,  65.,  55.,  45.,  35.,  25.,  15.,   5./)

  real, dimension(1) :: hyb_wrong1= &
       (/1.1/)
  real, dimension(1) :: hyb_wrong2= &
       (/1.e-6/)
  real, dimension(3) :: hyb_wrong3= &
       (/.9,.5,.1/)
  real :: rcoef1=4.,rcoef2=100.
  
  logical :: OK=.true.
  logical, parameter :: write_control_L=.true.
  character (len=256) :: file

  ! Construct a new set of vertical coordinate descriptors 3001 Gal-Chen
  if( vgd_new(d,kind=3,version=1,hyb=hgts,rcoef1=rcoef1,rcoef2=rcoef2,dhm=10.0,dht=1.5) == VGD_ERROR )OK=.false.
  if( vgd_print(d,convip_L=.true.) == VGD_ERROR )then
     print*,'ERROR'
     stop
  endif
  if( vgd_print(3001) == VGD_ERROR )then
     print*,'ERROR'
     stop
  endif

stop

  file='data/data_constructor_gen_3001.txt'
  stat = test_sleve(d,file,write_control_L,stat)
  if(stat.eq.VGD_ERROR)OK=.false.
  
  call ut_report(OK,'Grid_Descriptors::vgd_new vertical generate initializer SLEVE like coordinate')
end program constructor
!==============================================================================
!===============================================================================
!===============================================================================
integer function test_sleve(F_d,F_file,F_write_control_L,F_stat) result(istat)
   !
   use vGrid_Descriptors, only: vgrid_descriptor,vgd_get,VGD_ERROR,VGD_OK
   implicit none
   !
   type (vgrid_descriptor) :: F_d
   logical :: F_write_control_L
   character (len=256) :: F_file
   integer :: F_stat
   !
   ! Local variable
   !
   real, dimension(:), pointer :: vcdm,vcdt,work
   real*8, dimension(:), pointer :: b_m_8,c_m_8,a_m_8,b_t_8,c_t_8,a_t_8,work_8
   integer, dimension(:), pointer :: vipm,vipt,work_i
   integer :: nl_m,nl_t,k,nk,stat,kind,vers,vcode,ip1,my_ip1

   nullify(vcdm,vcdt,work,a_m_8,b_m_8,c_m_8,a_t_8,b_t_8,c_t_8,work_8,vipm,vipt,work_i)

   istat=VGD_OK

   call flush(6)

   if(F_stat.eq.VGD_ERROR)then
      print*,'In test_5002: test not performed, since previous command failed'
      istat=VGD_ERROR
      return
   endif

   if( vgd_get(F_d,key='KIND - vertical coordinate ip1 kind' ,value=kind)   == VGD_ERROR) return
   if( vgd_get(F_d,key='VERS - vertical coordinate version'  ,value=vers)   == VGD_ERROR) return
   if( vgd_get(F_d,key='CA_M - vertical A coefficient (m)'   ,value=a_m_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='CA_T - vertical A coefficient (t)'   ,value=a_t_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='CB_M - vertical B coefficient (m)'   ,value=b_m_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='CC_M - vertical C coefficient (m)'   ,value=c_m_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='CB_T - vertical B coefficient (t)'   ,value=b_t_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='CC_T - vertical C coefficient (t)'   ,value=c_t_8)  == VGD_ERROR) return
   if( vgd_get(F_d,key='VIPM - level ip1 list (m)'           ,value=vipm)   == VGD_ERROR) return
   if( vgd_get(F_d,key='VIPT - level ip1 list (t)'           ,value=vipt)   == VGD_ERROR) return
   if( vgd_get(F_d,key='VCDM - vertical coordinate (m)'      ,value=vcdm)   == VGD_ERROR) return
   if( vgd_get(F_d,key='VCDT - vertical coordinate (t)'      ,value=vcdt)   == VGD_ERROR) return
   if( vgd_get(F_d,key='NL_M - Number of vertical levels (m)',value=nl_m)   == VGD_ERROR) return
   if( vgd_get(F_d,key='NL_T - Number of vertical levels (t)',value=nl_t)   == VGD_ERROR) return
   if( vgd_get(F_d,key='IP_1 - record ip1'                   ,value=ip1)    == VGD_ERROR) return
   print*,'TESTING ',kind*1000+vers

   if(F_write_control_L)then
     open(unit=10,file=F_file)
     write(10,*)a_m_8
     write(10,*)a_t_8
     write(10,*)b_m_8
     write(10,*)b_t_8
     write(10,*)c_m_8
     write(10,*)c_t_8
     write(10,*)vipm
     write(10,*)vipt
     write(10,*)vcdm
     write(10,*)vcdt
     write(10,*)ip1
     close(10)
  endif

  if(nl_m.ne.size(b_m_8))then
     print*,'wrong size with NL_M, got',nl_m,' should be ',size(b_m_8)
     istat=VGD_ERROR
  endif
  if(nl_t.ne.size(b_t_8))then
     print*,'wrong size with NL_T, got',nl_t,' should be ',size(b_t_8)
     istat=VGD_ERROR
  endif

  nk=nl_m

  ! Check A
  open(unit=10,file=F_file,ACTION='READ')
  allocate(work_8(size(a_m_8)))
  print*,'Reading A M'
  read(10,*)work_8

  do k=1,nk
     if(a_m_8(k).eq.0.)then
        if(abs(work_8(k)-a_m_8(k))>100.*epsilon(a_m_8(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec A M, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,a_m_8(k)
        endif
     else
        if(abs(work_8(k)-a_m_8(k))/a_m_8(k)>100.*epsilon(a_m_8(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec A M, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,a_m_8(k)
        endif
     endif
  enddo

  print*,'Reading A T'
  read(10,*)work_8
  do k=1,nk
     if(a_t_8(k).eq.0.)then
        if(abs(work_8(k)-a_t_8(k))>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec A T, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,a_t_8(k)
        endif
     else
        if(abs(work_8(k)-a_t_8(k))/a_t_8(k)>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec A T, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,a_t_8(k)
        endif
     endif
  enddo
 
  ! Check B
  print*,'Reading B M'
  read(10,*)work_8
  do k=1,nk
     if(b_m_8(k).eq.0.)then
        if(work_8(k).ne.0.)then
           istat=VGD_ERROR
           print*,'Probleme avec B M, pas egal a zero ',work_8(k)
        endif
     else
        if(abs(work_8(k)-b_m_8(k))/b_m_8(k)>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec B M, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,b_m_8(k)
        endif
     endif
  enddo
  print*,'Reading B T'
  read(10,*)work_8
  do k=1,nk
     if(b_t_8(k).eq.0.)then
        if(work_8(k).ne.0.)then
           istat=VGD_ERROR
           print*,'Probleme avec B T, pas egal a zero ',work_8(k)
        endif
     else
        if(abs(work_8(k)-b_t_8(k))/b_t_8(k)>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec B T, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,b_t_8(k)
        endif
     endif
  enddo
  
  ! Check C
  print*,'Reading C M'
  read(10,*)work_8
  do k=1,nk
     if(c_m_8(k).eq.0.)then
        if(work_8(k).ne.0.)then
           istat=VGD_ERROR
           print*,'Probleme avec C M, pas egal a zero ',work_8(k)
        endif
     else
        if(abs(work_8(k)-c_m_8(k))/c_m_8(k)>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec C M, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,c_m_8(k)
        endif
     endif
  enddo
  print*,'Reading C T'
  read(10,*)work_8
  do k=1,nk
     if(c_t_8(k).eq.0.)then
        if(work_8(k).ne.0.)then
           istat=VGD_ERROR
           print*,'Probleme avec C t, pas egal a zero ',work_8(k)
        endif
     else 
        if(abs(work_8(k)-c_t_8(k))/c_t_8(k)>100.*epsilon(1.))then
           istat=VGD_ERROR
           print*,'Probleme avec C T, pas dans les limites tollerees'
           print*,work_8(k),'vs'
           print*,c_t_8(k)
        endif
     endif
  enddo

  ! Check IPs
  allocate(work_i(size(vipm)))
  print*,'Reading IP M'
  read(10,*)work_i
  do k=1,nk
     if(work_i(k).ne.vipm(k))then
        istat=VGD_ERROR
        print*,'Probleme avec IP:'
        print*,work_i(k),'vs'
        print*,vipm(k)
     endif
  enddo
  print*,'Reading IP T'
  read(10,*)work_i
  do k=1,nk
     if(work_i(k).ne.vipt(k))then
        istat=VGD_ERROR
        print*,'Probleme avec IP:'
        print*,work_i(k),'vs'
        print*,vipt(k)
     endif
  enddo

  ! Check vcdm
  allocate(work(size(vcdm)))
  print*,'Reading VCDM'
  read(10,*)work
  do k=1,nk
     if(vcdm(k).eq.0.)then
        if(abs(work(k)-vcdm(k))>100.*epsilon(vcdm(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec vcdm, pas dans les limites tollerees'
           print*,work(k),'vs'
           print*,vcdm(k)
        endif
     else
        if(abs(work(k)-vcdm(k))/vcdm(k)>100.*epsilon(vcdm(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec vcdm, pas dans les limites tollerees'
           print*,work(k),'vs'
           print*,vcdm(k)
        endif
     endif
  enddo

  print*,'Reading VCDT'
  read(10,*)work
  do k=1,nk
     if(vcdt(k).eq.0.)then
        if(abs(work(k)-vcdt(k))>100.*epsilon(vcdt(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec vcdm, pas dans les limites tollerees'
           print*,work(k),'vs'
           print*,vcdt(k)
        endif
     else
        if(abs(work(k)-vcdt(k))/vcdt(k)>100.*epsilon(vcdt(k)))then
           istat=VGD_ERROR
           print*,'Probleme avec vcdt, pas dans les limites tollerees'
           print*,work(k),'vs'
           print*,vcdt(k)
        endif
     endif
  enddo  

  print*,'Reading VCDT'
  read(10,*)my_ip1
  if(my_ip1.ne.ip1)then
      istat=VGD_ERROR
     print*,'Probleme avec ip1, expected ',ip1,' got',my_ip1     
  endif

  close(10)

  deallocate(vcdm,vcdt,work,a_m_8,b_m_8,c_m_8,a_t_8,b_t_8,c_t_8,work_8,vipm,vipt,work_i)

end function test_sleve
