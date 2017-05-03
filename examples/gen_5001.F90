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
program gen_5001
   
   use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_print,vgd_write,VGD_OK
   
   implicit none
   
   type(vgrid_descriptor) :: vgd
   integer :: stat,k, fnom, fstouv,fstfrm
   integer, parameter :: nk=80, lu=10
   integer, dimension(nk) :: ip1_m
   real :: rcoef
   real*8 :: ptop_8,pref_8=80000.d0
   ! The hyb, pref will normally be obtain with a read statement  
   real, dimension(nk) :: hyb = (/&
        0.125E-03, 0.221E-03, 0.382E-03, 0.635E-03, 0.101E-02, 0.153E-02, 0.224E-02, 0.317E-02, &
        0.433E-02, 0.577E-02, 0.750E-02, 0.955E-02, 0.119E-01, 0.146E-01, 0.177E-01, 0.210E-01, &
        0.247E-01, 0.287E-01, 0.330E-01, 0.375E-01, 0.422E-01, 0.472E-01, 0.524E-01, 0.576E-01, &
        0.630E-01, 0.684E-01, 0.738E-01, 0.791E-01, 0.843E-01, 0.893E-01, 0.942E-01, 0.988E-01, &
        0.103E+00, 0.108E+00, 0.113E+00, 0.119E+00, 0.126E+00, 0.134E+00, 0.142E+00, 0.152E+00, &
        0.163E+00, 0.175E+00, 0.189E+00, 0.206E+00, 0.223E+00, 0.241E+00, 0.260E+00, 0.279E+00, &
        0.300E+00, 0.321E+00, 0.344E+00, 0.367E+00, 0.390E+00, 0.415E+00, 0.440E+00, 0.467E+00, &
        0.493E+00, 0.521E+00, 0.549E+00, 0.578E+00, 0.607E+00, 0.637E+00, 0.668E+00, 0.699E+00, &
        0.730E+00, 0.759E+00, 0.787E+00, 0.812E+00, 0.836E+00, 0.859E+00, 0.879E+00, 0.898E+00, &
        0.916E+00, 0.932E+00, 0.947E+00, 0.961E+00, 0.974E+00, 0.985E+00, 0.995E+00, 0.100E+01 &
        /)
   
   character(len=1) :: dumc_S
   
   ptop_8=hyb(1)*pref_8
   print*,'ptop_8=',ptop_8
   rcoef=1.6

   do k=1,nk
      call convip(ip1_m(k),hyb,5, 2,dumc_S,.false.)
   end do
   
   stat=vgd_new(vgd,kind=5,version=1,hyb=hyb,rcoef1=rcoef,ptop_8=ptop_8,pref_8=pref_8)
   
   if ( stat /= VGD_OK )then
      print*,'ERROR 1'
      call exit(1)
   endif
   
   stat = vgd_print(vgd,6)
   
   stat=fnom(lu,"toc_5001.fst","RND",0)
   if(stat.lt.0)then
      print*,'ERROR on fnom with toc_5001.fst'
      call abort
   endif
   stat=fstouv(lu,'RND')   
   if(stat.lt.0)then
      print*,'ERROR on fstouv with toc_5001.fst'
      call abort
   endif
   stat=vgd_write(vgd,lu,"fst")
   if ( stat /= VGD_OK )then
      print*,'ERROR 2'
      call exit(1)
   endif
   stat=fstfrm(lu)

end program gen_5001
