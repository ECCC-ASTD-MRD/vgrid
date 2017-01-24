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
program gen_5999

  use vGrid_Descriptors, only: vgrid_descriptor,vgd_new,vgd_print,VGD_OK

  implicit none

  type(vgrid_descriptor) :: vgd
  integer :: stat,k
  integer, parameter :: nk=28
  integer, dimension(nk) :: ip1_m
  real :: ff
  ! The a_m_8, b_m_8 and ip1_m will normally be obtain with a read statement  
  real*8, dimension(nk) :: a_m_8 = (/&
       1000.000014901161,         1810.208037495613,         2885.672841221094,&
       4344.769790768623,         5656.799376010895,         6937.218904495239,&     
       8087.364435195923,         9193.436205387115,         10237.67769336700,&     
       11257.31885433197,         12226.51124000549,         13079.19859886169,&     
       13746.60611152649,         14157.72438049316,         14245.82004547119,&     
       14009.74750518799,         13434.39579010010,         12554.45957183838,&     
       11374.40204620361,         9932.713508605957,         8351.030349731445,&      
       6761.975288391113,         5158.953666687012,         3585.867881774902,&     
       2126.789093017578,         964.5986557006836,         341.1388397216797,&
       0.000000000000000 /)
   real*8, dimension(nk) :: b_m_8 = (/& 
        0.000000000000000,        7.3490012437105179E-004,   3.0915911775082350E-003,&
        8.5528781637549400E-003,   1.5852507203817368E-002,   2.5521762669086456E-002,& 
        3.6819949746131897E-002,   5.0644051283597946E-002,   6.7216031253337860E-002,& 
        8.8046513497829437E-002,   0.1144436150789261,        0.1472350209951401,&     
        0.1872804313898087,        0.2354654371738434,        0.2886772453784943,&      
        0.3469281792640686,        0.4113950729370117,        0.4786812663078308,&     
        0.5497199892997742,        0.6230410933494568,        0.6941621303558350,&     
        0.7594503164291382,        0.8209630846977234,        0.8781516551971436,&      
        0.9289771318435669,        0.9681925177574158,        0.9888227581977844,&     
        1.000000000000000 /)  
   character(len=1) :: dumc_S

   do k=1,nk
      ! ip1_m(k) may contain any valid kind 5 ip1, every ip1_m values must be unique.
      ! Here we take b_m_8 encoded in ip1 kind 5
      ff=b_m_8(k)
      call convip(ip1_m(k),ff,5, 2,dumc_S,.false.)
   end do

   stat=vgd_new(vgd,kind=5,version=999,nk=nk,ip1=1,ip2=2,&
       a_m_8=a_m_8,b_m_8=b_m_8,ip1_m=ip1_m)
   
   if ( stat /= VGD_OK )then
      print*,'ERROR'
      call exit(1)
   endif
   
   stat = vgd_print(vgd,6)
   
end program gen_5999
