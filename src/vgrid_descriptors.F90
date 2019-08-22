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

module vGrid_Descriptors

   ! Autor of C version : Andre Plante
   !                      Based on fortran version from:
   !                      Ron MacTaggart-Cowan, Andre Plante and Cecilien Charette

   ! Naming convention
   !
   ! var    may y a float or an integer
   ! var_8  is of type real(kind=8)
   ! var_L  is of type logical
   ! var_CP is of type type(c_ptr) from iso_c_binding

   use iso_c_binding, only : c_ptr, C_NULL_PTR, C_CHAR, C_NULL_CHAR, c_int, C_FLOAT, c_associated, c_loc

   implicit none
   private

   ! Public methods
   public :: vgrid_descriptor                    !vertical grid descriptor structure
   public :: vgd_new                             !class constructor

   ! Public class constants
#include "vgrid_descriptors.hf"

   ! Private class variables
   logical :: ALLOW_RESHAPE=.false.              ! Allow reshape of class pointer members
   integer, parameter :: KEY_LENGTH=4            !length of key string considered for get/put operations
   character(len=1), dimension(3), parameter :: MATCH_GRTYP=(/'X','Y','Z'/) !grid types with ip1,2 to ig1,2 mapping
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_T") :: &
        VGD_STDA76_SFC_T = 288.15
   real(C_FLOAT), public, protected, bind(C, name="VGD_STDA76_SFC_P") :: &
        VGD_STDA76_SFC_P = 101325

   type :: vgrid_descriptor
      ! The only member of this type is a C pointer
      type(c_ptr) :: cptr = C_NULL_PTR
   end type vgrid_descriptor
   
   interface

      integer(c_int) function f_new_read(vgd,unit,ip1,ip2,kind,version) bind(c, name='Cvgd_new_read')
         use iso_c_binding, only : c_ptr, c_int, c_char
         type(c_ptr) :: vgd
         integer (c_int), value :: unit, ip1, ip2, kind, version
      end function f_new_read
      
      integer(c_int) function f_new_from_table(vgd, table_CP, ni, nj, nk) bind(c, name='Cvgd_new_from_table')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr) :: vgd
         type(c_ptr), value :: table_CP
         integer (c_int), value :: ni, nj, nk
      end function f_new_from_table
      
      integer(c_int) function f_new_build_vert(vgd,kind,version,nk,ip1,ip2, &
           ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, &
           a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP, nl_m, nl_t, nl_w) bind(c, name='Cvgd_new_build_vert2')
         use iso_c_binding, only : c_ptr, c_int
         type(c_ptr) :: vgd
         integer (c_int), value :: kind,version,nk,ip1,ip2
         type(c_ptr), value :: ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP
         type(c_ptr), value :: a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP
         integer (c_int), value :: nl_m, nl_t, nl_w
      end function f_new_build_vert

   end interface
   
   interface vgd_new
      module procedure new_read
      module procedure new_build_vert
   end interface vgd_new
   
contains
   
   integer function new_read(self,unit,format,ip1,ip2,kind,version) result(status)
      use vgrid_utils, only: up
      ! Coordinate constructor - read from a file and initialize instance
      type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance
      integer, intent(in) :: unit                 !File unit to read descriptor information from
      character(len=*), target, optional, intent(in) :: format !File format ('fst' or 'bin') default is 'fst'
      integer, target,optional :: ip1,ip2                 !ip1,2 values of the desired descriptors
      integer, target,optional, intent(in) :: kind        ! Level kind requested by user.
      integer, target,optional, intent(in) :: version     ! Level version requested by user.
      
      ! Local variables
      integer :: ni,nj,nk, istat, error, l_ip1, l_ip2, l_kind, l_version
      character(len=100) :: myformat
      real(kind=8), dimension(:,:,:), pointer :: table_8

      nullify(table_8)

      status = VGD_ERROR

      myformat='FST'
      if (present(format)) myformat = trim(up(format))
      select case (trim(up(myformat)))
      case ('FST')      
         if(present(ip1))then
            l_ip1 = ip1
         else
            l_ip1 = -1
         endif
         if(present(ip2))then
            l_ip2 = ip2
         else
            l_ip2 = -1
         endif
         if(present(kind))then
            l_kind = kind
         else
            l_kind = -1
         endif
         if(present(version))then
            l_version = version
         else
            l_version = -1
         endif
         
         if( f_new_read(self%cptr, unit, l_ip1, l_ip2, l_kind, l_version) == VGD_ERROR )then
            print*,'(F_vgd) ERROR: In new_read, problem with f_new_read'
            return
         endif
      case ('BIN')
         read(unit) ni,nj,nk
         allocate(table_8(ni,nj,nk),stat=istat)
         if (istat /= 0) then
            write(for_msg,*) 'unable to allocate table_8'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         read(unit) table_8
         print*,'table_8(1:3,1,1)',table_8(1:3,1,1)
         
         error = new_from_table(self,table_8)
         if (error < 0) then
            write(for_msg,*) 'In new_read, problem creating record information'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         ! Warn user on invalid input format specification
      case DEFAULT
         write(for_msg,*) 'invalid constructor format request ',trim(myformat)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select

      status = VGD_OK

   end function new_read

    integer function new_from_table(self,table) result(status)
       ! Coordinate constructor - build vertical descriptor from table input
       ! Set internal vcode (if all above was successful)
       type(vgrid_descriptor), target, intent(inout) :: self !Vertical descriptor instance    
       real(kind=8), dimension(:,:,:), pointer :: table   !Raw table of vgrid records

       ! Local variables
       type(c_ptr) :: table_CP
       table_CP = c_loc(table(1,1,1))
       
       status = VGD_ERROR
       
       if ( f_new_from_table(self%cptr, table_CP, size(table,dim=1), size(table,dim=2), size(table,dim=3)) == VGD_ERROR )then
         print*,'(F_vgd) ERROR: in new_from_table, problem with f_new_from_table'
         return
      endif      
      
      status = VGD_OK
      
    end function new_from_table

   integer function new_build_vert(self,kind,version,nk,ip1,ip2, &
        ptop_8,pref_8,rcoef1,rcoef2,rcoef3,rcoef4,a_m_8,b_m_8,a_t_8,b_t_8, &
        ip1_m,ip1_t,c_m_8,c_t_8,a_w_8,b_w_8,c_w_8,ip1_w) result(status)
      ! Coordinate constructor - build vertical descriptor from arguments
      type(vgrid_descriptor) :: self                    !Vertical descriptor instance    
      integer, intent(in) :: kind,version               !Kind,version to create
      integer, intent(in) :: nk                         !Number of levels
      integer,target , optional, intent(in) :: ip1,ip2          !IP1,2 values for FST file record [0,0]
      real,target , optional, intent(in) :: rcoef1,rcoef2,rcoef3,rcoef4 !R-coefficient values for rectification
      real(kind=8),target , optional, intent(in) :: ptop_8            !Top-level pressure (Pa)
      real(kind=8),target , optional, intent(in) :: pref_8            !Reference-level pressure (Pa)
      real(kind=8),target , optional, dimension(:) :: a_m_8,a_t_8,a_w_8 !A-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      real(kind=8),target , optional, dimension(:) :: b_m_8,b_t_8,b_w_8 !B-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      real(kind=8),target , optional, dimension(:) :: c_m_8,c_t_8,c_w_8 !C-coefficients for momentum(m),thermo(t) and Vertical-Velocity levels
      integer,target , optional, dimension(:) :: ip1_m,ip1_t,ip1_w !Level ID (IP1) for momentum(m),thermo(t) and Vertical-Velocity levels

      ! Assign optional argument to C_NULL_PTR    

      ! Local variables
      type(c_ptr) :: rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, ptop_8_CP, pref_8_CP
      type(c_ptr) :: a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, ip1_m_CP, ip1_t_CP, ip1_w_CP
      integer l_ip1,l_ip2,nl_m, nl_t, nl_w

      status = VGD_ERROR

      nl_m=nk
      nl_t=-1
      nl_w=-1

      if(present(ip1))then
         l_ip1 = ip1
      else
         l_ip1 = -1
      endif
      if(present(ip2))then
         l_ip2 = ip2
      else
         l_ip2 = -1
      endif
      if(present(ptop_8))then
         ptop_8_CP = c_loc(ptop_8)
      else
         ptop_8_CP = C_NULL_PTR
      endif
      if(present(pref_8))then
         pref_8_CP = c_loc(pref_8)
      else
         pref_8_CP = C_NULL_PTR
      endif
      if(present(rcoef1))then
         rcoef1_CP = c_loc(rcoef1)
      else
         rcoef1_CP = C_NULL_PTR
      endif
      if(present(rcoef2))then
         rcoef2_CP = c_loc(rcoef2)
      else
         rcoef2_CP = C_NULL_PTR
      endif
      if(present(rcoef3))then
         rcoef3_CP = c_loc(rcoef3)
      else
         rcoef3_CP = C_NULL_PTR
      endif
      if(present(rcoef4))then
         rcoef4_CP = c_loc(rcoef4)
      else
         rcoef4_CP = C_NULL_PTR
      endif
      if(present(a_m_8))then
         a_m_8_CP = c_loc(a_m_8)
         nl_m = size(a_m_8)
      else
         a_m_8_CP = C_NULL_PTR
      endif
      if(present(b_m_8))then
         b_m_8_CP = c_loc(b_m_8)
      else
         b_m_8_CP = C_NULL_PTR
      endif
      if(present(c_m_8))then
         c_m_8_CP = c_loc(c_m_8)
      else
         c_m_8_CP = C_NULL_PTR
      endif
      if(present(a_t_8))then
         a_t_8_CP = c_loc(a_t_8)
         nl_t = size(a_t_8)
      else
         a_t_8_CP = C_NULL_PTR
      endif
      if(present(b_t_8))then
         b_t_8_CP = c_loc(b_t_8)
      else
         b_t_8_CP = C_NULL_PTR
      endif
      if(present(c_t_8))then
         c_t_8_CP = c_loc(c_t_8)
      else
         c_t_8_CP = C_NULL_PTR
      endif
      if(present(a_w_8))then
         a_w_8_CP = c_loc(a_w_8)
         nl_w = size(a_w_8)
      else
         a_w_8_CP = C_NULL_PTR
      endif
      if(present(b_w_8))then
         b_w_8_CP = c_loc(b_w_8)
      else
         b_w_8_CP = C_NULL_PTR
      endif
      if(present(c_w_8))then
         c_w_8_CP = c_loc(c_w_8)
      else
         c_w_8_CP = C_NULL_PTR
      endif
      if(present(ip1_m))then
         ip1_m_CP = c_loc(ip1_m)
      else
         ip1_m_CP = C_NULL_PTR
      endif
      if(present(ip1_t))then
         ip1_t_CP = c_loc(ip1_t)
      else
         ip1_t_CP = C_NULL_PTR
      endif
      if(present(ip1_w))then
         ip1_w_CP = c_loc(ip1_w)
      else
         ip1_w_CP = C_NULL_PTR
      endif
      if( f_new_build_vert(self%cptr,kind,version,nk,l_ip1,l_ip2, &
           ptop_8_CP, pref_8_CP, rcoef1_CP, rcoef2_CP, rcoef3_CP, rcoef4_CP, &
           a_m_8_CP, b_m_8_CP, c_m_8_CP, a_t_8_CP, b_t_8_CP, c_t_8_CP, a_w_8_CP, b_w_8_CP, c_w_8_CP, &
           ip1_m_CP, ip1_t_CP, ip1_w_CP, nl_m, nl_t, nl_w) == VGD_ERROR )then
         print*,'(F_vgd) ERROR in new_build_vert, problem with c_new_build_vert',VGD_ERROR
         return
      endif

      status = VGD_OK

   end function new_build_vert

end module vGrid_Descriptors
