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

   end interface
   
   interface vgd_new
      module procedure new_read_no_options
   end interface vgd_new
   
contains
   
   integer function new_read_no_options(self,unit,format) result(status)
      use vgrid_utils, only: up
      ! Coordinate constructor - read from a file and initialize instance
      type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance
      integer, intent(in) :: unit                 !File unit to read descriptor information from
      character(len=*), target, optional, intent(in) :: format !File format ('fst' or 'bin') default is 'fst'
      integer, target :: ip1,ip2                 !ip1,2 values of the desired descriptors
      integer, target :: kind        ! Level kind requested by user.
      integer, target :: version     ! Level version requested by user.
      
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
         l_ip1 = -1
         l_ip2 = -1
         l_kind = -1
         l_version = -1
         
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

   end function new_read_no_options

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

end module vGrid_Descriptors
