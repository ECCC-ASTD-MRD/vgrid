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

module utils

  implicit none
  private

  ! Public utilities
  public :: size_ok                             !array dimension checking
  public :: get_allocate                        !allocation of array values
  public :: flip_transfer                       !string transfer to/from real8 entry
  public :: same_vec                            !check for equivalence of arrays
  public :: up                                  !convert string to upper-case
  public :: get_error,put_error                 !get/put error messaging 
  public :: printingCharacters                  !printable character strings
  public :: comp_diag_a                         !compute value A of diagnostic level

  ! Public vgrid_descriptor constants
#include "vgrid.hf"

  ! Private constants
  integer, parameter :: LONG_STRING=1024        !number of characters in a long string

  ! Private variables
  interface comp_diag_a
     module procedure comp_diag_a_height
     module procedure comp_diag_a_ip1
  end interface comp_diag_a

  interface size_ok
     module procedure size_ok_i1d
     module procedure size_ok_r81d
     module procedure size_ok_r83d
  end interface

  interface get_allocate
     module procedure get_allocate_i1d
     module procedure get_allocate_r1d
     module procedure get_allocate_r3d
     module procedure get_allocate_r81d
     module procedure get_allocate_r83d
  end interface

  interface flip_transfer
     module procedure flip_transfer_r8
     module procedure flip_transfer_char
  end interface

  interface same_vec
     module procedure same_vec_i
     module procedure same_vec_r
     module procedure same_vec_r8
     module procedure same_vec_r83d
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Check array sizes
   
  real*8 function comp_diag_a_height(pref_8,height) result(aaa)
     implicit none
     real(kind=8), intent(in) :: pref_8
     real, intent(in) :: height
     ! Local variables
     real, parameter :: RGASD       =    0.287050000000E+03
     real, parameter :: GRAV        =    0.980616000000E+01
     real, parameter :: TCDK        =    0.273150000000E+03     
     aaa=log(pref_8) - GRAV*height/(RGASD*TCDK)
  end function comp_diag_a_height

  real*8 function comp_diag_a_ip1(pref_8,ip1) result(aaa)
     implicit none
     real(kind=8), intent(in) :: pref_8
     integer, intent(in) :: ip1
     ! Local variables
     real :: height
     real, parameter :: RGASD       =    0.287050000000E+03
     real, parameter :: GRAV        =    0.980616000000E+01
     real, parameter :: TCDK        =    0.273150000000E+03     
     integer :: kind
     call convip(ip1,height,kind,-1,"",.false.)
     aaa=log(pref_8) - GRAV*height/(RGASD*TCDK)
  end function comp_diag_a_ip1

  logical function size_ok_i1d(p1,p2) result(ok)
    implicit none
    ! Check size matching for 3d real8 arrays
    integer, dimension(:), pointer :: p1        !Input pointer 1
    integer, dimension(:), pointer :: p2        !Input pointer 2
    ok = .false.
    if (.not.associated(p1) .or. .not.associated(p2)) then
       write(for_msg,*) 'unallocated pointer to check in size_ok'
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    if (size(p1,dim=1) == size(p2,dim=1)) ok = .true.
    return
  end function size_ok_i1d

  logical function size_ok_r81d(p1,p2) result(ok)
    ! Check size matching for 3d real8 arrays
    implicit none
    real(kind=8), dimension(:), pointer :: p1   !Input pointer 1
    real(kind=8), dimension(:), pointer :: p2   !Input pointer 2
    ok = .false.
    if (.not.associated(p1) .or. .not.associated(p2)) then
       write(for_msg,*) 'unallocated pointer to check in size_ok'
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    if (size(p1,dim=1) == size(p2,dim=1)) ok = .true.
    return
  end function size_ok_r81d

  logical function size_ok_r83d(p1,p2) result(ok)
    ! Check size matching for 3d real8 arrays
    implicit none
    real(kind=8), dimension(:,:,:), pointer :: p1 !Input pointer 1
    real(kind=8), dimension(:,:,:), pointer :: p2 !Input pointer 2
    ok = .false.
    if (.not.associated(p1) .or. .not.associated(p2)) then
       write(for_msg,*) 'unallocated pointer to check in size_ok'
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    if ( size(p1,dim=1) == size(p2,dim=1) .and. &
         size(p1,dim=2) == size(p2,dim=2) .and. &
         size(p1,dim=3) == size(p2,dim=3)) ok = .true.
    return
  end function size_ok_r83d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Allocate space for pointer returns

  integer function get_allocate_i1d(key_S,value,len,allow_reshape_L,msg_S) result(istat)
    ! Allocate space for the result value and report error
    implicit none
    character(len=*), intent(in) :: key_S
    integer, dimension(:), pointer :: value
    integer, intent(in) :: len
    logical :: allow_reshape_L
    character(len=*) :: msg_S
    !Local variables
    logical :: alloc_lev_L   
    istat=-1
    alloc_lev_L=.false.
    if(.not.associated(value))then
       alloc_lev_L=.true.
    else
        if(size(value)/=len)then
           if(allow_reshape_L)then
              write(for_msg,*) 'reshaping 1D integer vector '//trim(msg_S)
              call msg(MSG_INFO,VGD_PRFX//for_msg)
              deallocate(value)
              alloc_lev_L=.true.
           else
              write(for_msg,*) '1D pointer already allocated with a different length, will not reallocate '//trim(msg_S)
              call msg(MSG_ERROR,VGD_PRFX//for_msg)
              return
           endif
        endif
     endif
     if(alloc_lev_L)then
        allocate(value(len),stat=istat)
        if (istat /= 0) then
           write(for_msg,*) 'unable to allocate space for '//trim(key_S)//' request '//trim(msg_S)
           call msg(MSG_CRITICAL,for_msg)
        endif
     else
        istat=0
     endif
  end function get_allocate_i1d
  
  integer function get_allocate_r1d(key_S,value,len,allow_reshape_L,msg_S) result(istat)
    ! Allocate space for the result value and report error
    implicit none
    character(len=*), intent(in) :: key_S
    real, dimension(:), pointer :: value
    integer, intent(in) :: len
    logical :: allow_reshape_L
    character(len=*) :: msg_S
    !Local variables
    logical :: alloc_lev_L   
    istat=-1
    alloc_lev_L=.false.
    if(.not.associated(value))then
       alloc_lev_L=.true.
    else
       if(size(value)/=len)then
          if(allow_reshape_L)then
             write(for_msg,*) 'reshaping 1D real vector '//trim(msg_S)
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             deallocate(value)
             alloc_lev_L=.true.
          else
             write(for_msg,*) '1D pointer already allocated with a different length, will not reallocate '//trim(msg_S)
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
       endif
    endif
    if(alloc_lev_L)then
       allocate(value(len),stat=istat)
       if (istat /= 0) then
          write(for_msg,*) 'unable to allocate space for '//trim(key_S)//' request '//trim(msg_S)
          call msg(MSG_CRITICAL,for_msg)
       endif
    else
       istat=0
    endif
  end function get_allocate_r1d

  integer function get_allocate_r81d(key_S,value,len,allow_reshape_L,msg_S) result(istat)
    ! Allocate space for the result value and report error
    implicit none
    character(len=*), intent(in) :: key_S
    real*8, dimension(:), pointer :: value
    integer, intent(in) :: len
    logical :: allow_reshape_L
    character(len=*) :: msg_S
    !Local variables
    logical :: alloc_lev_L   
    istat=-1
    alloc_lev_L=.false.
    if(.not.associated(value))then
       alloc_lev_L=.true.
    else
       if(size(value)/=len)then
          if(allow_reshape_L)then
             write(for_msg,*) 'reshaping 1D real*8 vector '//trim(msg_S)
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             deallocate(value)
             alloc_lev_L=.true.
          else
             write(for_msg,*) '1D pointer already allocated with a different length, will not reallocate '//trim(msg_S)
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
       endif
    endif
    if(alloc_lev_L)then
       allocate(value(len),stat=istat)
       if (istat /= 0) then
          write(for_msg,*) 'unable to allocate space for '//trim(key_S)//' request '//trim(msg_S)
          call msg(MSG_CRITICAL,for_msg)
       endif
    else
       istat=0
    endif
 end function get_allocate_r81d

 integer function get_allocate_r3d(key_S,value,len,allow_reshape_L,msg_S) result(istat)
    ! Allocate space for the result value and report error (len is result of 'shape()')
    implicit none
    character(len=*), intent(in) :: key_S
    real, dimension(:,:,:), pointer :: value
    integer, dimension(:), intent(in) :: len
    logical :: allow_reshape_L
    character(len=*) :: msg_S
    !Local variables
    logical :: alloc_lev_L   
    istat=-1
    if (size(len) < 3) then
       write(for_msg,*) 'wrong array shape specified for '//trim(key_S)
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    alloc_lev_L=.false.
    if(.not.associated(value))then
       alloc_lev_L=.true.
    else
       if(  size(value,1)/=len(1).or.&
            size(value,2)/=len(2).or.&
            size(value,3)/=len(3))then
          if(allow_reshape_L)then
             write(for_msg,*) 'reshaping 3D real table'//trim(msg_S)
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             deallocate(value)
             alloc_lev_L=.true.
          else
             write(for_msg,*) '3D pointer already allocated with a different length, will not reallocate '//trim(msg_S)
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
       endif
    endif
    if(alloc_lev_L)then
       allocate(value(len(1),len(2),len(3)),stat=istat)
       if (istat /= 0) then
          write(for_msg,*) 'unable to allocate space for '//trim(key_S)//' request '//trim(msg_S)
          call msg(MSG_CRITICAL,for_msg)
       endif
    else
       istat=0
    endif
 end function get_allocate_r3d

 integer function get_allocate_r83d(key_S,value,len,allow_reshape_L,msg_S) result(istat)
    ! Allocate space for the result value and report error (len is result of 'shape()')
    implicit none
    character(len=*), intent(in) :: key_S
    real*8, dimension(:,:,:), pointer :: value
    integer, dimension(:), intent(in) :: len
    logical :: allow_reshape_L
    character(len=*) :: msg_S
    !Local variables
    logical :: alloc_lev_L   
    istat=-1
    if (size(len) < 3) then
       write(for_msg,*) 'wrong array shape specified for '//trim(key_S)
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    alloc_lev_L=.false.
    if(.not.associated(value))then
       alloc_lev_L=.true.
    else
       if(  size(value,1)/=len(1).or.&
            size(value,2)/=len(2).or.&
            size(value,3)/=len(3))then
          if(allow_reshape_L)then
             write(for_msg,*) 'reshaping 3D real*8 table'//trim(msg_S)
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             deallocate(value)
             alloc_lev_L=.true.
          else
             write(for_msg,*) '3D pointer already allocated with a different length, will not reallocate '//trim(msg_S)
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
       endif
    endif
    if(alloc_lev_L)then
       allocate(value(len(1),len(2),len(3)),stat=istat)
       if (istat /= 0) then
          write(for_msg,*) 'unable to allocate space for '//trim(key_S)//' request '//trim(msg_S)
          call msg(MSG_CRITICAL,for_msg)
       endif
    else
       istat=0
    endif
 end function get_allocate_r83d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! String transfer to/from real8

  function is_big_endian() result(bend)
    ! Determine the endianness of the machine
    implicit none
    character(len=4) :: str
    logical :: bend
    select case (transfer(1094861636,str))
    case ('ABCD')
       bend = .true.
    case ('DCBA')
       bend = .false.
    case DEFAULT
       write(for_msg,*) 'WARNING: unable to determine bit ordering in is_big_endian'
       call msg(MSG_CRITICAL,for_msg)
       bend = .false.
    end select
    return
  end function is_big_endian

  integer function flip_transfer_r8(str,val_out) result(status)
    ! Convert string to a little-endian 8-byte real
    implicit none
    character(len=*), intent(in) :: str                 !string to convert
    real*8, intent(out) :: val_out                      !8-byte real for output
    integer :: i,cnt
    character(len=8) :: str_cvt
    status = VGD_ERROR; val_out = 0.d0
    if (len_trim(str) > len(str_cvt)) then
       write(for_msg,*) 'string length to flip_transfer (',len_trim(str),') exceeds limit: ',len(str)
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    if (is_big_endian()) then
       cnt = 1
       do i=len(str_cvt),1,-1
          if (cnt <= len_trim(str)) then
             str_cvt(i:i) = str(cnt:cnt)
             cnt = cnt+1
          else
             str_cvt(i:i) = ' '
          endif
       enddo
    else
       str_cvt = '        '
       str_cvt(1:len_trim(str)) = str
    endif
    val_out = transfer(str_cvt,val_out)
    status = VGD_OK
    return
  end function flip_transfer_r8

  integer function flip_transfer_char(val,str_out) result(status)
    ! Convert a little-endian 8-byte real to a string
    implicit none
    real*8, intent(in) :: val                           !8-byte real to convert
    character(len=*), intent(out) :: str_out            !string for output
    integer :: i,cnt
    character(len=8) :: str_cvt
    logical :: overflow
    status = VGD_ERROR; str_out = ''
    str_cvt = transfer(val,str_cvt)
    overflow = .false.
    if (is_big_endian()) then
       cnt = 1; overflow = .false.
       do i=len(str_cvt),1,-1
          if (cnt <= len(str_out)) then
             str_out(cnt:cnt) = str_cvt(i:i)
             cnt = cnt+1
          else
             if (ichar(str_cvt(i:i)) > 32) overflow = .true.
          endif
       enddo
    else
       str_out = str_cvt
    endif
    if (overflow) then
       write(for_msg,*) 'string length to flip_transfer (',len(str_out),') is insufficient: ',len_trim(str_cvt)
       call msg(MSG_CRITICAL,for_msg)
       return
    endif
    str_out = printingCharacters(str_out)
    status = VGD_OK
    return
  end function flip_transfer_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Compare a pair of vector pointers
  
  logical function same_vec_i(vec1,vec2) result(equal)
    ! Check for equality between a pair of pointer vectors
    implicit none
    integer, dimension(:), pointer :: vec1,vec2          !Pointer vectors to compare
    integer :: i
    equal = .false.
    if (associated(vec1)) then
       if (associated(vec2)) then
          if (size(vec1) == size(vec2)) then
             do i=1,size(vec1)
                if (vec1(i) /= vec2(i)) return
             enddo
          endif
       else
          return
       endif
    else
       if (associated(vec2)) return
    endif
    equal = .true.
    return
  end function same_vec_i

  logical function same_vec_r(vec1,vec2) result(equal)
    ! Check for equality between a pair of pointer vectors
    implicit none
    real, dimension(:), pointer :: vec1,vec2     !Pointer vectors to compare
    integer :: i
    equal = .false.
    if (associated(vec1)) then
       if (associated(vec2)) then
          if (size(vec1) == size(vec2)) then
             do i=1,size(vec1)
                if (vec1(i) /= vec2(i)) return
             enddo
          endif
       else
          return
       endif
    else
       if (associated(vec2)) return
    endif
    equal = .true.
    return
  end function same_vec_r
 
  logical function same_vec_r8(vec1,vec2) result(equal)
    ! Check for equality between a pair of pointer vectors
    implicit none
    real(kind=8), dimension(:), pointer :: vec1,vec2     !Pointer vectors to compare
    integer :: i
    equal = .false.
    if (associated(vec1)) then
       if (associated(vec2)) then
          if (size(vec1) == size(vec2)) then
             do i=1,size(vec1)
                if (vec1(i) /= vec2(i)) return
             enddo
          endif
       else
          return
       endif
    else
       if (associated(vec2)) return
    endif
    equal = .true.
    return
  end function same_vec_r8

  logical function same_vec_r83d(vec1,vec2) result(equal)
    ! Check for equality between a pair of pointer vectors
    implicit none
    real(kind=8), dimension(:,:,:), pointer :: vec1,vec2 !Pointer vectors to compare
    integer :: i,j,k
    equal = .false.
    if (associated(vec1)) then
       if (associated(vec2)) then
          if (size(vec1) == size(vec2)) then
             do k=1,size(vec1,dim=3)
                do j=1,size(vec1,dim=2)
                   do i=1,size(vec1,dim=1)
                      if (vec1(i,j,k) /= vec2(i,j,k)) return
                   enddo
                enddo
             enddo
          endif
       else
          return
       endif
    else
       if (associated(vec2)) return
    endif
    equal = .true.
    return
  end function same_vec_r83d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Convert string to upper-case

  function up (string) result(upper_string)
    ! Convert a string to all upper-case
    implicit none
    character(len=*), intent(in) :: string      !Input string to upper-case
    character(len=LONG_STRING) :: upper_string  !Upper-cased result
    integer :: i
    if (len_trim(string) > len(upper_string)) then
       write(for_msg,*) 'Long string truncated in up() ',trim(string)
       call msg(MSG_WARNING,for_msg)
    endif
    upper_string = string
    do i = 1,len_trim(string)
       if (string(i:i) >= 'a' .and. string(i:i) <= 'z') then
          upper_string(i:i) = achar(iachar(string(i:i)) - 32)
       endif
    enddo
    return
  end function up

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Get/Put support functions
  
  real function get_error(key,quiet) result(value)
    ! Write error message and return a missing value     
    implicit none
    character(len=*), intent(in) :: key
    logical, optional, intent(in) :: quiet      !Do not print massages
    ! Local variables
    integer :: level_msg
    level_msg=MSG_CRITICAL
    if (present(quiet)) then
       if(quiet)level_msg=MSG_QUIET    
    endif
    write(for_msg,*) 'Attempt to retrieve invalid key '//trim(key)//' returns VGD_MISSING'
    call msg(level_msg,for_msg)
    value = dble(VGD_MISSING)
    return
  end function get_error

  integer function put_error(key) result(error)
    character(len=*), intent(in) :: key
    write(for_msg,*) 'WARNING: attempt to set useless value for '//trim(key)
    call msg(MSG_CRITICAL,for_msg)
    error = VGD_ERROR
    return
  end function put_error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Printable string characters

  function printingCharacters(str) result(str_printing)
    ! Return a string that contains only printable characters
    implicit none
    character(len=*) :: str
    character(len=LONG_STRING) :: str_printing
    integer :: i,length
    length = len(str)
    i = 1
    do while (i <= len_trim(str) .and. length == len(str))
       if (ichar(str(i:i)) >= 32) then
          i = i+1
       else
          length = i-1
       endif
    enddo
    str_printing = str(1:length)
    return
  end function printingCharacters

end module utils
