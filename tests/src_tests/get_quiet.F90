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
program tests
  use vGrid_Descriptors, only: vgd_new,vgd_get,vgd_putopt,VGD_LEN_NAME,VGD_ERROR,VGD_OK
  use vgrid_utils, only: same_vec
  
  use Unit_Testing, only: ut_report
    
  implicit none

  integer :: vgdid
  integer :: stat,lu=0,fnom,fstouv,fstfrm,fclos,lutxt=69,ip1,ip2

  integer :: my_int,my_int2
  integer, dimension(:), pointer :: my_int_1d,my_int2_1d
  real :: my_real,my_real2
  real, dimension(:), pointer :: my_real_1d,my_real2_1d
  real(kind=8) :: my_real8,my_real82
  real(kind=8), dimension(:), pointer :: my_real8_1d,my_real82_1d
  real(kind=8), dimension(:,:,:), pointer :: my_real8_3d,my_real82_3d
  character(len=VGD_LEN_NAME) :: my_char, my_char2
  logical :: my_logical, ok=.true.,my_logical2

  nullify(my_int_1d,my_int2_1d,my_real_1d,my_real2_1d,my_real8_1d,my_real82_1d,my_real8_3d,my_real82_3d)

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

  !
  stat = vgd_putopt('ALLOW_RESHAPE',.true.)

  ! Construct a new set of 3D coordinate descriptors
  stat = vgd_new(vgdid,unit=lu,format="fst")
  call flush(6)

  ! Get information about the coordinate
  
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_int detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_int detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int_1d)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int_1d,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_int_1d detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_int_1d,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_int_1d detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_real detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_real detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real_1d)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real_1d,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_real_1d detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real_1d,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_real_1d detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_real8 detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_real8 detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_1d)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_1d,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_real8_1d detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_1d,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_real8_1d detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_3d)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_3d,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_real8_3d detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_real8_3d,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_real8_3d detected and message not printed above'
  call flush(6)

  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_char)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_char,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_char detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_char,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_char detected but message not printed above'
  call flush(6)  
  
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_logical)
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_logical,quiet=.false.)
  if(stat==VGD_ERROR)print*,'Error with get_logical detected and message printed above'
  stat = vgd_get(vgdid,key='UNDEFINED KEY',value=my_logical,quiet=.true.)
  if(stat==VGD_ERROR)print*,'Error with get_logical detected but message not printed above'
  call flush(6)  
  
  !===================================
  ! Tests get method on value returned
 
  ! int
  stat = vgd_get(vgdid,'KIND',my_int)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'KIND',my_int2,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_int==my_int2))call message(ok,'Test1 on get_int failed')
  stat = vgd_get(vgdid,'KIND',my_int2,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_int==my_int2))call message(ok,'Test2 on get_int failed')

  ! int 1d
  stat = vgd_get(vgdid,'VIPM',my_int_1d)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'VIPM',my_int2_1d,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_int_1d,my_int2_1d))call message(ok,'Test1 on get_int_1d failed')  
  stat = vgd_get(vgdid,'VIPM',my_int2_1d,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_int_1d,my_int2_1d))call message(ok,'Test2 on get_int_1d failed')

  ! real 
  stat = vgd_get(vgdid,'RC_1',my_real)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'RC_1',my_real2,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_real==my_real2))call message(ok,'Test1 on get_real failed')
  stat = vgd_get(vgdid,'RC_1',my_real2,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_real==my_real2))call message(ok,'Test2 on get_real failed')

  ! real 1d
  stat = vgd_get(vgdid,'VCDM',my_real_1d)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'VCDM',my_real2_1d,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real_1d,my_real2_1d))call message(ok,'Test1 on get_real_1d failed')
  stat = vgd_get(vgdid,'VCDM',my_real2_1d,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real_1d,my_real2_1d))call message(ok,'Test2 on get_real_1d failed')
  
  ! real8
  stat = vgd_get(vgdid,'PREF',my_real8)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'PREF',my_real82,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_real8==my_real82))call message(ok,'Test1 on get_real8 failed')
  stat = vgd_get(vgdid,'PREF',my_real82,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_real8==my_real82))call message(ok,'Test2 on get_real8 failed')  

  ! real8 1d
  stat = vgd_get(vgdid,'CA_M',my_real8_1d)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'CA_M',my_real82_1d,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real8_1d,my_real82_1d))call message(ok,'Test1 on get_real8_1d failed')
  stat = vgd_get(vgdid,'CA_M',my_real82_1d,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real8_1d,my_real82_1d))call message(ok,'Test2 on get_real8_1d failed')

  ! real* 3d
  stat = vgd_get(vgdid,'VTBL',my_real8_3d)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'VTBL',my_real82_3d,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real8_3d,my_real82_3d))call message(ok,'Test1 on get_my_real8_3d failed')
  stat = vgd_get(vgdid,'VTBL',my_real82_3d,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.same_vec(my_real8_3d,my_real82_3d))call message(ok,'Test2 on get_my_real8_3d failed')

  stat = vgd_get(vgdid,'LOGP',my_logical)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'LOGP',my_logical2,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_logical.and.my_logical2))call message(ok,'Test1 on get_logical failed')
  stat = vgd_get(vgdid,'LOGP',my_logical2,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_logical.and.my_logical2))call message(ok,'Test2 on get_logical failed')

  stat = vgd_get(vgdid,'RFLD',my_char)
  if(stat==VGD_ERROR)ok=.false.
  stat = vgd_get(vgdid,'RFLD',my_char2,quiet=.false.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_char==my_char2))call message(ok,'Test1 on get_char failed')
  stat = vgd_get(vgdid,'RFLD',my_char2,quiet=.true.)
  if(stat==VGD_ERROR)ok=.false.
  if(.not.(my_char==my_char2))call message(ok,'Test2 on get_char failed')

  call ut_report(ok,message='Grid_Descriptors::vgd_get get character value')

  stat = fstfrm(lu)
  stat = fclos(lu)

end program tests

subroutine message(F_ok,F_message)

   implicit none

   logical :: F_ok
   character(len=*) :: F_message

   F_ok=.false.

   print*,F_message;flush(6)

end subroutine message
