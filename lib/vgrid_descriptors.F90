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

  ! Autors : Ron MacTaggart-Cowan and Andre Plante
  ! Contributors : Cecilien Charette

  implicit none
  private

  ! Note : all pressure is hydrostatic

  ! Public methods
  public :: vgrid_descriptor                    !vertical grid descriptor structure
  public :: vgd_new                             !class constructor
  public :: vgd_free                            !class destructor
  public :: vgd_get                             !get instance variable value
  public :: vgd_put                             !set instance variable value
  public :: vgd_getopt                          !get class variable value
  public :: vgd_putopt                          !set class variable value
  public :: vgd_print                           !dump plain-text contents of instance
  public :: vgd_write                           !write coordinates to a file
  public :: vgd_levels                          !compute physical level information
  public :: vgd_dpidpis                         !compute pressure derivative with respesct the sfc pressure
  public :: operator(==)                        !overload equivalence operator

  ! Public class constants
#include "vgrid.hf"

  ! Private class variables
  integer, parameter :: STDERR=0                !STDERR output unit
  character(len=VGD_LEN_NAME) :: ZNAME='!!'     !name of the vertical coodinate
  integer, parameter :: MAX_VKIND=100           !maximum number of 'kind' (used in error check)
  integer, parameter :: KEY_LENGTH=4            !length of key string considered for get/put operations
  integer, parameter :: ETIK_LENGTH=12          !length of standard file etikets
  integer, parameter :: MAX_DESC_REC=10000      !maximum number of descriptor records in a single file
  character(len=1), dimension(3), parameter :: MATCH_GRTYP=(/'X','Y','Z'/) !grid types with ip1,2 to ig1,2 mapping
  logical :: ALLOW_RESHAPE=.false.              ! Allow reshape of class pointer members
  logical :: ALLOW_SIGMA=.false.                ! Allow legacy construction of sigma levels, it is false by default in
                                                !    order to prevent errors that araise often for cases that users only 
                                                !    copy P0 and forget to carry HY or PT along. If the user knows what
                                                !    he/she is doing, then he/she ca set ALLOW_SIGMA to true.

  ! Validity table for self
  integer, dimension(2), parameter :: ptop_out_8_valid=                                  (/5004,5005/)
  integer, dimension(6), parameter :: ptop_8_valid=             (/1002,1003,5001,5002,5003,5004/)
  integer, dimension(6), parameter :: pref_8_valid=                  (/1003,5001,5002,5003,5004,5005/)
  integer, dimension(6), parameter :: rcoef1_valid=                  (/1003,5001,5002,5003,5004,5005/)
  integer, dimension(4), parameter :: rcoef2_valid=                            (/5002,5003,5004,5005/)
  integer, dimension(9), parameter :: a_m_8_valid=    (/1001,1002,1003,2001,5001,5002,5003,5004,5005/)
  integer, dimension(9), parameter :: b_m_8_valid=    (/1001,1002,1003,2001,5001,5002,5003,5004,5005/)
  integer, dimension(4), parameter :: a_t_8_valid=                             (/5002,5003,5004,5005/)
  integer, dimension(8), parameter :: a_t_8_valid_get=(/1001,1002,     2001,5001,5002,5003,5004,5005/)
  integer, dimension(4), parameter :: b_t_8_valid=                             (/5002,5003,5004,5005/)
  integer, dimension(8), parameter :: b_t_8_valid_get=(/1001,1002,     2001,5001,5002,5003,5004,5005/)
  integer, dimension(9), parameter :: ip1_m_valid=    (/1001,1002,1003,2001,5001,5002,5003,5004,5005/)
  integer, dimension(4), parameter :: ip1_t_valid=                             (/5002,5003,5004,5005/)
  integer, dimension(8), parameter :: ip1_t_valid_get=(/1001,1002,     2001,5001,5002,5003,5004,5005/)
  integer, dimension(8), parameter :: ref_name_valid= (/1001,1002,1003     ,5001,5002,5003,5004,5005/)
  integer, dimension(1), parameter :: dhm_valid=                                              (/5005/)
  integer, dimension(1), parameter :: dht_valid=                                              (/5005/)

  ! FST file record structure
  type FSTD
     sequence
     private
     integer :: ip1,ip2,ip3,ig1,ig2,ig3,ig4,dateo,deet,npas,datyp,nbits
     logical :: initialized=.false.
     ! intel compiler needs minimum character lenght to be 4 in order to make correct alignment.
     character(len=4) :: grtyp
     character(len=4) :: typvar
     character(len=KEY_LENGTH) :: nomvar
     character(len=ETIK_LENGTH) :: etiket
  end type FSTD
  type FSTD_ext
     sequence
     private
     integer :: ig1,ig2,ig3,ig4,dateo,deet,npas,datyp,nbits,ni,nj,nk
     integer :: ip1,ip2,ip3,swa,lng,dltf,ubc,extra1,extra2,extra3,datev
     character(len=4) :: grtyp
     character(len=4) :: typvar
     character(len=KEY_LENGTH) :: nomvar
     character(len=ETIK_LENGTH) :: etiket
  end type FSTD_ext

  ! Combined coordinate structure
  type vgrid_descriptor
     sequence
     private
     real*8 :: ptop_8=dble(VGD_MISSING),pref_8=dble(VGD_MISSING)!Top level and reference pressures (Pa)
     real(kind=8), dimension(:,:,:), pointer :: table=>null()!complete grid descriptor record
     real(kind=8), dimension(:), pointer :: a_m_8=>null()!A-coefficients for momentum levels
     real(kind=8), dimension(:), pointer :: b_m_8=>null()!B-coefficients for momentum levels
     real(kind=8), dimension(:), pointer :: a_t_8=>null()!A-coefficients for thermodynamic levels
     real(kind=8), dimension(:), pointer :: b_t_8=>null()!B-coefficients for thermodynamic levels
     real :: dhm                                        ! Diag level Height (m) for Momentum variables UU,VV
     real :: dht                                        ! Diag level Height (m) for Thermo variables TT,HU, etc
     integer, dimension(:), pointer :: ip1_m=>null()    !ip1 values for momentum levels
     integer, dimension(:), pointer :: ip1_t=>null()    !ip1 values for thermodynamic levels
     real :: rcoef1=VGD_MISSING,rcoef2=VGD_MISSING      !Rectification coefficients
     logical :: initialized=.false.                     !initialization status of the structure
     logical :: match_ipig                              !do ip/ig matching for records
     logical :: valid=.false.                           !Validity of structure
     integer :: ip1=0,ip2=0                             !ip1,2 values given to the 3D descriptor
     integer :: unit                                    !file unit associated with this 3D descriptor
     integer :: vcode,kind,version                      !Vertical coordinate codes
     character(len=VGD_LEN_NAME) :: ref_name='None'     !reference field name
     type(FSTD) :: rec                                  !FST file record structure for descriptor
  end type vgrid_descriptor

  interface vgd_new
     module procedure new_read
     module procedure new_from_table
     module procedure new_build_vert
     module procedure new_gen
  end interface

  interface vgd_free
     module procedure garbage_collection
  end interface

  interface vgd_get
     module procedure get_int
     module procedure get_int_1d
     module procedure get_real
     module procedure get_real_1d
     module procedure get_real8
     module procedure get_real8_1d
     module procedure get_real8_3d
     module procedure get_char
     module procedure get_logical
  end interface

  interface vgd_put
     module procedure put_int
     module procedure put_int_1d
     module procedure put_real_1d
     module procedure put_real8
     module procedure put_real8_1d
     module procedure put_real8_3d
     module procedure put_char
  end interface

  interface vgd_getopt
     module procedure getopt_logical
  end interface

  interface vgd_putopt
     module procedure putopt_logical
  end interface
  
  interface vgd_print
     module procedure print_desc
     module procedure print_vcode_description
  end interface

  interface vgd_write
     module procedure write_desc
  end interface

  interface vgd_levels
     module procedure levels_toplevel
     module procedure levels_readref
     module procedure levels_withref
     module procedure levels_withref_8
     module procedure levels_withref_prof
     module procedure levels_withref_prof_8
  end interface

  interface operator (==)
     module procedure test_equality
  end interface

  interface vgd_dpidpis
     module procedure dpidpis_withref
     module procedure dpidpis_withref_8
     module procedure dpidpis_withref_prof
     module procedure dpidpis_withref_prof_8
  end interface

  interface set_vcode
     module procedure set_vcode_d
     module procedure set_vcode_i
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Class constructor

      subroutine igapg(grtyp,pg1,pg2,pg3,pg4,ig1,ig2,ig3,ig4)
      implicit none
      character *1 grtyp
      character *(*) pg1,pg2,pg3,pg4
      integer ig1,ig2,ig3,ig4
!
      real xg1,xg2,xg3,xg4
      external cigaxg
!
      if ((grtyp .eq. 'Y') .or. (grtyp .eq. 'Z')&
         .or. (grtyp .eq. '!')) then
         write(pg1,'(i6)') ig1
         write(pg2,'(i6)') ig2
         write(pg3,'(i7)') ig3
         write(pg4,'(i7)') ig4
         return
      else if ((grtyp .eq. 'A') .or. (grtyp .eq. 'B') .or.&
              (grtyp .eq. 'G') .or. (grtyp .eq. 'N') .or.&
              (grtyp .eq. 'S') .or. (grtyp .eq. 'E')) then
         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
      else
         write(pg1,'(i6)') ig1
         write(pg2,'(i6)') ig2
         write(pg3,'(i7)') ig3
         write(pg4,'(i7)') ig4
         return
      endif
      if ((grtyp .eq. 'N') .or. (grtyp .eq. 'S')) then
         write(pg1,'(f6.1)') xg1
         write(pg2,'(f6.1)') xg2
         write(pg3,'(f5.1,a2)') (xg3/1000.), 'Km'
         write(pg4,'(f7.3)') xg4
      else if ((grtyp .eq. 'A') .or. (grtyp .eq. 'B') .or.&
              (grtyp .eq. 'G')) then
         write(pg1,'(i6)') int(xg1)
         write(pg2,'(i6)') int(xg2)
         write(pg3,'(i7)') int(xg3)
         write(pg4,'(i7)') int(xg4)
      else
         write(pg1,'(f6.2)') xg1
         write(pg2,'(f6.2)') xg2
         write(pg3,'(f7.3)') xg3
         write(pg4,'(f7.3)') xg4
      endif
      return
   end subroutine igapg



   integer function new_read(self,unit,format,ip1,ip2,kind,version) result(status)
      use utils, only: up
      ! Coordinate constructor - read from a file and initialize instance
      type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance
      integer, intent(in) :: unit                 !File unit to read descriptor information from
      character(len=*), optional, intent(in) :: format !File format ('fst' or 'bin') default is 'fst'
      integer,optional :: ip1,ip2                 !ip1,2 values of the desired descriptors
      integer,optional, intent(in) :: kind        ! Level kind requested by user.
      integer,optional, intent(in) :: version     ! Level version requested by user.

      ! Internal variables
      integer :: istat,error,lkind,mykind,myversion,lversion,count,i,myip1,myip2
      integer :: fstinl,fstluk,fstprm,ni,nj,nk,swa,lng,ubc,dltf,extra1,extra2,extra3
      integer :: nii,njj,nkk
      integer, dimension(MAX_DESC_REC) :: keyList
      type(FSTD_ext) :: var
      real*8, dimension(:,:,:), pointer :: l_table
      character(len=100) :: myformat
      logical :: toc_found_L

      ! Set error status
      status = VGD_ERROR

      nullify(l_table)

      if (vgd_free(self).eq.VGD_ERROR)then
         write(for_msg,*) 'Error with vgd_free in vgd_new (new_read)'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         Return
      endif
      
      ! Set default values
      myformat='FST'
      if (present(format)) myformat = format
      myip1 = -1
      if (present(ip1)) myip1 = ip1
      myip2 = -1
      if (present(ip2)) myip2 = ip2
      mykind=-1
      if(present(kind)) mykind = kind
      myversion=-1
      if(present(version))then
         myversion=version
         if(mykind.eq.-1.and.myversion.ne.-1)then
            write(for_msg,*) 'vgd_new option kind must be used with option version'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            self%vcode = -1
            return
         endif         
      endif

      ! Set instance variables
      self%unit = unit
      if (myip1 < 0) then
         self%match_ipig = .false.
      else
         self%match_ipig = .true.
      endif

      ! Construct vertical descriptor
      select case (trim(up(myformat)))

      ! RPN Standard file input
      case ('FST')
         error = fstinl(unit,ni,nj,nk,-1,' ',myip1,myip2,-1,' ',ZNAME,keyList,count,size(keyList))
         if (error < 0) then
            write(for_msg,*) 'problem in vgd_new (new_read) with fstinl on nomvar !!'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            self%vcode = -1
            return
         endif
         TEST_COUNT: if(count == 0)then
            write(for_msg,'("cannot find '//trim(ZNAME)//' with the following: ip1=",i10,",ip2=",i10)')myip1,myip2
            call msg(MSG_WARNING,VGD_PRFX//for_msg)
            if(self%match_ipig)then
               self%vcode = -1
               return
            endif            
            write(for_msg,'("Trying to construct vgrid descriptor from legacy encoding (PT,HY ...)")')
            call msg(MSG_WARNING,VGD_PRFX//for_msg)
            istat=vgd_legacy(self,unit,mykind)
            if(istat.eq.VGD_ERROR)then
               self%vcode = -1
               return
            endif
            error = fstd_init(self)
            if (error < 0) then
               write(for_msg,*) 'problem in vgd_new (new_read) creating record information'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
         else
            ! Loop on all !! found
            toc_found_L=.false.
            LOOP_ON_TOCS: do i=1,count
               error=my_fstprm(keyList(i),var)
               if (error < 0) then
                  write(for_msg,*) 'problem in vgd_new (new_read) with my_fstprm on key ',keyList(i)
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  self%vcode = -1
                  return
               endif
               if(mykind.ne.-1.and.myversion.ne.-1)then
                  if(var%ig1.ne.mykind*1000+myversion)cycle
               else
                  if(mykind.ne.-1)then
                     ! Get kind from fst vcode (ig1)
                     if(nint(float(var%ig1)/1000.).ne.mykind)cycle
                  endif
                  if(myversion.ne.-1)then
                     ! Get version from fst vcode (ig1)
                     if(var%ig1-nint(float(var%ig1)/1000.)*1000 .ne.myversion)cycle
                  endif
               endif
               ! If we reaches this stage then the toc satisfy the selection criteria but
               ! it may not be the only one.
               FOUND_TOC: if(.not.toc_found_L)then
                  toc_found_L=.true.
                  ni=var%ni;nj=var%nj;nk=var%nk
                  allocate(self%table(ni,nj,nk),stat=istat)
                  if (istat /= 0) then
                     write(for_msg,*) 'in vgd_new (new_read) unable to allocate Vgrid descriptor table'
                     call msg(MSG_ERROR,VGD_PRFX//for_msg)
                     return
                  endif
                  istat=fstluk(self%table,keyList(i),ni,nj,nk)
                  if(istat < 0)then
                     write(for_msg,*) 'in vgd_new (new_read) problem with fstluk on '//trim(ZNAME)
                     call msg(MSG_ERROR,VGD_PRFX//for_msg)
                     deallocate(self%table)
                     return
                  endif
                  error = fstd_init(self)
                  if (error < 0) then
                     write(for_msg,*) 'in vgd_new (new_read) problem creating record information'
                     call msg(MSG_ERROR,VGD_PRFX//for_msg)
                     deallocate(self%table)
                     return
                  endif
                  ! Sorry, I have to use fstprm!
                  istat=fstprm(keyList(i), self%rec%dateo, self%rec%deet, self%rec%npas, &
                       ni, nj, nk, self%rec%nbits, self%rec%datyp, self%rec%ip1, self%rec%ip2, self%rec%ip3, &
                       self%rec%typvar,self%rec%nomvar, self%rec%etiket, self%rec%grtyp,&
                       self%rec%ig1, self%rec%ig2, self%rec%ig3, self%rec%ig4, &
                       swa,lng,dltf,ubc,extra1,extra2,extra3)
                  if(istat<0)then
                     write(for_msg,*) 'in vgd_new (new_read) problem with fstprm on key',keyList(i)
                     call msg(MSG_ERROR,VGD_PRFX//for_msg)
                     deallocate(self%table)
                     return
                  endif
                  cycle
               end if FOUND_TOC
               ! If we get at this point this means that there are more than one toc satisfying the selection criteria.
               ! We load then all to check if they are the same. If not, we return with an error message.
               if(var%ig1.ne.self%vcode)then
                   write(for_msg,*) 'found vertical descriptors with different vcode (ig1) ',var%ig1,' vs ',self%vcode
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  return
               endif
               allocate(l_table(var%ni,var%nj,var%nk),stat=istat)
               if (istat /= 0) then
                  write(for_msg,*) 'problem in vgd_new (new_read) unable to allocate l_table'
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  return
               endif
               istat=fstluk(l_table,keyList(i),nii,njj,nkk)
               if(istat < 0)then
                  write(for_msg,*) 'problem in vgd_new (new_read) problem with fstluk on l_table i=',i
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  deallocate(l_table)
                  return
               endif
               if(nii.ne.ni.or.njj.ne.nj.or.nkk.ne.nk)then
                  write(for_msg,*) 'found different entry dimensions in vertical descriptors after search on ip1, ip2, kind, version -> ',myip1,myip2,mykind,myversion
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  deallocate(l_table)
                  return
               endif
               if(any(l_table /= self%table))then
                  write(for_msg,*) 'found different entries in vertical descriptors after search on ip1, ip2, kind, version -> ',myip1,myip2,mykind,myversion
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  deallocate(l_table)
                  return
               endif
               deallocate(l_table)                              
            enddo LOOP_ON_TOCS
            if(.not.toc_found_L)then               
               write(for_msg,*) 'cannot find !! of kind and version ',mykind,myversion
               call msg(MSG_WARNING,VGD_PRFX//for_msg)
               write(for_msg,*) "Trying to construct vgrid descriptor from legacy encoding (PT,HY ...) with kind ",mykind
               call msg(MSG_WARNING,VGD_PRFX//for_msg)
               istat=vgd_legacy(self,unit,mykind)
               if(istat.eq.VGD_ERROR)then
                  write(for_msg,*) 'Cannot find !! or legacy encoding of kind and version ',mykind,myversion
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  self%vcode = -1
                  return
               endif
               error = fstd_init(self)
               if (error < 0) then
                  write(for_msg,*) 'problem in vgd_new (new_read) creating record information with kind',mykind
                  call msg(MSG_ERROR,VGD_PRFX//for_msg)
                  return
               endif               
            endif
         endif TEST_COUNT
         
         ! Fortran binary file input (must already be in the correct position)
      case ('BIN')
         read(unit) ni,nj,nk
         allocate(self%table(ni,nj,nk),stat=istat)
         if (istat /= 0) then
            write(for_msg,*) 'unable to allocate self%table'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         read(unit) self%table
         error = fstd_init(self)
         if (error < 0) then
            write(for_msg,*) 'problem creating record information'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif

         ! Warn user on invalid input format specification
      case DEFAULT
         write(for_msg,*) 'invalid constructor format request ',trim(myformat)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select

      ! Fill structure from input table
      error = new_from_table(self,self%table)
      if (error /= VGD_OK) then
         write(for_msg,*) 'unable to construct from table'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
      endif

      ! Check that the requested kind/version matches the record kind/version
      if (present(kind)) then
         istat = get_version_info(self,lkind,lversion)
         if (lkind /= mykind .and. mykind /= -1) then
            write(for_msg,*) 'cannot find requested kind=',mykind,' in file'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            return
         endif
         if (present(version)) then
            if (lversion /= myversion .and. myversion /= -1) then
               write(for_msg,*) 'cannot find requested kind and version=',mykind,myversion,' in file'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
         endif
      endif

      self%valid=.true.
      ! Set status and return
      if (found_descriptor(self)) status = VGD_OK
      return
   end function new_read

   integer function new_from_table(self,table) result(status)
     ! Coordinate constructor - build vertical descriptor from table input
     ! Set internal vcode (if all above was successful)
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance    
     real(kind=8), dimension(:,:,:), pointer :: table   !Raw table of vgrid records

     ! Local variables
     integer :: error,istat,kind,version
     real(kind=8), dimension(size(table,dim=1),size(table,dim=2),size(table,dim=3)) :: ltable

     self%valid=.false.
     ! Set error status
     status = VGD_ERROR

     ! Initializations
     ltable = table
     
     if (associated(self%table)) deallocate(self%table)
     allocate(self%table(size(ltable,dim=1),size(ltable,dim=2),size(ltable,dim=3)),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%table'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     self%table = ltable

     ! Fill remainder of structure
     error = set_vcode(self)
     select case (self%vcode)
     case (1001)
        istat=decode_vert_1001(self)
        if(istat < 0)then
           write(for_msg,*) 'problem decoding table for vcode 1001'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
           return
        endif
     case (1002)
        istat=decode_vert_1002(self)
        if(istat < 0)then
           write(for_msg,*) 'problem decoding table with vcode 1002'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
           return
        endif
     case (2001)
        istat=decode_vert_2001(self)
     case (1003,5001)
        istat=decode_vert_5001(self)
        if(istat < 0)then
           write(for_msg,*) 'problem decoding table with vcode 1003 or 5001'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
           return
        endif
     case (5002,5003,5004,5005)
        istat=decode_vert_5002(self)
        if(istat < 0)then
           write(for_msg,*) 'problem decoding table with vcode 5002,5003,5004 or 5005'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
           return
        endif
     case DEFAULT
        istat = get_version_info(self,kind,version)
        write(for_msg,*) 'invalid kind or version in new_from_table:',kind,version
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     end select

     ! Initialize record information
     error = fstd_init(self)
     if (error < 0) then
        write(for_msg,*) 'problem creating record information'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     
     self%valid=.true.
     ! Set status and return
     status = VGD_OK
     return
   end function new_from_table

   integer function new_build_vert(self,kind,version,nk,ip1,ip2, &
        ptop_8,pref_8,rcoef1,rcoef2,a_m_8,b_m_8,a_t_8,b_t_8, &
        ip1_m,ip1_t) result(status)
      ! Coordinate constructor - build vertical descriptor from arguments
      type(vgrid_descriptor) :: self                    !Vertical descriptor instance    
      integer, intent(in) :: kind,version               !Kind,version to create
      integer, intent(in) :: nk                         !Number of levels
      integer, optional, intent(in) :: ip1,ip2          !IP1,2 values for FST file record [0,0]
      real, optional, intent(in) :: rcoef1,rcoef2       !R-coefficient values for rectification
      real*8, optional, intent(in) :: ptop_8            !Top-level pressure (Pa)
      real*8, optional, intent(in) :: pref_8            !Reference-level pressure (Pa)
      real*8, optional, dimension(:) :: a_m_8,a_t_8     !A-coefficients for momentum(m),thermo(t) levels
      real*8, optional, dimension(:) :: b_m_8,b_t_8     !B-coefficients for momentum(m),thermo(t) levels
      integer, optional, dimension(:) :: ip1_m,ip1_t    !Level ID (IP1) for momentum(m),thermo(t) levels

      ! Local variables
      integer :: error
      logical :: missingInput
      character(len=10) :: cvcode

      self%valid=.false.
      ! Set error status
      status = VGD_ERROR

      ! Initializations
      self%kind=kind
      self%version=version
      self%unit = -1      
      self%match_ipig=.true.

      ! Check for required inputs
      error = set_vcode(self,kind,version)
      missingInput = .false.
      if(is_valid(self,ptop_8_valid)) then
         if(present(ptop_8))then
            self%ptop_8 = ptop_8
         else
            write(for_msg,*) 'ptop_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,pref_8_valid)) then
         if(present(pref_8))then
            self%pref_8 = pref_8
         else
            write(for_msg,*) 'pref_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,rcoef1_valid)) then
         if(present(rcoef1))then
            self%rcoef1 = rcoef1
         else
            write(for_msg,*) 'rcoef1 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif     
      if(is_valid(self,rcoef2_valid)) then
         if(present(rcoef2))then
            self%rcoef2 = rcoef2
         else
            write(for_msg,*) 'rcoef2 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,a_m_8_valid)) then
         if(present(a_m_8))then
            if (associated(self%a_m_8)) deallocate(self%a_m_8)
            allocate(self%a_m_8(size(a_m_8)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating a_m_8 in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%a_m_8 = a_m_8
         else
            write(for_msg,*) 'a_m_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,b_m_8_valid)) then
         if(present(b_m_8))then
            if (associated(self%b_m_8)) deallocate(self%b_m_8)
            allocate(self%b_m_8(size(b_m_8)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating b_m_8 in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%b_m_8 = b_m_8
         else
            write(for_msg,*) 'b_m_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,a_t_8_valid)) then
         if(present(a_t_8))then
            if (associated(self%a_t_8)) deallocate(self%a_t_8)
            allocate(self%a_t_8(size(a_t_8)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating a_t_8 in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%a_t_8 = a_t_8
         else
            write(for_msg,*) 'a_t_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,b_t_8_valid)) then
         if(present(b_t_8))then
            if (associated(self%b_t_8)) deallocate(self%b_t_8)
            allocate(self%b_t_8(size(b_t_8)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating b_t_8 in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%b_t_8 = b_t_8
         else
            write(for_msg,*) 'b_t_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif
      if(is_valid(self,ip1_m_valid)) then
         if(present(ip1_m))then            
            if (associated(self%ip1_m)) deallocate(self%ip1_m)
            allocate(self%ip1_m(size(ip1_m)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating ip1_m in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%ip1_m = ip1_m
         else
            write(for_msg,*) 'ip1_m is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif      
      if(is_valid(self,ip1_t_valid)) then
         if(present(ip1_t))then
            if (associated(self%ip1_t)) deallocate(self%ip1_t)
            allocate(self%ip1_t(size(ip1_t)),stat=error)
            if(error < 0)then
               write(for_msg,*) 'problem allocating ip1_t in new_build_vert'
               call msg(MSG_ERROR,VGD_PRFX//for_msg)
               return
            endif
            self%ip1_t = ip1_t
         else
            write(for_msg,*) 'ip1_t is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            missingInput = .true.
         endif
      endif 
       
      if (missingInput) return      

      ! Fill table with version-specific encoder
      select case (self%vcode)
      case (1001)
         cvcode="1001"
         error = encode_vert_1001(self,nk)
      case (1002)
         cvcode="1002"                
         error = encode_vert_1002(self,nk)         
      case (1003)
         cvcode="1003"
         error = encode_vert_5001(self,nk)
      case (2001)
         cvcode="2001"
         error = encode_vert_2001(self)
      case (5001)
         cvcode="5001"
         error = encode_vert_5001(self,nk)
      case (5002,5003,5004,5005)
         cvcode="5002"
         error = encode_vert_5002(self,nk)
      case DEFAULT
         write(for_msg,*) 'unsupported kind and version : ',kind,version,' (vcode) ',self%vcode
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select
      if (error /= VGD_OK) then
         write(for_msg,'(i4)') self%vcode
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         write(for_msg,*) 'problem with encode_vert_'//trim(cvcode)
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif

      ! Initialize record information
      error = fstd_init(self)
      if (error < 0) then
         write(for_msg,*) 'problem creating record information'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      endif
      self%rec%ip1=0
      if (present(ip1))then
         if(ip1 > -1)self%rec%ip1 = ip1
      endif
      self%rec%ip2=0
      if (present(ip2))then
         if(ip2 > -1)self%rec%ip2 = ip2
      endif

      self%valid=.true.
      ! Set status and return
      status = VGD_OK
      return
   end function new_build_vert

   integer function new_gen(self,kind,version,hyb,rcoef1,rcoef2,ptop_8,pref_8,ptop_out_8,ip1,ip2,stdout_unit,dhm,dht) result(status)
      use vdescript_1001,      only: vgrid_genab_1001
      use vdescript_1002_5001, only: vgrid_genab_1002_5001
      use vdescript_2001,      only: vgrid_genab_2001
      use vdescript_5002,      only: vgrid_genab_5002
      ! Coordinate constructor - build vertical descriptor from hybrid coordinate entries
      type(vgrid_descriptor),intent(inout) :: self      !Vertical descriptor instance    
      integer, intent(in) :: kind,version               !Kind,version to create
      real, dimension(:),intent(in) :: hyb              !List of hybrid levels
      real, optional, intent(in) :: rcoef1,rcoef2       !R-coefficient values for rectification
      real*8, optional, intent(in) :: ptop_8            !Top-level pressure (Pa) inout
      real*8, optional, intent(out):: ptop_out_8        !Top-level pressure (Pa) output if ptop_8 < 0
      real*8, optional, intent(in) :: pref_8            !Reference-level pressure (Pa)
      integer, optional, intent(in) :: ip1,ip2          !IP1,2 values for FST file record [0,0]
      integer, optional, intent(in) :: stdout_unit      !Unit number for verbose output [STDERR]
      real, optional, intent(in) :: dhm,dht             !Diag levels Height for Momentum/Thermo vaiables

      ! Internal variables
      integer :: myip1,myip2,mystdout_unit,error
      integer, dimension(:), pointer :: ip1_m,ip1_t
      real, dimension(:), pointer :: hybm,hybt
      real*8, dimension(:), pointer :: a_m_8,b_m_8,a_t_8,b_t_8
      logical :: errorInput=.false.

      nullify(ip1_m,ip1_t,hybm,hybt,a_m_8,b_m_8,a_t_8,b_t_8)

      self%valid=.false.
      ! Set error status
      status = VGD_ERROR

      ! Set default values
      myip1 = -1
      if (present(ip1)) myip1 = ip1
      myip2 = -1
      if (present(ip2)) myip2 = ip2
      mystdout_unit = STDERR
      if (present(stdout_unit)) mystdout_unit = stdout_unit

      error = VGD_ERROR
      ! Check for required inputs

      error = set_vcode(self,kind,version)
      errorInput = .false.
      if(is_valid(self,ptop_8_valid)) then
         if(.not.present(ptop_8))then
            write(for_msg,*) 'ptop_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,ptop_out_8_valid)) then
         if(.not.present(ptop_out_8))then
            write(for_msg,*) 'ptop_out_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,pref_8_valid)) then
         if(.not.present(pref_8))then
            write(for_msg,*) 'pref_8 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,rcoef1_valid)) then
         if(.not.present(rcoef1))then
            write(for_msg,*) 'rcoef1 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,rcoef2_valid)) then
         if(.not.present(rcoef2))then
            write(for_msg,*) 'rcoef2 is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,dhm_valid)) then
         if(.not.present(dhm))then
            write(for_msg,*) 'dhm is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(is_valid(self,dht_valid)) then
         if(.not.present(dht))then
            write(for_msg,*) 'dht is a required constructor entry'
            call msg(MSG_ERROR,VGD_PRFX//for_msg)
            errorInput = .true.
         endif
      endif
      if(present(ptop_8).and.(.not.is_valid(self,ptop_8_valid)))then
         write(for_msg,*) 'ptop_8 is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(ptop_out_8).and.(.not.is_valid(self,ptop_out_8_valid)))then
         write(for_msg,*) 'ptop_out_8 is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(pref_8).and.(.not.is_valid(self,pref_8_valid)))then
         write(for_msg,*) 'pref_8 is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(rcoef1).and.(.not.is_valid(self,rcoef1_valid)))then
         write(for_msg,*) 'rcoef1 is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(rcoef2).and.(.not.is_valid(self,rcoef2_valid)))then
         write(for_msg,*) 'rcoef2 is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(dhm).and.(.not.is_valid(self,dhm_valid)))then
         write(for_msg,*) 'dhm is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if(present(dht).and.(.not.is_valid(self,dht_valid)))then
         write(for_msg,*) 'dht is not a required constructor entry'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         errorInput = .true.
      endif
      if (errorInput) return

      ! Call appropriate support module to compute required values
      select case (self%vcode)
      case(1001)
         call vgrid_genab_1001(hyb,hybm,a_m_8,b_m_8,ip1_m,error)
         if (error /= VGD_OK)then
            if(associated(hybm))deallocate(hybm)
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(ip1_m))deallocate(ip1_m)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              ip1_m=ip1_m)
         if (error /= VGD_OK) return        
      case (1002)
         call vgrid_genab_1002_5001(self%vcode,hyb,1.,ptop_8,80000.d0, &
              hybm,a_m_8,b_m_8,ip1_m,error)
         if (error /= VGD_OK)then
            if(associated(hybm))deallocate(hybm)
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(ip1_m))deallocate(ip1_m)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_8,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              ip1_m=ip1_m)
         if (error /= VGD_OK) return
      case(2001)
         call vgrid_genab_2001(hyb,a_m_8,b_m_8,error,ip1=ip1_m)
         if (error /= VGD_OK)then
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(ip1_m))deallocate(ip1_m)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,         &
              ip2=myip2,         &
              a_m_8=a_m_8,       &
              b_m_8=b_m_8,       &
              ip1_m=ip1_m)
         if (error /= VGD_OK) return
      case (5001)
         call vgrid_genab_1002_5001(self%vcode,hyb,rcoef1,ptop_8,pref_8, &
              hybm,a_m_8,b_m_8,ip1_m,error)
         if (error /= VGD_OK)then
            if(associated(hybm))deallocate(hybm)
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(ip1_m))deallocate(ip1_m)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_8,       &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              ip1_m=ip1_m)
         if (error /= VGD_OK) return
      case (5002)
         call vgrid_genab_5002(version,hyb,(/rcoef1,rcoef2/),ptop_8,pref_8, &
              a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,error)
         if (error /= VGD_OK)then
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(a_t_8))deallocate(a_t_8)
            if(associated(b_t_8))deallocate(b_t_8)
            if(associated(ip1_m))deallocate(ip1_m)
            if(associated(ip1_t))deallocate(ip1_t)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_8,       &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              rcoef2=rcoef2,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              a_t_8=a_t_8,         &
              b_t_8=b_t_8,         &
              ip1_m=ip1_m,         &
              ip1_t=ip1_t)
         if (error /= VGD_OK) return
      case (5003)
         call vgrid_genab_5002(version,hyb,(/rcoef1,rcoef2/),ptop_8,pref_8, &
              a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,error,lastTatU_L=.true.)
         if (error /= VGD_OK)then
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(a_t_8))deallocate(a_t_8)
            if(associated(b_t_8))deallocate(b_t_8)
            if(associated(ip1_m))deallocate(ip1_m)
            if(associated(ip1_t))deallocate(ip1_t)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_8,       &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              rcoef2=rcoef2,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              a_t_8=a_t_8,         &
              b_t_8=b_t_8,         &
              ip1_m=ip1_m,         &
              ip1_t=ip1_t)
         if (error /= VGD_OK) return
      case (5004)  
         call vgrid_genab_5002(version,hyb,(/rcoef1,rcoef2/),ptop_8,pref_8, &
              a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,error,notop_L=.true.,ptop_out_8=ptop_out_8)
         if (error /= VGD_OK)then
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(a_t_8))deallocate(a_t_8)
            if(associated(b_t_8))deallocate(b_t_8)
            if(associated(ip1_m))deallocate(ip1_m)
            if(associated(ip1_t))deallocate(ip1_t)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_out_8,   &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              rcoef2=rcoef2,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              a_t_8=a_t_8,         &
              b_t_8=b_t_8,         &
              ip1_m=ip1_m,         &
              ip1_t=ip1_t)
         if (error /= VGD_OK) return
      case (5005)  
         call vgrid_genab_5002(version,hyb,(/rcoef1,rcoef2/),-2.d0,pref_8, &
              a_m_8,b_m_8,a_t_8,b_t_8,ip1_m,ip1_t,error,notop_L=.true.,ptop_out_8=ptop_out_8, &
              dhm=dhm,dht=dht)
         if (error /= VGD_OK)then
            if(associated(a_m_8))deallocate(a_m_8)
            if(associated(b_m_8))deallocate(b_m_8)
            if(associated(a_t_8))deallocate(a_t_8)
            if(associated(b_t_8))deallocate(b_t_8)
            if(associated(ip1_m))deallocate(ip1_m)
            if(associated(ip1_t))deallocate(ip1_t)
            return
         endif
         error = new_build_vert(self,kind,version,size(hyb), &
              ip1=myip1,           &
              ip2=myip2,           &
              ptop_8=ptop_out_8,   &
              pref_8=pref_8,       &
              rcoef1=rcoef1,       &
              rcoef2=rcoef2,       &
              a_m_8=a_m_8,         &
              b_m_8=b_m_8,         &
              a_t_8=a_t_8,         &
              b_t_8=b_t_8,         &
              ip1_m=ip1_m,         &
              ip1_t=ip1_t)
         if (error /= VGD_OK) return
      case DEFAULT
         write(for_msg,*)'unsuported IP1 kind = ',kind,', AND/OR version = ',version
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      end select
      if(associated(ip1_m))deallocate(ip1_m)
      if(associated(ip1_t))deallocate(ip1_t)
      if(associated(hybm))deallocate(hybm)
      if(associated(hybt))deallocate(hybt)
      if(associated(a_m_8))deallocate(a_m_8)
      if(associated(b_m_8))deallocate(b_m_8)
      if(associated(a_t_8))deallocate(a_t_8)
      if(associated(b_t_8))deallocate(b_t_8)

      self%valid=.true.
      ! Set status and return
      status = VGD_OK
      return
   end function new_gen
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Class destructor

   integer function garbage_collection(self) result(status)
     ! Free all memory associated with with structure and uninitialize it
     type(vgrid_descriptor), intent(inout) :: self          !Vertical descriptor instance
     status=VGD_ERROR
     self%ptop_8=dble(VGD_MISSING)
     self%pref_8=dble(VGD_MISSING)
     if(associated(self%table))deallocate(self%table)
     if(associated(self%a_m_8))deallocate(self%a_m_8)
     if(associated(self%b_m_8))deallocate(self%b_m_8)
     if(associated(self%a_t_8))deallocate(self%a_t_8)
     if(associated(self%b_t_8))deallocate(self%b_t_8)
     if(associated(self%ip1_m))deallocate(self%ip1_m)
     if(associated(self%ip1_t))deallocate(self%ip1_t)
     self%rcoef1=VGD_MISSING
     self%rcoef2=VGD_MISSING
     self%initialized=.false.
     self%valid=.false.
     self%ip1=0
     self%ip2=0
     self%vcode=-1
     self%kind=-1
     self%version=-1
     self%ref_name='None'
     !
     self%rec%initialized=.false.
     ! Set error status
     status = VGD_OK
   end function garbage_collection

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Get methods

  integer function get_logical(self,key,value,quiet) result(status)
    use utils, only: up
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    character(len=*), intent(in) :: key                 !Descriptor key to retrieve
    logical, intent(out) :: value                       !Retrieved value
    logical, optional, intent(in) :: quiet              !Do not print massages    
    
    ! Local variables
    integer :: level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('LOGP')
       select case (self%vcode)
       case (1001,1002,5001)
          value=.false.
       case (5002,5003,5004,5005)
          value=.true.
       case DEFAULT
          write(for_msg,*) 'unsupported vcode for LOGP: ',self%vcode
          call msg(level_msg,VGD_PRFX//for_msg)
          return
       end select
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (logical)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
 end function get_logical

  integer function get_int(self,key,value,quiet) result(status)
    use utils, only: up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    character(len=*), intent(in) :: key                 !Descriptor key to retrieve
    integer, intent(out) :: value                       !Retrieved value
    logical, optional, intent(in) :: quiet              !Do not print massages

    ! Local variables
    integer :: istat,level_msg
    integer, dimension(:), pointer :: vip
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    nullify(vip)

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_int'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element

    select case (up(key(1:KEY_LENGTH)))
    case ('DATE')
       value = self%rec%dateo
    case ('IG_1')
       value = self%rec%ig1
    case ('IG_2')
       value = self%rec%ig2
    case ('IG_3')
       value = self%rec%ig3
    case ('IG_4')
       value = self%rec%ig4
    case ('IP_1')
       value = self%rec%ip1
    case ('IP_2')
       value = self%rec%ip2
    case ('IP_3')
       value = self%rec%ip3
    case ('KIND')
       value = self%kind
    case ('VERS')
       value = self%version
    case ('NL_M')
       istat=vgd_get(self,key='VIPM - level ip1 list (m)',value=vip)
       if(istat /= VGD_OK)then
          if(associated(vip))deallocate(vip)
          return
       endif
       value=size(vip)
    case ('NL_T')
       istat=vgd_get(self,key='VIPT - level ip1 list (m)',value=vip)
       if(istat /= VGD_OK)then
          if(associated(vip))deallocate(vip)
          return
       endif
       value=size(vip)
    case ('DIPM')
       if (is_valid(self,dhm_valid)) then
          value=self%ip1_m(size(self%ip1_m))
          
       else
          value = get_error(key,my_quiet)
          return
       endif
    case ('DIPT')
       if (is_valid(self,dht_valid)) then
          value=self%ip1_t(size(self%ip1_t))
       else
          value = get_error(key,my_quiet)
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (int)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select
    if(associated(vip))deallocate(vip)
    ! Set status and return
    status = VGD_OK
    return
  end function get_int

  integer function get_int_1d(self,key,value,quiet) result(status)
    use utils, only: get_allocate,up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    character(len=*), intent(in) :: key                 !Descriptor key to retrieve
    integer, dimension(:), pointer :: value             !Retrieved value
    logical, optional, intent(in) :: quiet              !Do not print massages

    ! Internal variables
    integer :: istat,error
    integer :: level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_int_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VIP1')
       if (is_valid(self,ip1_m_valid)) then
          write(for_msg,*) 'depricated key '//trim(key)//', use VIPM instead'
          call msg(level_msg,VGD_PRFX//for_msg)
          return
       else
          write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (int 1D)'
          call msg(level_msg,VGD_PRFX//for_msg)
          return
       endif
    case ('VIPM')
       if (is_valid(self,ip1_m_valid)) then
          istat = get_allocate(key,value,size(self%ip1_m),ALLOW_RESHAPE,'(VIPM in get_int_1d)')
          if (istat /= 0) return
          value = self%ip1_m
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('VIPT')
       if (is_valid(self,ip1_t_valid_get)) then
          if (is_valid(self,ip1_t_valid)) then
             istat = get_allocate(key,value,size(self%ip1_t),ALLOW_RESHAPE,'(VIPT in get_int_1d)')
             if (istat /= 0) return
             value = self%ip1_t
          else
             istat = get_allocate(key,value,size(self%ip1_m),ALLOW_RESHAPE,'(VIPT in get_int_1d)')
             if (istat /= 0) return
             value = self%ip1_m
          endif
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (int 1D)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_int_1d

  integer function get_real(self,key,value,quiet) result(status)
    use utils, only: up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self   !Vertical descriptor instance
    character(len=*), intent(in) :: key          !Descriptor key to retrieve
    real, intent(out) :: value                   !Retrieved value
    logical, optional, intent(in) :: quiet       !Do not print massages

    ! Internal variables
    integer :: level_msg, iwork,kind
    real :: work
    logical :: my_quiet
    character(len=1) :: dum_S 

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_real'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('RC_1')
       if (is_valid(self,rcoef1_valid)) then
          value = self%rcoef1
       else
          value = get_error(key,my_quiet)
          return
       endif
    case ('RC_2')
       if (is_valid(self,rcoef2_valid)) then
          value = self%rcoef2
       else
          value = get_error(key,my_quiet)
          return
       endif
    case ('DHM ')
       if (is_valid(self,dhm_valid)) then
          iwork=self%ip1_m(size(self%ip1_m))          
          call convip(iwork,work,kind,-1,dum_S,.false.)
          value=work
       else
          value = get_error(key,my_quiet)
          return
       endif
    case ('DHT ')       
       iwork=vgd_print(self)
       if (is_valid(self,dht_valid)) then
          iwork=self%ip1_t(size(self%ip1_t))
          call convip(iwork,work,kind,-1,dum_S,.false.)
          value=work
       else
          value = get_error(key,my_quiet)
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (real)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_real

  integer function get_real_1d(self,key,value,quiet) result(status)
    use utils, only: get_allocate,up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to retrieve
    real, dimension(:), pointer :: value        !Retrieved value
    logical, optional, intent(in) :: quiet      !Do not print massages

    ! Internal variables
    integer :: k,istat,error,level_msg,kind
    integer, dimension(:), pointer :: vipt
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    nullify(vipt)

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_real_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VCDM')
       if (is_valid(self,ip1_m_valid)) then
          istat = get_allocate(key,value,size(self%ip1_m),ALLOW_RESHAPE,'(VCDM in get_real_1d)')
          if (istat /= 0) return
          kind = self%kind
          call convip(k,value(1),kind,0,'',.false.)
          do k=1,size(value)
             call convip(self%ip1_m(k),value(k),kind,-1,'',.false.)
          enddo
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('VCDT')
       if (is_valid(self,ip1_t_valid_get)) then
          if (is_valid(self,ip1_t_valid)) then
             istat=vgd_get(self,key='VIPT - level ip1 list (t)',value=vipt)
          else
             istat=vgd_get(self,key='VIPM - level ip1 list (m)',value=vipt)
          endif
          if (istat /= VGD_OK)then
             if(associated(vipt))deallocate(vipt)
             return
          endif
          istat = get_allocate(key,value,size(vipt),ALLOW_RESHAPE,'(VCDT in get_real_1d)')
          kind = self%kind
          call convip(k,value(1),kind,0,'',.false.)
          do k=1,size(vipt)
             call convip(vipt(k),value(k),kind,-1,'',.false.)
          enddo
          deallocate(vipt)
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('VCRD')
       if (is_valid(self,ip1_m_valid)) then
          write(for_msg,*) 'depricated key '//trim(key)//', use VCDM instead'
          call msg(level_msg,VGD_PRFX//for_msg)
          return
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (real 1D)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_real_1d

  integer function get_real8(self,key,value,quiet) result(status)
    use utils, only: up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to retrieve
    real(kind=8), intent(out) :: value          !Retrieved value
    logical, optional, intent(in) :: quiet      !Do not print massages

    ! Internal Variables
    integer :: level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_real8'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('PTOP')
       if (is_valid(self,ptop_8_valid)) then
          value = self%ptop_8
       else
          value = dble(get_error(key,my_quiet))
          return
       endif
    case ('PREF')
       if (is_valid(self,pref_8_valid)) then
          value = self%pref_8
       else
          value = dble(get_error(key,my_quiet))
          return
       endif
    case ('RC_1')
       if (is_valid(self,rcoef1_valid)) then
          value = self%rcoef1
       else
          value = dble(get_error(key,my_quiet))
          return
       endif
    case ('RC_2')
       if (is_valid(self,rcoef2_valid)) then
          value = self%rcoef2
       else
          value = dble(get_error(key,my_quiet))
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (real8)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_real8

  integer function get_real8_1d(self,key,value,quiet) result(status)
    use utils, only: get_allocate,up,get_error
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to retrieve
    real(kind=8), dimension(:), pointer :: value !Retrieved value
    logical, optional, intent(in) :: quiet      !Do not print massages

    ! Internal variables
    integer :: istat,error,level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_real8_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('CA_M')
       if (is_valid(self,a_m_8_valid)) then
          istat = get_allocate(key,value,size(self%a_m_8),ALLOW_RESHAPE,'(CA_M in get_real8_1d)')
          if (istat /= 0) return
          value = self%a_m_8
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('CB_M')
       if (is_valid(self,b_m_8_valid)) then
          istat = get_allocate(key,value,size(self%b_m_8),ALLOW_RESHAPE,'(CB_M in get_real8_1d)')
          if (istat /= 0) return
          value = self%b_m_8
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('CA_T')
       if (is_valid(self,a_t_8_valid_get)) then
          if(is_valid(self,a_t_8_valid)) then
             istat = get_allocate(key,value,size(self%a_t_8),ALLOW_RESHAPE,'(CA_T in get_real8_1d)')
             if (istat /= 0) return
             value = self%a_t_8
          else
             istat = get_allocate(key,value,size(self%a_m_8),ALLOW_RESHAPE,'(CA_T (m) in get_real8_1d)')
             if (istat /= 0) return
             value = self%a_m_8
          endif
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('CB_T')
       if (is_valid(self,b_t_8_valid_get)) then
          if (is_valid(self,b_t_8_valid)) then
             istat = get_allocate(key,value,size(self%b_t_8),ALLOW_RESHAPE,'(CB_T in get_real8_1d)')
             if (istat /= 0) return
             value = self%b_t_8
          else
             istat = get_allocate(key,value,size(self%b_m_8),ALLOW_RESHAPE,'(CB_T (m) in get_real8_1d)')
             if (istat /= 0) return
             value = self%b_m_8
          endif
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('COFA')
       if (is_valid(self,a_m_8_valid)) then
          istat = get_allocate(key,value,size(self%a_m_8),ALLOW_RESHAPE,'(COFA in get_real8_1d)')
          if (istat /= 0) return
          value = self%a_m_8
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case ('COFB')
       if (is_valid(self,b_m_8_valid)) then
          istat = get_allocate(key,value,size(self%b_m_8),ALLOW_RESHAPE,'(COFB in get_real8_1d)')
          if (istat /= 0) return
          value = self%b_m_8
       else
          error = int(get_error(key,my_quiet))
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (real8 1D)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_real8_1d

  integer function get_real8_3d(self,key,value,quiet) result(status)
    use utils, only: get_allocate,up
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to retrieve
    real(kind=8), dimension(:,:,:), pointer :: value !Retrieved value
    logical, optional, intent(in) :: quiet      !Do not print massages

    ! Internal variables
    integer :: istat,level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_real8_3d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VTBL')
       istat = get_allocate(key,value,shape(self%table),ALLOW_RESHAPE,'(VTBL) in get_real8_3d)')
       if (istat /= 0) return
       value = self%table
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (real8 3D)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_real8_3d

  integer function get_char(self,key,value,quiet) result(status)
    use utils, only: up,get_error,printingCharacters
    ! Retrieve the value of the requested instance variable
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to retrieve
    character(len=*), intent(out) :: value      !Retrieved value
    logical, optional, intent(in) :: quiet      !Do not print massages
    
    ! Local variables
    integer :: error,level_msg
    logical :: my_quiet

    ! Set error status
    status = VGD_ERROR

    level_msg=MSG_ERROR
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in get_char'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif
    my_quiet=.false.
    if (present(quiet)) my_quiet = quiet
    if(my_quiet)level_msg=MSG_QUIET

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('ETIK')
       value = self%rec%etiket
    case ('NAME')
       value = self%rec%nomvar
    case ('RFLD')
       if (is_valid(self,ref_name_valid)) then
          value = printingCharacters(self%ref_name)
       else
          error = get_error(key,my_quiet)
          write(for_msg,'(i8)') error
          call msg(level_msg,VGD_PRFX//for_msg)
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_get (char)'
       call msg(level_msg,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function get_char
  
  integer function getopt_logical(key,value,quiet) result(status)
     character(len=*), intent(in) :: key           !Descriptor key to retrieve
     logical, intent(out) :: value                    !Retrieved value
     logical, intent(in), optional :: quiet        !Do not generate messages

     ! Local variables
     integer :: level_msg
     logical :: my_quiet
     
    ! Set error status
     status=VGD_ERROR

     level_msg=MSG_ERROR
     my_quiet=.false.
     if (present(quiet)) my_quiet = quiet
     if(my_quiet)level_msg=MSG_QUIET
     
     select case(trim(key))
     case ('ALLOW_RESHAPE')
        value=ALLOW_RESHAPE
     case ('ALLOW_SIGMA')
        value=ALLOW_SIGMA
     case DEFAULT
        write(for_msg,*) 'invalid key in call to getopt_logical: ',trim(key)
        call msg(level_msg,VGD_PRFX//for_msg)
        return
     end select
     ! Set status and return
     status = VGD_OK
     return
  end function getopt_logical
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Put methods

  integer function put_int(self,key,value) result(status)
    use utils, only: up,comp_diag_a
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    integer, intent(in) :: value                !Value to set
    
    ! Local variables
    integer :: error

    ! Set error status
    status = VGD_ERROR
    
    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_int'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    
    
    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('DATE')
       self%rec%dateo = value
    case ('IG_1')
       self%rec%ig1 = value
    case ('IG_2')
       self%rec%ig2 = value
    case ('IG_3')
       self%rec%ig3 = value
    case ('IG_4')
       self%rec%ig4 = value
    case ('IP_1')
       self%rec%ip1 = value
    case ('IP_2')
       self%rec%ip2 = value
    case ('IP_3')
       self%rec%ip3 = value
    case ('DIPM')
       if (is_valid(self,dhm_valid)) then
          self%ip1_m(size(self%ip1_m))=value          
          self%a_m_8(size(self%a_m_8))=comp_diag_a(self%pref_8,value)
          error=table_update(self)
          if(error==VGD_ERROR)then
             write(for_msg,*) 'Problem with table_update for key '//trim(key)//' given to gd_put (int)'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
       else
          return
       endif
    case ('DIPT')
       if (is_valid(self,dht_valid)) then
          self%ip1_t(size(self%ip1_t))=value
          self%a_t_8(size(self%a_t_8))=comp_diag_a(self%pref_8,value)
          error=table_update(self)
          if(error==VGD_ERROR)then
             write(for_msg,*) 'Problem with table_update for key '//trim(key)//' given to gd_put (int)'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             return
          endif
        else
          return
       endif
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (int)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_int

  integer function put_int_1d(self,key,value) result(status)
    use utils, only: size_ok,up,put_error
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self       !Vertical descriptor instance
    character(len=*), intent(in) :: key                 !Descriptor key to set
    integer, dimension(:), pointer :: value             !Value to set

    ! Local variables
    
    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_int_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Check for valid value
    if (.not.associated(value)) then
       write(for_msg,*) 'unallocated value sent to gd_put for '//trim(key)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VIPM')
       if (.not.size_ok(self%ip1_m,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%ip1_m = value
       if (.not.is_valid(self,ip1_m_valid)) status = put_error(key)
    case ('VIPT')
       if (.not.size_ok(self%ip1_t,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%ip1_t = value
       if (.not.is_valid(self,ip1_t_valid)) status = put_error(key)
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (real8 1D)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_int_1d

  integer function put_real_1d(self,key,value) result(status)
    use utils, only: up
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self!Descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    real, dimension(:), pointer :: value !Value to set

    ! Local variables

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_real_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Check for valid value
    if (.not.associated(value)) then
       write(for_msg,*) 'unallocated value sent to gd_put for '//trim(key)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VCDM')
       write(for_msg,*) trim(key)//' cannot be set - modify VIPM instead'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    case ('VCDT')
       write(for_msg,*) trim(key)//' cannot be set - modify VIPT instead'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    case ('VCRD')
       write(for_msg,*) trim(key)//' cannot be set - modify VIPM instead'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (real8 1D)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_real_1d

  integer function put_real8(self,key,value) result(status)
    use utils, only: up,put_error
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self!Descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    real(kind=8), intent(in) :: value           !Value to set

    ! Local variables
    integer :: error

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_real8'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Map key name to derived-type element
    error = VGD_OK
    select case (up(key(1:KEY_LENGTH)))
    case ('PTOP')
       self%ptop_8 = value
       if (.not.is_valid(self,ptop_8_valid)) error = put_error(key)
    case ('PREF')
       self%pref_8 = value
       if (.not.is_valid(self,pref_8_valid)) error = put_error(key)
    case ('RC_1')
       self%rcoef1 = value
       if (.not.is_valid(self,rcoef1_valid)) error = put_error(key)
    case ('RC_2')
       self%rcoef2 = value
       if (.not.is_valid(self,rcoef2_valid)) error = put_error(key)
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (real8)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Check for sets of invalid values
    if (error /= VGD_OK) return

    ! Set status and return
    status = VGD_OK
    return
  end function put_real8

  integer function put_real8_1d(self,key,value) result(status)
    use utils, only: size_ok,up,put_error
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self!Descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    real(kind=8), dimension(:), pointer :: value !Value to set

    ! Local variables
    
    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_real8_1d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Check for valid value
    if (.not.associated(value)) then
       write(for_msg,*) 'unallocated value sent to gd_put for '//trim(key)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('CA_M')
       if (.not.size_ok(self%a_m_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%a_m_8 = value
       if (.not.is_valid(self,a_m_8_valid)) status = put_error(key)
    case ('CB_M')
       if (.not.size_ok(self%b_m_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%b_m_8 = value
       if (.not.is_valid(self,b_m_8_valid)) status = put_error(key)
    case ('CA_T')
       if (.not.size_ok(self%a_t_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%a_t_8 = value
       if (.not.is_valid(self,a_t_8_valid)) status = put_error(key)
    case ('CB_T')
       if (.not.size_ok(self%b_t_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%b_t_8 = value
       if (.not.is_valid(self,b_t_8_valid)) status = put_error(key)
    case ('COFA')
       if (.not.size_ok(self%a_m_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%a_m_8 = value
       if (.not.is_valid(self,a_m_8_valid)) status = put_error(key)
    case ('COFB')
       if (.not.size_ok(self%b_m_8,value)) then
          write(for_msg,*) 'value does not match '//trim(key)//' size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%b_m_8 = value
       if (.not.is_valid(self,b_m_8_valid)) status = put_error(key)
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (real8 1D)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_real8_1d

  integer function put_real8_3d(self,key,value) result(status)
    use utils, only: size_ok, up
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self!Descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    real(kind=8), dimension(:,:,:), pointer :: value !Value to set
    
    ! Internal variables

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_real8_3d'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Check for valid value
    if (.not.associated(value)) then
       write(for_msg,*) 'unallocated value sent to gd_put for'//trim(key)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('VTBL')
       if (.not.size_ok(self%table,value)) then
          write(for_msg,*) 'value does not match table size in gd_put'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       self%table = value
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (real8 3D)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_real8_3d

  integer function put_char(self,key,value) result(status)
    use utils, only: up,put_error
    ! Set the value of the requested instance variable
    type(vgrid_descriptor), intent(inout) :: self!Descriptor instance
    character(len=*), intent(in) :: key         !Descriptor key to set
    character(len=*), intent(in) :: value       !Value to set

    ! Internal variables
    integer :: error

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in put_char'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif    

    ! Map key name to derived-type element
    select case (up(key(1:KEY_LENGTH)))
    case ('ETIK')
       self%rec%etiket = value
    case ('NAME')
       self%rec%nomvar = value
    case ('RFLD')
       self%ref_name = value
       if (.not.is_valid(self,ref_name_valid)) error = put_error(key)
    case DEFAULT
       write(for_msg,*) 'invalid key '//trim(key)//' given to gd_put (char)'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function put_char

  integer function putopt_logical(key,value) result(status)
     character(len=*), intent(in) :: key           !Descriptor key to retrieve
     logical, intent(in) :: value                    !Retrieved value

     ! Local variables
     
    ! Set error status
     status=VGD_ERROR

     select case(trim(key))
     case ('ALLOW_RESHAPE')
        ALLOW_RESHAPE=value
     case ('ALLOW_SIGMA')
        ALLOW_SIGMA=value
     case DEFAULT
        write(for_msg,*) 'invalid key in call to putopt_logical: ',trim(key)
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     end select
     ! Set status and return
     status = VGD_OK
     return
  end function putopt_logical


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Dump contents

  integer function print_desc(self,stdout,convip_L) result(status)
    ! Dump plain text grid descriptor information to the requested file unit
    type(vgrid_descriptor), intent(in) :: self  !Vertical descriptor instance
    integer, intent(in), optional :: stdout     !Output unit to write to [6]
    logical, intent(in), optional :: convip_L

    ! Internal variables
    integer :: my_stdout,nk,k,kind
    character(len=64) :: hr
    logical :: my_convip_L
    real :: pres,height
    character(len=1) :: null_S

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in print_desc'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif

    my_convip_L=.false.
    if(present(convip_L))my_convip_L=convip_L

    ! Set default values
    my_stdout = 6
    if (present(stdout)) my_stdout = stdout
    
    ! Create horizontal rule
    hr = '-------------------------------------------------------'

    ! Dump general descriptor information
    write(for_msg,*) '-- Vertical Grid Descriptor Information --'
    call msg(MSG_VERBATIM,trim(for_msg))
    write(for_msg,*) '  ip1=',self%rec%ip1
    call msg(MSG_VERBATIM,trim(for_msg))
    write(for_msg,*) '  ip2=',self%rec%ip2
    call msg(MSG_VERBATIM,trim(for_msg))
    write(for_msg,*) '',trim(hr)
    call msg(MSG_VERBATIM,trim(for_msg))
    write(for_msg,*) 'Vcode=',self%vcode
    call msg(MSG_VERBATIM,trim(for_msg))

    ! Dump vertical descriptor information
    have_descriptor: if (found_descriptor(self)) then
       write(for_msg,*)'  Descriptor Nomvar: ',trim(self%rec%nomvar)
       call msg(MSG_VERBATIM,trim(for_msg))      
       write(for_msg,'(a,i2,a,i3)')'   level kind =',self%kind,', level version = ',self%version
       call msg(MSG_VERBATIM,trim(for_msg))
       if (is_valid(self,ptop_8_valid))then
          write(for_msg,*)'  ptop=',self%ptop_8,'Pa'
          call msg(MSG_VERBATIM,trim(for_msg))
       endif
       if (is_valid(self,pref_8_valid))then
          write(for_msg,*)'  pref=',self%pref_8,'Pa'
          call msg(MSG_VERBATIM,trim(for_msg))
       endif
       if (is_valid(self,rcoef1_valid))then
          write(for_msg,*)'  rcoef1=',self%rcoef1
          call msg(MSG_VERBATIM,trim(for_msg))
       endif
       if (is_valid(self,rcoef2_valid))then
          write(for_msg,*)'  rcoef2=',self%rcoef2
          call msg(MSG_VERBATIM,trim(for_msg))
       endif
       if (is_valid(self,ref_name_valid))then
          write(for_msg,*)'  Surface field nomvar ',self%ref_name
          call msg(MSG_VERBATIM,trim(for_msg))
       endif
       select case (self%vcode)
       case (1001)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of sigma levels',nk
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): pi = B * P0*100'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (1002)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of eta levels',nk
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100'
          call msg(MSG_VERBATIM,trim(for_msg))    
       case (1003)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of hybrid normalized levels',nk
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (2001)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of pressure levels',nk
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (P): P = A'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (5001)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of hybrid levels',nk
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): pi = A + B * P0*100'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (5002,5003)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of hybrid levels (momentum levels)',nk-1
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (5004)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of hybrid levels (momentum/thermo levels)',nk-1
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)'
          call msg(MSG_VERBATIM,trim(for_msg))
       case (5005)
          nk=size(self%ip1_m)
          write(for_msg,*)'  Number of hybrid levels (momentum/thermo levels)',nk-2          
          call msg(MSG_VERBATIM,trim(for_msg))
          call convip(self%ip1_m(nk),height,kind,-1,"",.false.)
          write(for_msg,*)'  Diagnostic momentum level (ip1=',self%ip1_m(nk),') at ',height,' m Above Ground Level'
          call msg(MSG_VERBATIM,trim(for_msg))
          call convip(self%ip1_t(nk),height,kind,-1,"",.false.)
          write(for_msg,*)'  Diagnostic thermo   level (ip1=',self%ip1_t(nk),') at ',height,' m Above Ground Level'          
          call msg(MSG_VERBATIM,trim(for_msg))
          write(for_msg,*)'  Equation to compute hydrostatic pressure (pi): ln(pi) = A + B * ln(P0*100/pref)'
          call msg(MSG_VERBATIM,trim(for_msg))
       end select

       if(my_convip_L)then
          if (is_valid(self,ip1_m_valid))then
             nk=size(self%ip1_m)
             write(for_msg,*)'  Momentum levels ip1, p, A, B:'
             call msg(MSG_VERBATIM,trim(for_msg))
             null_S=''
             do k=1,nk                
                call convip(self%ip1_m(k),pres,kind,-1,null_S,.false.)
                write(for_msg,*)self%ip1_m(k),pres,self%a_m_8(k),self%b_m_8(k)
                call msg(MSG_VERBATIM,trim(for_msg))
             enddo
          endif
          if (is_valid(self,ip1_t_valid))then
             nk=size(self%ip1_t)
             write(for_msg,*)'  Thermodynamic levels ip1, p, A, B:'
             call msg(MSG_VERBATIM,trim(for_msg))
             do k=1,nk
                call convip(self%ip1_t(k),pres,kind,-1,null_S,.false.)
                write(for_msg,*)self%ip1_t(k),pres,self%a_t_8(k),self%b_t_8(k)
                call msg(MSG_VERBATIM,trim(for_msg))
             enddo
          endif
       else
          if (is_valid(self,ip1_m_valid))then
             nk=size(self%ip1_m)
             write(for_msg,*)'  Momentum levels ip1, A, B:'
             call msg(MSG_VERBATIM,trim(for_msg))
             do k=1,nk
                write(for_msg,*)self%ip1_m(k),self%a_m_8(k),self%b_m_8(k)
                call msg(MSG_VERBATIM,trim(for_msg))
             enddo
          endif
          if (is_valid(self,ip1_t_valid))then
             nk=size(self%ip1_t)
             write(for_msg,*)'  Thermodynamic levels ip1, A, B:'
             call msg(MSG_VERBATIM,trim(for_msg))
             do k=1,nk
                write(for_msg,*)self%ip1_t(k),self%a_t_8(k),self%b_t_8(k)
                call msg(MSG_VERBATIM,trim(for_msg))
             enddo
          endif
       endif
    else
       write(for_msg,*) '* No Vertical Grid Descriptor Information Found'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif have_descriptor

    ! Set status and return
    status = VGD_OK
    return
  end function print_desc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer function print_vcode_description(vcode) result(status)
    ! Dump plain text vcode descriptor information to the requested file unit
    integer, intent(in) :: vcode

    ! Internal variables
    character(len=64) :: hr

    ! Set error status
    status = VGD_ERROR

     ! Create horizontal rule
    hr = '-------------------------------------------------------'
    
    if(vcode.eq.1001.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 1001, kind 1, version 1'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Sigma levels'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.1002.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 1002, kind 1, version 2'
       call msg(MSG_VERBATIM,trim(for_msg))    
       write(for_msg,*)'   Eta levels'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.1003.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 1003, kind 1, version 3'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid normalized levels'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.2001.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 2001, kind 2, version 1'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Pressure levels'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.5001.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 5001, kind 5, version 1'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid levels, unstaggered'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.5002.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 5002, kind 5, version 2'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid staggered levels, nk momentum levels, nk+1 thermo levels'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   First level at top is a thermo level'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.5003.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 5003, kind 5, version 3'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid staggered levels, nk momentum levels, nk+1 thermo levels'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   First level at top is a thermo level'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Last thermo level is unstaggered (tlift)'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.5004.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 5004, kind 5, version 4'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid staggered levels, same number of momentum and themro levels'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   First level at top is a momentum level'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif
    if(vcode.eq.5005.or.vcode==-1)then
       write(for_msg,*)hr
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'Vcode 5005, kind 5, version 5'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Hybrid staggered levels, same number of momentum and themro levels'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   First level at top is a momentum level'
       call msg(MSG_VERBATIM,trim(for_msg))
       write(for_msg,*)'   Diag level heights (m AGL) encoded'
       call msg(MSG_VERBATIM,trim(for_msg))
    endif

    write(for_msg,*)hr
    call msg(MSG_VERBATIM,trim(for_msg))
    
    ! Set status and return
    status = VGD_OK
    return
 end function print_vcode_description

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Write descriptors
  
  integer function write_desc(self,unit,format) result(status)     
    use utils, only: up
    ! Write descriptors to the requested file
    type(vgrid_descriptor), intent(in) :: self       !Vertical descriptor instance
    integer, intent(in) :: unit                      !File unit to write to
    character(len=*), optional, intent(in) :: format !File format ('fst' or 'bin' ) default is 'fst'

    ! Local variables
    integer :: error,ni,nj,nk,fstecr
    real :: dummy
    character(len=100) :: myformat

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in write_desc'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif

    ! Set default values                                                                                  
    myformat='FST'                                                                                        
    if (present(format)) myformat = format 
    
    ! Array size from table
    ni=size(self%table,dim=1)
    nj=size(self%table,dim=2)
    nk=size(self%table,dim=3)

    ! Write to the desired output file type
    select case (trim(up(myformat)))

    ! Write to an RPN Standard file
    case ('FST')
    error=fstecr(self%table,dummy,-self%rec%nbits,unit, &
         self%rec%dateo,self%rec%deet,self%rec%npas, &
         ni,nj,nk,self%rec%ip1,self%rec%ip2,self%rec%ip3, &
         self%rec%typvar,self%rec%nomvar,self%rec%etiket,self%rec%grtyp, &
         self%rec%ig1,self%rec%ig2,self%rec%ig3,self%rec%ig4,self%rec%datyp,.true.)
    if(error < 0)then
       write(for_msg,*) 'problem with fstecr in write_desc'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Write to a Fortran binary file
    case ('BIN')
       
       write(unit) ni,nj,nk
       write(unit) self%table

    ! Warn user for unknown format
    case DEFAULT
       write(for_msg,*) 'No write done for unknown format ',trim(myformat)
       call msg(MSG_WARNING,VGD_PRFX//for_msg)
    end select

    ! Set status and return
    status = VGD_OK
    return
  end function write_desc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Compute vertical levelling

  integer function levels_toplevel(unit,fstkeys,levels,in_log) result(status)
    ! Top-level interface for computing physical levelling information
    integer, intent(in) :: unit                         !File unit associated with the key
    integer, dimension(:), intent(in) :: fstkeys        !Key of prototype field
    real, dimension(:,:,:), pointer :: levels           !Physical level values
    logical, optional, intent(in) :: in_log             !Compute levels in ln() [.false.]

    ! Local variables
    real :: lev
    integer :: ip1,kind
    character(len=1) :: blk_S
    integer :: error,ig1,ig2,ig3,i
    character(len=1) :: grtyp
    logical :: multiple_grids,my_in_log
    type(vgrid_descriptor) :: gd
    type(FSTD_ext) :: var

    ! Set error status
    status = VGD_ERROR

    ! Set default values
    my_in_log = .false.
    if (present(in_log)) my_in_log = in_log

    ! Construct appropriate grid_descriptor object (rebuild if new ig1-3 values are found)
    multiple_grids = .false.
    grids: do i=1,size(fstkeys)
       error = my_fstprm(fstkeys(i),var)
       if (error /= VGD_OK) then
          write(for_msg,*) 'return from fstprm wrapper for fst key ',fstkeys(i)
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (i==1) then
          ig1 = var%ig1
          ig2 = var%ig2
          ig3 = var%ig3
          grtyp = var%grtyp(1:1)
          ip1 = var%ip1
       endif
       if (var%ig1 /= ig1 .or. var%ig2 /= ig2 .or. var%ig3 /= ig3 .or. trim(var%grtyp(1:1)) /= trim(grtyp)) then
          write(for_msg,*) 'multiple grids defined in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo grids

    call convip (ip1, lev, kind,-1, blk_S, .false.)
    ! Create grid descriptor instance and call level calculator
    if (any(MATCH_GRTYP == grtyp)) then
       error = new_read(gd,unit=unit,format='fst',ip1=ig1,ip2=ig2,kind=kind)
       if(error==VGD_ERROR)then
          error = new_read(gd,unit=unit,format='fst',                kind=kind)
       endif
    else
       error = new_read(gd,unit=unit,format='fst',ip1=-1,ip2=-1,kind=kind)
    endif
    if (error /= VGD_OK) then
       write(for_msg,*) 'cannot build grid descriptor instance for fst key ',fstkeys(1)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    error = levels_readref(gd,unit=unit,fstkeys=fstkeys,levels=levels,in_log=my_in_log)
    if (error /= VGD_OK) then
       write(for_msg,*) 'problem computing level information for fst key ',fstkeys(1)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Set status and return
    status = VGD_OK

    return
  end function levels_toplevel

  integer function levels_readref(self,unit,fstkeys,levels,in_log) result(status)
    ! Reading referent, compute physical levelling information from the vertical description
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    integer, intent(in) :: unit                         !File unit associated with the key
    integer, dimension(:), intent(in) :: fstkeys        !Key of prototype field
    real, dimension(:,:,:), pointer :: levels           !Physical level values
    logical, optional, intent(in) :: in_log             !Compute levels in ln() [.false.]

    ! Internal variables
    integer :: fstinf,ni,nj,nk,sfc_key,istat,error,fstluk,i
    integer, dimension(size(fstkeys)) :: ip1_list
    type(FSTD_ext) prmk,prm_p0,prm_check
    real, dimension(:,:), pointer :: p0
    logical :: my_in_log, relax_ipig_match_L

    nullify(p0)
    
    relax_ipig_match_L=.false.

    ! Set error status
    status = VGD_ERROR

    if(.not.self%valid)then
       write(for_msg,*) 'vgrid structure is not valid in levels_readref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)       
       return
    endif

    ! Set default values
    my_in_log = .false.
    if (present(in_log)) my_in_log=in_log

    ! Check input keys
    if(size(fstkeys)<1)then
       write(for_msg,*) 'fstkeys vector passed to vgd_levels is null'                             
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return                                                                
    endif
    istat=my_fstprm(fstkeys(1),prmk)
    if(istat < 0)then
       write(for_msg,*) 'problem with my_fstprm in levels_readref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
    endif
    check_times: do i=1,size(fstkeys)
       istat=my_fstprm(fstkeys(i),prm_check)
       ip1_list(i) = prm_check%ip1
       if (prm_check%datev /= prmk%datev) then
          write(for_msg,*) 'multiple valid times given in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (prm_check%ig1 /= prmk%ig1 .or. prm_check%ig2 /= prmk%ig2) then
          write(for_msg,*) 'multiple grids given in fstkeys vector'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo check_times
    if (self%match_ipig) then
       if (prmk%ig1 /= self%rec%ip1 .or. prmk%ig2 /= self%rec%ip2) then
          write(for_msg,*) 'fstkeys do not correspond to the correct grid descriptor'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,'("   expecting (ip1,ip2)->(",i8,",",i8,"), got (ig1,ig2)->(",i8,",",i8,")")')&
          prmk%ig1,prmk%ig2,self%rec%ip1,self%rec%ip2
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    endif

    ! Check surface field if needed
    sfc_valid: if (is_valid(self,ref_name_valid)) then
       sfc_key = fstinf(unit,ni,nj,nk,prmk%datev,prmk%etiket,-1,prmk%ip2,prmk%ip3,' ',self%ref_name)
       if(sfc_key < 0)then
          write(for_msg,*) 'cannot find ',self%ref_name,' for :'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,*) 'datev=',prmk%datev,' etiket=',prmk%etiket,' ip2=',prmk%ip2,' ip3=',prmk%ip3
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return   
       endif
       istat=my_fstprm(sfc_key,prm_p0)
       if(prm_p0%ni.ne.prmk%ni.or.prm_p0%nj.ne.prmk%nj)then
          write(for_msg,*) 'horizontal grid mismatch for '//trim(self%ref_name),ni,nj,' vs',prmk%ni,prmk%nj
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if (self%match_ipig) then
          if (prm_p0%ig1 /= self%rec%ip1 .or. prm_p0%ig2 /= self%rec%ip2) then
             write(for_msg,*) 'sfc_field ig1 ig2 do not correspond to the correct grid descriptor'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             write(for_msg,'("   expecting (ip1,ip2)->(",i8,",",i8,"), got (ig1,ig2)->(",i8,",",i8,")")')&
                  self%rec%ip1,self%rec%ip2,prm_p0%ig1,prm_p0%ig2
             call msg(MSG_ERROR,VGD_PRFX//for_msg)                            
             return
          endif
       endif
       allocate(p0(ni,nj),stat=error)
       if (error /= 0) then
          nullify(p0)
          write(for_msg,*) 'cannot allocate space for p0 in levels_readref'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       error = fstluk(p0,sfc_key,ni,nj,nk)
       if(error < 0 )then
          write(for_msg,*) 'problem with fstluk '//trim(self%ref_name)//' in levels_readref'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          deallocate(p0)
          return
       endif
       if (trim(self%ref_name) == 'P0') p0 = p0*100. !convert mb to Pa
    else
       allocate(p0(1,1),stat=error)
       if (error /= 0) then
          nullify(p0)
          write(for_msg,*) 'cannot allocate space for p0 in levels_readref'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       p0 = VGD_MISSING
    endif sfc_valid
   
    ! Wrap call to level calculator
    error = levels_withref(self,sfc_field=p0,ip1_list=ip1_list,levels=levels,in_log=my_in_log)
    deallocate(p0)
    if (error /= VGD_OK) then
       write(for_msg,*) 'got error return from levels_withref in levels_readref'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    ! Set status and return
    status = VGD_OK
    return
  end function levels_readref

  integer function levels_withref_prof(self,ip1_list,levels,sfc_field,in_log) result(status)
     use utils, only: get_allocate
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real, dimension(:), pointer :: levels                       !Physical level values
     real, optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          

     ! Local variables
     real*8 :: my_sfc_field_8
     real*8, dimension(:), pointer :: levels_8
     integer :: error,stat
     logical :: my_in_log

     nullify(levels_8)

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in levels_withref_prof'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     my_sfc_field_8 = VGD_MISSING
     if (present(sfc_field)) my_sfc_field_8 = sfc_field
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculation
     stat = diag_withref_prof_8(self,ip1_list,levels_8,sfc_field=my_sfc_field_8,in_log=my_in_log)
     if(stat==VGD_ERROR)then
        if(associated(levels_8))deallocate(levels_8)
        return
     endif
     ! Write results back to 32 bits
     error = get_allocate('levels',levels,size(levels_8),ALLOW_RESHAPE,'(in levels_withref_prof)')
     if (error /= 0) then
        if(associated(levels_8))deallocate(levels_8)
        return
     endif
     levels=levels_8
     deallocate(levels_8)
     status=VGD_OK
     return
  end function levels_withref_prof

  integer function levels_withref_prof_8(self,ip1_list,levels,sfc_field,in_log) result(status)
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real*8, dimension(:), pointer :: levels                       !Physical level values
     real*8, optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          

     ! Local variables
     real*8 :: my_sfc_field
     logical :: my_in_log

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in levels_withref_prof_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculation
     status = diag_withref_prof_8(self,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
     return
  end function levels_withref_prof_8

  integer function dpidpis_withref_prof(self,ip1_list,dpidpis,sfc_field) result(status)
     use utils, only: get_allocate,up
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real, dimension(:), pointer :: dpidpis                      !Derivative values
     real, optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]

     ! Local variables
     real*8 :: my_sfc_field_8
     real*8, dimension(:), pointer :: dpidpis_8
     integer :: error,stat

     nullify(dpidpis_8)

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in dpidpis_withref_prof'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     my_sfc_field_8 = VGD_MISSING
     if (present(sfc_field)) my_sfc_field_8 = sfc_field
     
     ! Wrap call to level calculation
     stat = diag_withref_prof_8(self,ip1_list,dpidpis_8,sfc_field=my_sfc_field_8,dpidpis=.true.)
     if(stat==VGD_ERROR)then
        if(associated(dpidpis_8))deallocate(dpidpis_8)
        return
     endif
     error = get_allocate('dpidpis',dpidpis,size(dpidpis_8),ALLOW_RESHAPE,'(in dpidpis_withref_prof)')
     if (error /= 0)then
        if(associated(dpidpis_8))deallocate(dpidpis_8)
        return
     endif
     dpidpis=dpidpis_8
     deallocate(dpidpis_8)
     status=VGD_OK
     return
  end function dpidpis_withref_prof

  integer function dpidpis_withref_prof_8(self,ip1_list,dpidpis,sfc_field) result(status)
     use utils, only: get_allocate,up
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real*8, dimension(:), pointer :: dpidpis                      !Derivative values
     real*8, optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]

     ! Local variables
     real*8 :: my_sfc_field_8
     integer :: stat

     ! Set return value
     status = VGD_ERROR
     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in dpidpis_withref_prof_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif
     ! Set default values
     my_sfc_field_8 = VGD_MISSING
     if (present(sfc_field)) my_sfc_field_8 = sfc_field
     ! Wrap call to level calculation
     stat = diag_withref_prof_8(self,ip1_list,dpidpis,sfc_field=my_sfc_field_8,dpidpis=.true.)
     if(stat==VGD_ERROR)then
        write(for_msg,*) 'ERROR with diag_withref_prof_8 in dpidpis_withref_prof_8'
        return
     endif
     status = VGD_OK
     return
  end function dpidpis_withref_prof_8

  integer function diag_withref_prof_8(self,ip1_list,levels,sfc_field,in_log,dpidpis) result(status)
     use utils, only: get_allocate
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real*8, dimension(:), pointer :: levels                       !Physical level values
     real*8, optional, intent(in) :: sfc_field                     !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]          
     logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
                                                                 !   respect to surface hydrostatic pressure(pis) [.false.]
     
     ! Local variables
     integer :: error,nk
     real*8 :: my_sfc_field
     real*8, dimension(:,:), pointer :: sfc_field_2d
     real*8, dimension(:,:,:), pointer :: levels_3d
     logical :: my_in_log,my_dpidpis

     ! Set error status
     status = VGD_ERROR

     nullify(sfc_field_2d,levels_3d)

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in diag_withref_prof_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     my_sfc_field = VGD_MISSING
     if (present(sfc_field)) my_sfc_field = sfc_field
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log
     my_dpidpis = .false.
     if (present(dpidpis)) my_dpidpis = dpidpis

     nk=size(ip1_list)

     allocate(sfc_field_2d(1,1),levels_3d(1,1,nk),stat=error)
     if (error /= 0) then
        if(associated(sfc_field_2d))deallocate(sfc_field_2d)
        if(associated(levels_3d))deallocate(levels_3d)
        write(for_msg,*) 'cannot allocate space for p0/levels in diag_withref_prof_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     sfc_field_2d=my_sfc_field
     ! Wrap call to level calculator    
     error = diag_withref_8(self,sfc_field=sfc_field_2d,ip1_list=ip1_list,levels=levels_3d,in_log=my_in_log,dpidpis=my_dpidpis)    
     if (error /= 0) then
        deallocate(sfc_field_2d,levels_3d)
        write(for_msg,*) 'problem with diag_withref in diag_withref_prof_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = get_allocate('levels',levels,nk,ALLOW_RESHAPE,'(in diag_withref_prof_8)')
     if(error/=0)return
     levels=levels_3d(1,1,1:nk)
     deallocate(sfc_field_2d,levels_3d)
     ! Set status and return
     status = VGD_OK
     return

  end function diag_withref_prof_8

  integer function levels_withref(self,ip1_list,levels,sfc_field,in_log) result(status)
     use utils, only: get_allocate
     ! Given referent, compute physical levelling information from the vertical description
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real, dimension(:,:,:), pointer :: levels                   !Physical level values
     real, dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]

     ! Local variables
     integer :: error,ni,nj,stat
     real*8, dimension(:,:,:), pointer :: levels_8
     real*8, dimension(:,:), pointer :: my_sfc_field
     logical :: my_in_log

     nullify(levels_8,my_sfc_field)

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in levels_withref'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     if (present(sfc_field)) then
        ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
     else
        ni = 1; nj = 1
     endif
     allocate(my_sfc_field(ni,nj),stat=error)
     if (error /= 0) then
        write(for_msg,*) 'cannot allocate space for my_sfc_field in levels_withref'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (present(sfc_field)) then
        my_sfc_field = sfc_field
     else
        my_sfc_field = VGD_MISSING
     endif
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculator at 64 bits
     stat=diag_withref_8(self,ip1_list,levels_8,sfc_field=my_sfc_field,in_log=my_in_log)
     if(stat==VGD_ERROR)then
        if(associated(levels_8))deallocate(levels_8)
        deallocate(my_sfc_field)
        return
     endif
     ! Write results back to 32 bits
     error = get_allocate('levels',levels,shape(levels_8),ALLOW_RESHAPE,'(in levels_withref)')
     if (error /= 0) then
        return
     endif
     levels=levels_8
     deallocate(my_sfc_field,levels_8)
     status=VGD_OK
     return
  end function levels_withref

  integer function levels_withref_8(self,ip1_list,levels,sfc_field,in_log) result(status)
     ! Given referent, compute physical levelling information from the vertical description
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real*8, dimension(:,:,:), pointer :: levels                   !Physical level values
     real*8, dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
     logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]

     ! Local variables
     integer :: error,ni,nj
     real*8, dimension(:,:), allocatable :: my_sfc_field
     logical :: my_in_log

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in levels_withref_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     if (present(sfc_field)) then
        ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
     else
        ni = 1; nj = 1
     endif
     allocate(my_sfc_field(ni,nj),stat=error)
     if (error /= 0) then
        write(for_msg,*) 'cannot allocate space for sfc_field in levels_withref'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (present(sfc_field)) then
        my_sfc_field = sfc_field
     else
        my_sfc_field = VGD_MISSING
     endif
     my_in_log = .false.
     if (present(in_log)) my_in_log = in_log

     ! Wrap call to level calculator
     status=diag_withref_8(self,ip1_list,levels,sfc_field=my_sfc_field,in_log=my_in_log)
     deallocate(my_sfc_field)
     return
  end function levels_withref_8

  integer function dpidpis_withref(self,ip1_list,dpidpis,sfc_field) result(status)
     use utils, only: get_allocate
     ! Given referent, compute physical levelling information from the vertical description
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real, dimension(:,:,:), pointer :: dpidpis                  !pressure derivative with respect to sfc pressure
     real, dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]

     ! Local variables 
     integer :: error,ni,nj,stat
     real*8, dimension(:,:), pointer :: my_sfc_field_8
     real*8, dimension(:,:,:), pointer :: dpidpis_8

     nullify(my_sfc_field_8,dpidpis_8)

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in dpidpis_withref'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     if (present(sfc_field)) then
        ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
     else
        ni = 1; nj = 1
     endif
     allocate(my_sfc_field_8(ni,nj),stat=error)
     if (error /= 0) then
        nullify(my_sfc_field_8)
        write(for_msg,*) 'cannot allocate space for sfc_field in dpidpis_withref'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (present(sfc_field)) then
        my_sfc_field_8 = sfc_field
     else
        my_sfc_field_8 = VGD_MISSING
     endif
     ! Wrap call to level calculator
     stat=diag_withref_8(self,ip1_list,dpidpis_8,sfc_field=my_sfc_field_8,dpidpis=.true.)
     if(stat==VGD_ERROR)then
        if(associated(my_sfc_field_8))deallocate(my_sfc_field_8)
        if(associated(dpidpis_8))deallocate(dpidpis_8)
        return
     endif
     ! Write results back to 32 bits
     error = get_allocate('dpidpis',dpidpis,shape(dpidpis_8),ALLOW_RESHAPE,'(in dpidpis_withref)')
     if (error /= 0) then
        return
     endif
     dpidpis=dpidpis_8
     deallocate(my_sfc_field_8,dpidpis_8)
     status=VGD_OK
     return
  end function dpidpis_withref

  integer function dpidpis_withref_8(self,ip1_list,dpidpis,sfc_field) result(status)
     use utils, only: get_allocate
     ! Given referent, compute physical levelling information from the vertical description
     type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
     integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
     real*8, dimension(:,:,:), pointer :: dpidpis                !pressure derivative with respect to sfc pressure
     real*8, dimension(:,:), optional, intent(in) :: sfc_field   !Surface field reference for coordinate [none]

     ! Local variables 
     integer :: ni,nj,error,stat
     real*8, dimension(:,:), pointer :: my_sfc_field_8

     nullify(my_sfc_field_8)

     ! Set return value
     status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in dpidpis_withref_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

     ! Set default values
     if (present(sfc_field)) then
        ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2)
     else
        ni = 1; nj = 1
     endif
     allocate(my_sfc_field_8(ni,nj),stat=error)
     if (error /= 0) then
        nullify(my_sfc_field_8)
        write(for_msg,*) 'cannot allocate space for sfc_field in dpidpis_withref_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (present(sfc_field)) then
        my_sfc_field_8 = sfc_field
     else
        my_sfc_field_8 = VGD_MISSING
     endif
     ! Wrap call to level calculator
     stat=diag_withref_8(self,ip1_list,dpidpis,sfc_field=my_sfc_field_8,dpidpis=.true.)
     if(stat==VGD_ERROR)then
        deallocate(my_sfc_field_8)
        return
     endif
     deallocate(my_sfc_field_8)
     status=VGD_OK
     return
  end function dpidpis_withref_8

 integer function diag_withref_8(self,ip1_list,levels,sfc_field,in_log,dpidpis) result(status)
    ! Given referent, compute physical levelling information from the vertical description
    type(vgrid_descriptor), intent(in) :: self                  !Vertical descriptor instance
    integer, dimension(:), intent(in) :: ip1_list               !Key of prototype field
    real*8, dimension(:,:,:), pointer :: levels                   !Physical level values
    real*8, dimension(:,:), optional, intent(in) :: sfc_field     !Surface field reference for coordinate [none]
    logical, optional, intent(in) :: in_log                     !Compute levels in ln() [.false.]
    logical, optional, intent(in) :: dpidpis                    !Compute partial derivative of hydrostatic pressure (pi) with
                                                                !   respect to surface hydrostatic pressure(pis) [.false.]
   
    ! Local variables
    integer kind,version,istat,ni,nj,nk,error
    logical :: my_in_log, my_dpidpis

    ! Set error status
    status = VGD_ERROR

     if(.not.self%valid)then
        write(for_msg,*) 'vgrid structure is not valid in diag_withref_8'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)       
        return
     endif

    ! Set default values
    my_in_log = .false.
    if (present(in_log)) my_in_log = in_log
    my_dpidpis = .false.
    if (present(dpidpis)) my_dpidpis = dpidpis
    
    ! Set size of output and allocate space
    if (present(sfc_field)) then
       ni = size(sfc_field,dim=1); nj = size(sfc_field,dim=2); nk = size(ip1_list)
    else
      if (is_valid(self,ref_name_valid)) then
         write(for_msg,*) 'reference field must be provided to diag_withref_8'
         call msg(MSG_ERROR,VGD_PRFX//for_msg)
         return
      else
         ni = 1; nj = 1; nk = size(ip1_list)
      endif
    endif
    if (associated(levels)) then
       if (size(levels,dim=1) /= ni .or. size(levels,dim=2) /= nj .or. size(levels,dim=3) /= nk) then
          write(for_msg,*) 'Levels array size error - will be reallocated'
          call msg(MSG_WARNING,VGD_PRFX//for_msg)
       endif
       deallocate(levels)
    endif
    allocate(levels(ni,nj,nk),stat=error)
    if (error /= 0) then
       write(for_msg,*) 'cannot allocate space for levels in diag_withref_8'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    ! Compute levels for known vertical coordinates
    select case (self%vcode)
    case (1001)
       if(my_dpidpis)then
          write(for_msg,*) 'dpidpis not implemented for vertical coordinate 1001'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return 
       endif
       istat = compute_pressure_1001_8(self,sfc_field,ip1_list,levels,my_in_log)
    case (1002)
       if(my_dpidpis)then
          write(for_msg,*) 'dpidpis not implemented for vertical coordinate 1002'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return 
       endif
       istat = compute_pressure_1002_8(self,sfc_field,ip1_list,levels,my_in_log)
    case (2001)
       if(my_dpidpis)then
          write(for_msg,*) 'dpidpis not implemented for vertical coordinate 2001'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return 
       endif
       istat = compute_pressure_2001_8(self,ip1_list,levels,my_in_log)
    case (1003,5001)
       istat = compute_pressure_5001_8(self,sfc_field,ip1_list,levels,my_in_log,my_dpidpis)
    case (5002,5003,5004,5005)
       istat = compute_pressure_5002_8(self,sfc_field,ip1_list,levels,my_in_log,my_dpidpis)
    case DEFAULT
       istat = get_version_info(self,kind,version)
       write(for_msg,*) 'kind or version invalid in diag_withref_8:',kind,version
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return 
    end select
    if (istat /= VGD_OK) then
       if(my_dpidpis)then
          write(for_msg,*) 'error computing dpidpis in diag_withref_8'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
       else
          write(for_msg,*) 'error computing pressure in diag_withref_8'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
       endif
       return
    endif

    ! Set status and return
    status = VGD_OK
    return
 end function diag_withref_8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Test vgrid_descriptor type for equality

 logical function test_equality(vgd1,vgd2) result(equal)
   use utils, only: same_vec
   ! Determine whether a given pair of vgrid_descriptor structures are identical
   type(vgrid_descriptor), intent(in) :: vgd1,vgd2      !vertical grid descriptors to compare

   ! Local variables

   ! Assume that structures are not identical
   equal = .false.

   ! Check each element of the structure (except FST attributes) for equality
   if (vgd1%vcode /= vgd2%vcode) return
   if (vgd1%kind /= vgd2%kind) return
   if (vgd1%version /= vgd2%version) return
   if (vgd1%ref_name /= vgd2%ref_name) return
   if (vgd1%ptop_8 /= vgd2%ptop_8) return
   if (vgd1%pref_8 /= vgd2%pref_8) return
   if (vgd1%rcoef1 /= vgd2%rcoef1) return
   if (vgd1%rcoef2 /= vgd2%rcoef2) return

   ! Check pointer associations and values
   if (.not.same_vec(vgd1%ip1_m,vgd2%ip1_m)) return
   if (.not.same_vec(vgd1%ip1_t,vgd2%ip1_t)) return
   if (.not.same_vec(vgd1%a_m_8,vgd2%a_m_8)) return
   if (.not.same_vec(vgd1%b_m_8,vgd2%b_m_8)) return
   if (.not.same_vec(vgd1%a_t_8,vgd2%a_t_8)) return
   if (.not.same_vec(vgd1%b_t_8,vgd2%b_t_8)) return
   if (.not.same_vec(vgd1%table,vgd2%table)) return

   ! The full structure is equivalent
   equal = .true.
   return
 end function test_equality

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Encoding functions

  integer function encode_vert_1001(self,nk) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance
     integer, intent(in) :: nk                          !Number of levels

     ! Local variables
     integer :: nn,error,k,ind
     integer, parameter :: skip=2
     real*8 :: for_char_8

     ! Set error status
     status = VGD_ERROR

     ! Allocate table space
     if (associated(self%table)) deallocate(self%table)         
     allocate(self%table(3,nk+skip,1),stat=error)
     if(error < 0)then
        write(for_msg,*) 'cannot allocate self%table in encode_vert_1001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     
     ! Associate reference field name
     self%ref_name='P0'

     ! Vector size checks
     nn=size(self%ip1_m)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for ip1_m, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%a_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for a_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%b_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for b_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = flip_transfer(self%ref_name,for_char_8)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from encode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Fill header
     self%table(1:3,1,1)=(/dble(self%kind),dble(self%version),dble(skip)/)
     self%table(1:3,2,1)=(/for_char_8     , 0.d0             ,0.d0/)

     ! Fill level data
     do k=1,nk
        ind=k+skip
        self%table(1:3,ind,1)=(/dble(self%ip1_m(k)),self%a_m_8(k),self%b_m_8(k)/)
     enddo     

     ! Set status and return
     status = VGD_OK
  end function encode_vert_1001

  integer function encode_vert_1002(self,nk) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance
     integer, intent(in) :: nk                          !Number of levels

     ! Local variables
     integer :: nn,error,k,ind
     integer, parameter :: skip=2
     real*8 :: for_char_8

     ! Set error status
     status = VGD_ERROR

     ! Allocate table space
     if (associated(self%table)) deallocate(self%table)         
     allocate(self%table(3,nk+skip,1),stat=error)
     if(error < 0)then
        write(for_msg,*) 'cannot allocate self%table in encode_vert_1002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     
     ! Associate reference field name
     self%ref_name='P0'

     ! Vector size checks
     nn=size(self%ip1_m)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for ip1_m, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%a_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for a_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%b_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for b_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = flip_transfer(self%ref_name,for_char_8)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from encode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Fill header
     self%table(1:3,1,1)=(/dble(self%kind),dble(self%version),dble(skip)/)
     self%table(1:3,2,1)=(/self%ptop_8    ,for_char_8, 0.d0/)

     ! Fill level data
     do k=1,nk
        ind=k+skip
        self%table(1:3,ind,1)=(/dble(self%ip1_m(k)),self%a_m_8(k),self%b_m_8(k)/)
     enddo     

     ! Set status and return
     status = VGD_OK
  end function encode_vert_1002

  integer function encode_vert_2001(self) result(status)
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance

     ! Local variables
     integer :: nn,nk,k,ind,error
     integer, parameter :: skip=1

     ! Set error status
     status = VGD_ERROR

     ! Allocate table space
     nk = size(self%a_m_8)
     if (associated(self%table)) deallocate(self%table)         
     allocate(self%table(3,nk+skip,1),stat=error)
     if(error < 0)then
        write(for_msg,*) 'cannot allocate self%table in encode_vert_2001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Vector size checks
     nn=size(self%a_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for a_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     
     ! Fill header
     self%table(1:3,1,1)=(/dble(self%kind),dble(self%version),dble(skip)/)

     ! Fill pressure level data
     do k=1,nk
        ind=k+skip
        self%table(1:3,ind,1)=(/dble(self%ip1_m(k)),self%a_m_8(k),self%b_m_8(k)/)
     enddo

     ! Set status and return
     status = VGD_OK
     return
   end function encode_vert_2001

  integer function encode_vert_5001(self,nk) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance
     integer, intent(in) :: nk                          !Number of levels

     ! Local variables
     integer :: nn,error,k,ind
     integer, parameter :: skip=3
     real*8 :: for_char_8

     ! Set error status
     status = VGD_ERROR

     ! Allocate table space
     if (associated(self%table)) deallocate(self%table)         
     allocate(self%table(3,nk+skip,1),stat=error)
     if(error < 0)then
        write(for_msg,*) 'cannot allocate self%table in encode_vert_5001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Associate reference field name
     self%ref_name='P0'
     
     ! Vector size checks
     nn=size(self%ip1_m)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for ip1_m, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%a_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for a_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%b_m_8)
     if(nn.ne.nk)then
        write(for_msg,*) 'wrong size for b_m_8, is ',nn,'should be ',nk
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = flip_transfer(self%ref_name,for_char_8)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from encode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Fill header
     self%table(1:3,1,1)=(/dble(self%kind),dble(self%version),dble(skip)/)
     self%table(1:3,2,1)=(/self%ptop_8    ,self%pref_8       ,dble(self%rcoef1)/)     
     self%table(1:3,3,1)=(/for_char_8     ,0.d0              ,0.d0/)

     ! Fill level data
     do k=1,nk
        ind=k+skip
        self%table(1:3,ind,1)=(/dble(self%ip1_m(k)),self%a_m_8(k),self%b_m_8(k)/)
     enddo     

     ! Set status and return
     status = VGD_OK
  end function encode_vert_5001

  Integer function encode_vert_5002(self,F_nk,update_L) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance
     integer, intent(in), optional :: F_nk              !Number of levels
     logical, intent(in), optional :: update_L          !Update table

     ! Local variables
     integer :: nn,error,k,ind,k_plus_top,k_plus_diag,nb,nk
     integer, parameter :: skip=3
     real*8 :: for_char_8
     character(len=8) :: ref_name
     logical :: my_update_L

     ! Set error status
     status = VGD_ERROR

     my_update_L=.false.
     if(present(update_L))my_update_L=update_L
     if(my_update_L.and.present(F_nk))then
        write(for_msg,*) 'Error in encode_vert_5002, optional parameter F_nk must not be used with option update_L set to .true.'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     end if

     !==========================================================================================
     ! Nk is the number of dynamic momentum levels without hyb = 1.0 and diag level or top level
     ! Therefore, the number of lines in table is nj =
     !   nk + 1 for hyb=1 + 0 or 1 for diag                    -> momentum
     ! + nk + 1 for hyb=1 + 0 or 1 for diag + 0 or 1 for top   -> thermo  
     ! + skip
     ! = 
     ! nj = 2 * ( nk + 1 + diag ) + top + skip
     !
     ! ->  nk = ( nj - top - skip ) / 2 - 1 - diag
     !
     !==========================================================================================
     !
     ! Option notop ?
     k_plus_top=1
     ! Test if version >= 4 -> notop
     if(self%version>=4)k_plus_top=0

     ! Option diag level in meter Above Ground Level included ?
     k_plus_diag=0
     if(is_valid(self,dhm_valid))k_plus_diag=1

     if(my_update_L)then
        nk = ( size(self%table,2) - k_plus_top - skip ) / 2 - 1 - k_plus_diag
     else
        if(.not.present(F_nk))then
           write(for_msg,*) 'Error in encode_vert_5002, internal error with F_nk and update_L'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
        endif
        nk = F_nk
     endif

     ! nb              is the number or line for mometum levels
     ! nb + k_plus_top is the number or line for thermo  levels
     nb=nk+1+k_plus_diag
        
     ! Allocate table space
     if(.not.my_update_L)then
        if (associated(self%table)) deallocate(self%table)         
        allocate(self%table(3,2*nb+k_plus_top+skip,1),stat=error)
        if(error < 0)then
           write(for_msg,*) 'cannot allocate self%table in encode_vert_5002'
           call msg(MSG_ERROR,VGD_PRFX//for_msg)
           return
        endif
     endif
     
     ! Associate reference field name
     self%ref_name='P0'

     ! Vector size checks
     nn=size(self%ip1_m)
     if(nn.ne.nb)then
        write(for_msg,*) 'wrong size for ip1_m, is ',nn,'should be ',nb
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%a_m_8)
     if(nn.ne.nb)then
        write(for_msg,*) 'wrong size for a_m_8, is ',nn,'should be ',nb
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%b_m_8)
     if(nn.ne.nb)then
        write(for_msg,*) 'wrong size for b_m_8, is ',nn,'should be ',nb
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%ip1_t)
     if(nn.ne.nb+k_plus_top)then
        write(for_msg,*) 'wrong size for ip1_t, is ',nn,'should be ',nb+k_plus_top
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%a_t_8)
     if(nn.ne.nb+k_plus_top)then
        write(for_msg,*) 'wrong size for a_t_8, is ',nn,'should be ',nb+k_plus_top
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nn=size(self%b_t_8)
     if(nn.ne.nb+k_plus_top)then
        write(for_msg,*) 'wrong size for b_t_8, is ',nn,'should be ',nb+k_plus_top
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (len_trim(self%ref_name) > len(ref_name)) then
        write(for_msg,*) 'reference field name '//trim(self%ref_name)//' longer than limit: ',len(ref_name)
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     error = flip_transfer(self%ref_name,for_char_8)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned error code from encode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Fill header
     self%table(1:3,1,1)=(/dble(self%kind)  ,dble(self%version),dble(skip)/)
     self%table(1:3,2,1)=(/self%ptop_8      ,self%pref_8       ,dble(self%rcoef1)/)     
     self%table(1:3,3,1)=(/dble(self%rcoef2),for_char_8           ,0.d0/)

     ! Fill momentum level data
     do k=1,nb
        ind=k+skip
        self%table(1:3,ind,1)=(/dble(self%ip1_m(k)),self%a_m_8(k),self%b_m_8(k)/)
     enddo     

     ! Fill thermodynamic level data
     do k=1,nb+k_plus_top
        ind=k+skip+nb
        self%table(1:3,ind,1)=(/dble(self%ip1_t(k)),self%a_t_8(k),self%b_t_8(k)/)
     enddo
     
     ! Set status and return
     status = VGD_OK
  end function encode_vert_5002

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Decoding functions

  integer function decode_vert_1001(self) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self  !Vertical descriptor instance
     
     ! Local variables
     integer :: skip,nj,nk,k,ind,error,istat

     ! Set error status
     status = VGD_ERROR

     ! Read header line 1
     self%kind     = nint(self%table(1,1,1))
     self%version  = nint(self%table(2,1,1))
     skip          = nint(self%table(3,1,1))
     
     ! Read header line 2
     error = flip_transfer(self%table(1,2,1),self%ref_name)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from decode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nj=size(self%table,dim=2)
     nk=nj-skip

     ! Allocate and assign level data
     if (associated(self%ip1_m)) deallocate(self%ip1_m)
     allocate(self%ip1_m(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%ip1_m(nk) in decode_vert_1001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%a_m_8)) deallocate(self%a_m_8)
     allocate(self%a_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%a_m_8 in decode_vert_1001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%b_m_8)) deallocate(self%b_m_8)
     allocate(self%b_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%b_m_8(nk) in decode_vert_1001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif     

     do k=1,nk
        ind=k+skip
        self%ip1_m(k) = nint(self%table(1,ind,1))
        self%a_m_8(k) =      self%table(2,ind,1)
        self%b_m_8(k) =      self%table(3,ind,1)
     enddo

     ! Set status and return
     status = VGD_OK

  end function decode_vert_1001

  integer function decode_vert_1002(self) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self  !Vertical descriptor instance
     
     ! Local variables
     integer :: skip,nj,nk,k,ind,error,istat

     ! Set error status
     status = VGD_ERROR

     ! Read header line 1
     self%kind     = nint(self%table(1,1,1))
     self%version  = nint(self%table(2,1,1))
     skip          = nint(self%table(3,1,1))
     
     ! Read header line 2
     self%ptop_8   = self%table(1,2,1)
     error = flip_transfer(self%table(2,2,1),self%ref_name)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from decode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nj=size(self%table,dim=2)
     nk=nj-skip

     ! Allocate and assign level data
     if (associated(self%ip1_m)) deallocate(self%ip1_m)
     allocate(self%ip1_m(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%ip1_m(nk) in decode_vert_1002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%a_m_8)) deallocate(self%a_m_8)
     allocate(self%a_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%a_m_8(nk) in decode_vert_1002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%b_m_8)) deallocate(self%b_m_8)
     allocate(self%b_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%b_m_8(nk) in decode_vert_1002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif     

     do k=1,nk
        ind=k+skip
        self%ip1_m(k) = nint(self%table(1,ind,1))
        self%a_m_8(k) =      self%table(2,ind,1)
        self%b_m_8(k) =      self%table(3,ind,1)
     enddo

     ! Set status and return
     status = VGD_OK

  end function decode_vert_1002

  integer function decode_vert_2001(self) result(status)
    type(vgrid_descriptor), intent(inout) :: self  !Vertical descriptor instance

    ! Local variables
    integer :: skip,k,ind,istat,nk
     
    ! Set error status
    status = VGD_ERROR
    
    ! Read header line 1
    self%kind     = nint(self%table(1,1,1))
    self%version  = nint(self%table(2,1,1))
    skip          = nint(self%table(3,1,1))

    nk = size(self%table,dim=2) - skip

    ! Allocate and assign pressure level data
    if (associated(self%ip1_m)) deallocate(self%ip1_m)
    allocate(self%ip1_m(nk),stat=istat)
    if (istat /= 0) then
       write(for_msg,*) 'unable to allocate self%ip1_m(nk) in decode_vert_2001'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if (associated(self%a_m_8)) deallocate(self%a_m_8)
    allocate(self%a_m_8(nk),stat=istat)
    if (istat /= 0) then
       write(for_msg,*) 'unable to allocate self%a_m_8(nk) in decode_vert_2001'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if (associated(self%b_m_8)) deallocate(self%b_m_8)
    allocate(self%b_m_8(nk),stat=istat)
    if (istat /= 0) then
       write(for_msg,*) 'unable to allocate self%b_m_8(nk) in decode_vert_2001'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    do k=1,nk
       ind=k+skip
       self%ip1_m(k) = nint(self%table(1,ind,1))
       self%a_m_8(k) =      self%table(2,ind,1)
       self%b_m_8(k) =      self%table(3,ind,1)
    end do

    ! Set status and return
    status = VGD_OK

  end function decode_vert_2001
       
  integer function decode_vert_5001(self) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self  !Vertical descriptor instance
     
     ! Local variables
     integer :: skip,nj,nk,k,ind,error,istat

     ! Set error status
     status = VGD_ERROR

     ! Read header line 1
     self%kind     = nint(self%table(1,1,1))
     self%version  = nint(self%table(2,1,1))
     skip          = nint(self%table(3,1,1))
     
     ! Read header line 2
     self%ptop_8   = self%table(1,2,1)
     self%pref_8   = self%table(2,2,1)
     self%rcoef1   = real(self%table(3,2,1))

     ! Read header line 3
     error = flip_transfer(self%table(1,3,1),self%ref_name)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from decode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nj=size(self%table,dim=2)
     nk=nj-skip

     ! Allocate and assign level data
     if (associated(self%ip1_m)) deallocate(self%ip1_m)
     allocate(self%ip1_m(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%ip1_m(nk) in decode_vert_5001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%a_m_8)) deallocate(self%a_m_8)
     allocate(self%a_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%a_m_8(nk) in decode_vert_5001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%b_m_8)) deallocate(self%b_m_8)
     allocate(self%b_m_8(nk),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%b_m_8(nk) in decode_vert_5001'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     do k=1,nk
        ind=k+skip
        self%ip1_m(k) = nint(self%table(1,ind,1))
        self%a_m_8(k) =      self%table(2,ind,1)
        self%b_m_8(k) =      self%table(3,ind,1)
     enddo

     ! Set status and return
     status = VGD_OK

  end function decode_vert_5001

  integer function decode_vert_5002(self) result(status)
     use utils, only: flip_transfer
     type(vgrid_descriptor), intent(inout) :: self  !Vertical descriptor instance
     
     ! Local variables
     integer :: skip,nj,nk,k,ind,error,istat,k_plus_top,k_plus_diag,nb

     ! Set error status
     status = VGD_ERROR

     ! Read header line 1
     self%kind     = nint(self%table(1,1,1))
     self%version  = nint(self%table(2,1,1))
     skip          = nint(self%table(3,1,1))
     k_plus_top=1
     if(self%version>=4)k_plus_top=0
     k_plus_diag=0
     if(is_valid(self,dhm_valid))k_plus_diag=1

     ! Read header line 2
     self%ptop_8   = self%table(1,2,1)
     self%pref_8   = self%table(2,2,1)
     self%rcoef1   = real(self%table(3,2,1))

     ! Read header line 3
     self%rcoef2   = real(self%table(1,3,1))
     error = flip_transfer(self%table(2,3,1),self%ref_name)
     if (error /= VGD_OK) then
        write(for_msg,*) 'flip_transfer function returned an error code from decode ',error
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     nj=size(self%table,dim=2)

     nk=(nj-k_plus_top-skip)/2-1-k_plus_diag
     nb=nk+1+k_plus_diag

     ! Allocate and assign momentum level data
     if (associated(self%ip1_m)) deallocate(self%ip1_m)
     
     allocate(self%ip1_m(nb),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%ip1_m(nb) in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%a_m_8)) deallocate(self%a_m_8)
     allocate(self%a_m_8(nb),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%a_m_8(nb) in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%b_m_8)) deallocate(self%b_m_8)
     allocate(self%b_m_8(nb),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%b_m_8(nb) in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     do k=1,nb
        ind=k+skip
        self%ip1_m(k) = nint(self%table(1,ind,1))
        self%a_m_8(k) =      self%table(2,ind,1)
        self%b_m_8(k) =      self%table(3,ind,1)
     enddo

     ! Allocate and assign thermodynamic level data
     if (associated(self%ip1_t)) deallocate(self%ip1_t)
     allocate(self%ip1_t(nb+k_plus_top),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%ip1_t in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%a_t_8)) deallocate(self%a_t_8)
     allocate(self%a_t_8(nb+k_plus_top),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%a_t_8 in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     if (associated(self%b_t_8)) deallocate(self%b_t_8)
     allocate(self%b_t_8(nb+k_plus_top),stat=istat)
     if (istat /= 0) then
        write(for_msg,*) 'unable to allocate self%b_t_8 in decode_vert_5002'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     do k=1,nb+k_plus_top
        ind=k+skip+nb
        self%ip1_t(k) = nint(self%table(1,ind,1))
        self%a_t_8(k) =      self%table(2,ind,1)
        self%b_t_8(k) =      self%table(3,ind,1)
     enddo     

     ! Set status and return
     status = VGD_OK

  end function decode_vert_5002

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Set and check the vertical code
  
  integer function set_vcode_d(self) result(status)
     ! Compute the internal vertical code from table
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance

     ! Local variables
     integer :: kind,version
     
     ! Set error status and check initialization
     status = VGD_ERROR

     if (.not.associated(self%table)) then
        write(for_msg,*) 'set_vcode called before constructor'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Wrap call to integer mangling
     status = get_version_info(self,kind,version)
     if (status /= VGD_OK) then
        write(for_msg,*) 'cannot decode table to read kind and version: ',kind,version
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     status = set_vcode(self,kind,version)

     ! Set status and return
     status = VGD_OK
     return
  end function set_vcode_d

  integer function set_vcode_i(self,kind,version) result(status)
     ! Compute the internal vertical code from kind and version
     type(vgrid_descriptor), intent(inout) :: self      !Vertical descriptor instance
     integer, intent(in) :: kind,version                !Kind and version of vertical coord

     ! Set error status
     status = VGD_ERROR

     ! Check for valid entries (version is limited by the mangling equation)
     if (kind > MAX_VKIND .or. kind < 0 .or. version > 999 .or. version < 0) then
        write(for_msg,*) 'invalid kind or version in set_vcode_d: ',kind,version
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif

     ! Mangle the kind and version information
     self%vcode = kind*1000+version

     ! Set status and return
     status = VGD_OK
     return
  end function set_vcode_i

  integer function get_version_info(self,kind,version) result(status)
    ! Retrieve kind and version information from the table
    type(vgrid_descriptor), intent(in) :: self         !Vertical descriptor instance
    integer, intent(out) :: kind,version               !Kind and version of vertical coord

    ! Set error status
    status = VGD_ERROR
    
    ! Retrieve kind and version information from table
    kind = nint(self%table(1,1,1))
    version = nint(self%table(2,1,1))

    ! Set status and return
    status = VGD_OK
    return
  end function get_version_info

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Descriptor operation support functions

  logical function found_descriptor(self) result(found)
     ! Is the descriptor valid?
     type(vgrid_descriptor), intent(in) :: self         !Vertical descriptor instance
     found = .false.
     if (self%vcode >= 0) then
        found = .true.
     else
        write(for_msg,*) 'WARNING: uninitialized descriptor given to found_descriptor()'
        call msg(MSG_ERROR,VGD_PRFX//for_msg)
        return
     endif
     return
  end function found_descriptor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Validity inquiry functions

  logical function is_valid(self,element_valid) result(valid)
     ! Check for validity of the element
     type(vgrid_descriptor) :: self          !Vertical descriptor instance
     integer, dimension(:) :: element_valid
     valid = .false.
     if (any(element_valid == self%vcode)) valid = .true.
     return
  end function is_valid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) FSTD support functions

  integer function fstd_init(self) result(status)
    ! Initialize common elements of the fstd record
    type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance
    ! Local variables
    integer :: ig2,ig3,ig4,error
    character(len=ETIK_LENGTH) :: etiket
    status = VGD_ERROR
    if (self%rec%initialized) then
       status = VGD_OK
       return
    endif
    ig2=0; ig3=0; ig4=0
    error = set_vcode(self)
    select case (self%vcode)
    case (1001)
       etiket='ETA_GEMV3'
    case (1002)
       etiket='ETA_GEMV3'
       ig2=nint(self%ptop_8*10.d0)
    case (2001)
       etiket='PRESSURE'
    case (1003)
       etiket='HYBNORM_GEM3'
       ig2=nint(self%ptop_8*10.d0)
       ig3=nint(self%rcoef1*100.)
    case (5001)
       etiket='HYB_GEMV3'
       ig2=nint(self%ptop_8*10.d0)
       ig3=nint(self%rcoef1*100.)
    case (5002,5003,5004)
       etiket='STG_CP_GEMV4'
       ig2=nint(self%ptop_8*10.d0)
       ig3=nint(self%rcoef1*100.)
       ig4=nint(self%rcoef2*100.)
    case (5005)
       etiket='STG_CP_GEMV4'
       ig2=0
       ig3=nint(self%rcoef1*100.)
       ig4=nint(self%rcoef2*100.)
    case DEFAULT
       write(for_msg,*) 'invalid kind or version in fstd_init:',self%kind,self%version
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select
    self%rec%nomvar=ZNAME
    self%rec%dateo=0
    self%rec%deet=0
    self%rec%npas=0
    self%rec%datyp=5
    self%rec%nbits=64
    self%rec%grtyp='X'
    self%rec%typvar='X'     
    self%rec%etiket=trim(etiket)
    self%rec%ip3=0
    self%rec%ig1=self%vcode
    self%rec%ig2=ig2
    self%rec%ig3=ig3
    self%rec%ig4=ig4
    self%rec%initialized=.true.
    status = VGD_OK
  end function fstd_init

  integer function my_fstprm(fstkey,record) result(status)
    ! Use fstprm function to get information about the record
    integer, intent(in) :: fstkey               !Key from FST file record
    type(FSTD_ext) :: record                    !Record information
    ! Local variables
    integer :: error
    integer, external :: fstprm,fstinf
    real*8 :: nhours
    status = VGD_ERROR
    error=fstprm(fstkey,record%dateo,record%deet,record%npas, &
         record%ni,record%nj,record%nk,record%nbits,record%datyp,record%ip1,record%ip2, &
         record%ip3,record%typvar,record%nomvar,record%etiket,record%grtyp, &
         record%ig1,record%ig2,record%ig3,record%ig4,record%swa, &
         record%lng,record%dltf,record%ubc,record%extra1,record%extra2, &
         record%extra3)
    if (error < 0) then
       write(6,*) 'cannot fstprm for fstkey ',fstkey
       return
    end if
    nhours=record%deet*record%npas/3600.d0
    ! Note incdatr prints the following when dateo is zero :
    !    label 1,idate2:           0
    if (record%dateo == 0 )then
       record%datev=0
    else
       call incdatr(record%datev,record%dateo,nhours)
    endif
    status = VGD_OK
  end function my_fstprm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Pressure computation functions
  integer function compute_pressure_1001_8(self,sfc_field,ip1_list,levels,in_log) result(status)
    ! Compute pressure for all levels specified in ip1_list
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    real*8, dimension(:,:), intent(in) :: sfc_field       !Surface field reference for coordinate
    integer, dimension(:), intent(in) :: ip1_list       !List of IP1 levels to calculate on
    real*8, dimension(:,:,:), pointer  :: levels          !Physical level values
    logical, intent(in) :: in_log                       !Compute level values in ln()

    ! Internal variables
    integer :: i,j,nk
    real*8, dimension(size(ip1_list)) :: aa_8,bb_8
    logical :: found

    ! Set error status
    status = VGD_ERROR

    ! Set size of output
    nk = size(ip1_list)
    
    ! Find ip1 values
    do i=1,nk
       found = .false.
       do j=1,size(self%ip1_m)
          if (self%ip1_m(j) == ip1_list(i)) then
             found = .true.
             aa_8(i) = self%a_m_8(j)
             bb_8(i) = self%b_m_8(j)
             exit
          endif
       enddo
       if (.not.found) then
          write(for_msg,*) 'cannot find ip1 ',ip1_list(i),' in compute_pressure_1001'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo

    ! Compute pressure
    do i=1,nk
       levels(:,:,i) = aa_8(i) + bb_8(i)*sfc_field
    enddo
    if (in_log) levels = log(levels)

    ! Set status and return
    status = VGD_OK
    return
  end function compute_pressure_1001_8

  integer function compute_pressure_1002_8(self,sfc_field,ip1_list,levels,in_log) result(status)
    ! Compute pressure for all levels specified in ip1_list
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    real*8, dimension(:,:), intent(in) :: sfc_field       !Surface field reference for coordinate
    integer, dimension(:), intent(in) :: ip1_list       !List of IP1 levels to calculate on
    real*8, dimension(:,:,:), pointer  :: levels          !Physical level values
    logical, intent(in) :: in_log                       !Compute level values in ln()

    ! Internal variables
    integer :: i,j,nk
    real*8, dimension(size(ip1_list)) :: aa_8,bb_8
    logical :: found

    ! Set error status
    status = VGD_ERROR

    ! Set size of output
    nk = size(ip1_list)
    
    ! Find ip1 values
    do i=1,nk
       found = .false.
       do j=1,size(self%ip1_m)
          if (self%ip1_m(j) == ip1_list(i)) then
             found = .true.
             aa_8(i) = self%a_m_8(j)
             bb_8(i) = self%b_m_8(j)
             exit
          endif
       enddo
       if (.not.found) then
          write(for_msg,*) 'cannot find ip1 ',ip1_list(i),' in compute_pressure_1002'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo

    ! Compute pressure
    do i=1,nk
       levels(:,:,i) = aa_8(i) + bb_8(i)*sfc_field
    enddo
    if (in_log) levels = log(levels)

    ! Set status and return
    status = VGD_OK
    return
 end function compute_pressure_1002_8

 integer function compute_pressure_2001_8(self,ip1_list,levels,in_log) result(status)
    ! Compute pressure for all levels specified in ip1_list
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    integer, dimension(:), intent(in) :: ip1_list       !List of IP1 levels to calculate on
    real*8, dimension(:,:,:), pointer  :: levels          !Physical level values
    logical, intent(in) :: in_log                       !Compute level values in ln()

    ! Local variables
    integer :: i,j,nk
    real*8, dimension(size(ip1_list)) :: aa_8,bb_8
    logical :: found
    
    ! Set error status
    status = VGD_ERROR
    
    ! Set size of output
    nk = size(ip1_list)

    ! Find ip1 values
    do i=1,nk
       found = .false.
       do j=1,size(self%ip1_m)
          if (self%ip1_m(j) == ip1_list(i)) then
             found = .true.
             aa_8(i) = self%a_m_8(j)
             bb_8(i) = self%b_m_8(j)
          endif
       enddo
       if (.not.found) then
          write(for_msg,*) 'cannot find ip1 ',ip1_list(i),' in compute_pressure_2001'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo

    ! Compute pressure
    do i=1,nk
       levels(:,:,i) = aa_8(i) + 0.d0*bb_8(i)
    enddo
    if (in_log) levels = log(levels)

    ! Set status and return
    status = VGD_OK
    return

 end function compute_pressure_2001_8

  integer function compute_pressure_5001_8(self,sfc_field,ip1_list,levels,in_log,dpidpis) result(status)
    ! Compute pressure for all levels specified in ip1_list
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    real*8, dimension(:,:), intent(in) :: sfc_field       !Surface field reference for coordinate
    integer, dimension(:), intent(in) :: ip1_list       !List of IP1 levels to calculate on
    real*8, dimension(:,:,:), pointer  :: levels          !Physical level values
    logical, intent(in) :: in_log                       !Compute level values in ln()
    logical, intent(in) :: dpidpis                      !Compute partial derivative of hydrostatic pressure (pi) with
                                                        !   respect to surface hydrostatic pressure(pis)
    ! Internal variables
    integer :: i,j,nk
    real*8, dimension(size(ip1_list)) :: aa_8,bb_8
    logical :: found

    ! Set error status
    status = VGD_ERROR

    ! Set size of output
    nk = size(ip1_list)
    
    ! Find ip1 values
    do i=1,nk
       found = .false.
       do j=1,size(self%ip1_m)
          if (self%ip1_m(j) == ip1_list(i)) then
             found = .true.
             aa_8(i) = self%a_m_8(j)
             bb_8(i) = self%b_m_8(j)
             exit
          endif
       enddo
       if (.not.found) then
          write(for_msg,*) 'cannot find ip1 ',ip1_list(i),' in compute_pressure_5001'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo

    if(dpidpis)then
       if(in_log)then
          write(for_msg,*) 'option in_log not allowed with dpidpis'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       do i=1,nk
          levels(:,:,i) = bb_8(i)
       enddo
       status = VGD_OK
       return
    endif

    ! Compute pressure
    do i=1,nk
       levels(:,:,i) = aa_8(i) + bb_8(i)*sfc_field
    enddo
    if (in_log) levels = log(levels)

    ! Set status and return
    status = VGD_OK
    return
  end function compute_pressure_5001_8

  integer function compute_pressure_5002_8(self,sfc_field,ip1_list,levels,in_log,dpidpis) result(status)
    ! Compute pressure for all levels specified in ip1_list
    type(vgrid_descriptor), intent(in) :: self          !Vertical descriptor instance
    real*8, dimension(:,:), intent(in) :: sfc_field       !Surface field reference for coordinate
    integer, dimension(:), intent(in) :: ip1_list       !List of IP1 levels to calculate on
    real*8, dimension(:,:,:), pointer  :: levels          !Physical level values
    logical, intent(in) :: in_log                       !Compute level values in ln()
    logical, intent(in) :: dpidpis                      !Compute partial derivative of hydrostatic pressure (pi) with
                                                        !   respect to surface hydrostatic pressure(pis)

    ! Internal variables
    integer :: i,j,nk,kind
    real*8, dimension(size(sfc_field,dim=1),size(sfc_field,dim=2)) :: s_8
    real*8, dimension(size(ip1_list)) :: aa_8,bb_8
    real :: pppp
    logical :: found
    character(len=1)  :: dummy_S

    ! Set error status
    status = VGD_ERROR

    ! Set size of output
    nk = size(ip1_list)
    
    ! Find ip1 values
    do i=1,nk
       found = .false.
       do j=1,size(self%ip1_m)
          if (self%ip1_m(j) == ip1_list(i)) then
             found = .true.
             aa_8(i) = self%a_m_8(j)
             bb_8(i) = self%b_m_8(j)
             exit
          endif
       enddo
       if (.not.found) then
          do j=1,size(self%ip1_t)
             if (self%ip1_t(j) == ip1_list(i)) then
                found = .true.
                aa_8(i) = self%a_t_8(j)
                bb_8(i) = self%b_t_8(j)
                exit
             endif
          enddo
       endif
       if (.not.found) then
          write(for_msg,*) 'cannot find ip1 ',ip1_list(i),' in compute_pressure_5002_8'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
    enddo

    ! Compute pressure
    !s_8 = log(dble(sfc_field)/self%pref_8)
    ! Andre Plante dble(sfc_field) not needed since sfc_field already real*8
    s_8 = log(sfc_field/self%pref_8)
    do i=1,nk
       levels(:,:,i) = aa_8(i) + bb_8(i)*s_8
    enddo
    if (.not.in_log)then
       levels = exp(levels)
       ! Force surface pressure to be equal to sfc_field
       ! Needed by assimilation section.
       do i=1,nk
          call convip(ip1_list(i),pppp,kind,-1,dummy_S,.false.)
          if(abs(pppp-1.).lt.epsilon(pppp) .and. kind==5)levels(:,:,i)=sfc_field         
       enddo       
    endif
    if(dpidpis)then
       if(in_log)then
          write(for_msg,*) 'in compute_pressure_5002_8, cannot get dpidpis in log'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       do i=1,nk
          levels(:,:,i) = bb_8(i)*levels(:,:,i)/sfc_field
       enddo
    endif

    ! Set status and return
    status = VGD_OK
    return
 end function compute_pressure_5002_8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) Construct vertical structure from legacy encoding (PT,HY...)
 
 integer function vgd_legacy(self,F_lu,F_kind) result(status)
    ! Construct vertical structure from legacy encoding (PT,HY...)
    !      
    ! Arguments
    type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance    
    integer, intent(in)    :: F_lu  ! fst lu containing records
    integer, optional, intent(in) :: F_kind ! ip1 kind set (all by default)
    
    ! Local variables
    integer, dimension(MAX_DESC_REC) :: KeyList,Ip1List
    real,    dimension(MAX_DESC_REC) :: PresList
    integer :: fstinl,count,error,ni,nj,nk,i,j,k,m,nip1,nb
    integer :: kind,user_kind,valid_kind
    integer, parameter :: nb_kind=100
    integer, dimension(0:nb_kind) :: num_in_kind

    real :: x1
    type(FSTD_ext) :: var

    user_kind=-1
    if(present(F_kind))then
       user_kind=F_kind
       write(for_msg,*) 'Looking for kind=',user_kind
       call msg(MSG_INFO,VGD_PRFX//for_msg)
    endif

    num_in_kind=0

    status=VGD_ERROR
    error = fstinl(F_lu,ni,nj,nk,-1,' ',-1,-1,-1,' ',' ',keyList,count,size(keyList))
    if (error < 0) then
       write(for_msg,*) 'problem fstinl in vgd_legacy'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    nip1=0
    do i=1,count
       error = my_fstprm(keyList(i),var)
       if (error /= VGD_OK) then
          write(for_msg,*) 'in vgd_legacy, error return from fstprm wrapper for fst key ',keyList(i)
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       call convip(var%ip1,PresList(i),kind,-1,'',.false.)
       if(var%nomvar == '>>')cycle
       if(var%nomvar == '^^')cycle
       if(var%nomvar == 'P0')cycle
       if(var%nomvar == 'PT')cycle
       if(var%nomvar == 'HY')cycle
       if(kind==2.and.PresList(i)==0.)cycle
       if(user_kind /= -1 .and. kind /= user_kind)cycle
       if(kind==1.or.kind==2.or.kind==5)then
          num_in_kind(kind)=num_in_kind(kind)+1
          nip1=nip1+1
          Ip1List(nip1)=var%ip1
          KeyList(nip1)=keyList(i)
          PresList(nip1)=PresList(i)
          valid_kind=kind
       endif
    enddo

    if(maxval(num_in_kind) /= nip1)then       
       write(for_msg,*) 'more than one pressure/sigma/hyb coordinate in file'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       do i=0,nb_kind
          if(num_in_kind(i) > 0)then
             write(for_msg,'("There are",i10," records of kind ",i3)')num_in_kind(i),i
             call msg(MSG_INFO,VGD_PRFX//for_msg)
          endif
       enddo
       return
    endif

    !    Sort levels in ascending order    
    do i=1,nip1-1
       k=i
       do j=i+1,nip1
          if(PresList(j).lt.PresList(k))k=j
       enddo
       if(k.ne.i) then
          ! hyb
          x1     =PresList(k)
          PresList(k)=PresList(i)
          PresList(i)=x1

          ! ip1
          m         = Ip1List(k)
          Ip1List(k)= Ip1List(i)
          Ip1List(i)= m

          ! fstkey
          m           = KeyList(k)
          KeyList(k)= KeyList(i)
          KeyList(i)= m
          
       endif
    enddo

    ! Remove duplictate (there must be a better way to do this)
    do i=1,nip1-1
       if(Ip1List(i)/=-1)then
          do j=i+1,nip1
             if(Ip1List(j)==Ip1List(i))then
                Ip1List(j)=-1
             endif
          enddo
       endif
    enddo
    nb=0
    do i=1,nip1
       if(Ip1List(i)/=-1)then
          nb=nb+1
          Ip1List(nb)=Ip1List(i)
          KeyList(nb)=KeyList(i)
          ! pres is not used below but adjusting for consistency and possible future use.
          PresList(nb)=PresList(i)
       endif
    enddo

    if(nb==0)then
       if(user_kind /= -1)then
          write(for_msg,'("No record of kind",i2," in file")')user_kind
       else
          write(for_msg,*) 'No record of type pressure/sigma/hyb in file'
       endif
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return          
    endif
    write(for_msg,'("   Found",i10," unique ip1 of kind ",i2," among the ",i10," records in file to construct the vertical descriptor")')nb,valid_kind,count
    call msg(MSG_INFO,VGD_PRFX//for_msg)
    
    status = gen_legacy_desc(self,F_lu,Ip1List(1:nb),KeyList(1:nb),nb)
    if(status/=VGD_OK)then
       write(for_msg,'(" Error : problem with gen_legacy_desc")')
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    write(for_msg,'("   Vertical descriptor successfully reconstructed")')
    call msg(MSG_INFO,VGD_PRFX//for_msg)

 end function vgd_legacy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) !s/r gen_legacy_desc - to return a grid descriptor given
 ! a unit number and a list of fstkeys of the RPN standard file
 !*********************************************************************
 !
 integer function gen_legacy_desc(self,iun,ip1s,fstkeys,nlev) result(status)
    use vdescript_1001,      only: vgrid_genab_1001
    use vdescript_1002_5001, only: vgrid_genab_1002_5001 
    use vdescript_1004,      only: vgrid_genab_1004
    use vdescript_2001,      only: vgrid_genab_2001
    !
    ! Arguments
    type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor
    integer, intent(in)    :: iun               ! unit containing records
    integer, dimension(:), intent(in):: ip1s    ! list of ips
    integer, dimension(:), intent(in):: fstkeys ! list of fstkeys (matching ip1s)
    integer, intent(in)    :: nlev              ! number of fstkeys given
    !
    !author
    !     V.Lee-A.Plante 2010 
    !
    ! Local variables
    integer, parameter :: nmax=1000
    integer :: fstinl,fstinf,fstprm,fstluk,fnom,read_decode_hyb,hyb_to_pres,eta_to_pres,&
         sigma_to_pres,etasef_to_pres 
    external  fstinl,fstinf,fstprm,fstluk,fnom,read_decode_hyb,hyb_to_pres,eta_to_pres,&
         sigma_to_pres,etasef_to_pres
    
    integer :: ier,error,origkind,istat
    integer,dimension(:),pointer :: ip1
    real,dimension(:),pointer :: hyb,hybm
    integer :: nia,nja,nka,ni1,nj1,nk1,k,ni,nj,nk
    integer :: e1_key,hy_key,pt_key
    integer :: datev, dateo, deet, ipas, ip1a, ip2a, ip3a, &
         ig1a, ig2a, ig3a, ig4a, bit, datyp, &
         swa, lng, dlf, ubc, ex1, ex2, ex3, kind
    real :: lev,ptop,pref,rcoef,etatop
    character(len=1) :: tva, grda, blk_S
    character(len=KEY_LENGTH) :: var
    character(len=ETIK_LENGTH) :: etik_S
    real(kind=8), dimension(:), pointer :: a_m_8!A-coefficients for momentum levels
    real(kind=8), dimension(:), pointer :: b_m_8!B-coefficients for momentum levels
    
    nullify(ip1,hyb,hybm,a_m_8,b_m_8)

    status=VGD_ERROR
    
    allocate(ip1(nlev),hyb(nlev),hybm(nlev),stat=istat)
    if (istat /= 0) then
       if(associated(ip1))deallocate(ip1)
       if(associated(hyb))deallocate(hyb)
       if(associated(hybm))deallocate(hybm)
       write(for_msg,*) 'unable to allocate ip1,hyb,hybm, in gen_legacy_desc'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    
    ier = fstprm (fstkeys(1), dateo, deet, ipas, nia, nja, nka, &
         bit, datyp, ip1a,ip2a,ip3a, tva, var, etik_S, grda, &
         ig1a,ig2a,ig3a,ig4a, swa,lng, dlf, ubc, ex1, ex2, ex3 )
    if (ier.lt.0) then
       write(for_msg,*) 'gen_legacy_desc error: fstprm on key',fstkeys(1)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    else
       call convip (ip1a, lev, kind,-1, blk_S, .false.)
       if (kind.ne.1.and.kind.ne.2.and.kind.ne.5) then
          write(for_msg,*) 'gen_legacy_desc error: kind = ',kind,' has to be 1,2 or 5'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          deallocate(ip1,hyb,hybm)
          return
       endif
       origkind=kind
       ip1(1)=ip1a
       hyb(1)=lev
    endif
    do k=2,nlev
       ier = fstprm (fstkeys(k), dateo, deet, ipas, ni1, nj1, nk1, &
            bit, datyp, ip1a,ip2a,ip3a, tva, var, etik_S, grda, &
            ig1a,ig2a,ig3a,ig4a, swa,lng, dlf, ubc, ex1, ex2, ex3 )
       if (ni1.ne.nia.and.nj1.ne.nja.and.nk1.ne.nka.or.&
            ier.lt.0) then
          write(for_msg,*) 'gen_legacy_desc error: fstprm on key',fstkeys(k),'dim mismatch'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          goto 999
       endif
       call convip (ip1a, lev, kind,-1, blk_S, .false.)
       if (kind.ne.origkind) then
          write(for_msg,*) 'gen_legacy_desc error: orig kind=',origkind,'kind found=',kind
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          goto 999
       endif
       ip1(k)=ip1a
       hyb(k)=lev
    enddo
    ni=nia
    nj=nja
    
    hy_key=fstinf (iun,ni,nj,nk,-1,' ',-1,  -1,  -1,' ','HY')
    pt_key=fstinf (iun,ni,nj,nk,-1,' ',-1,  -1,  -1,' ','PT')
    e1_key=fstinf (iun,ni,nj,nk,-1,' ',-1,  -1,  -1,' ','E1')

    call incdatr(datev,dateo,ipas*deet/3600.0d0)
    
    if (kind.eq.1) then
       if (pt_key.ge.0) then
          ier=get_consistent_pt_e1(iun,ptop,'PT')
          if(ier.eq.VGD_ERROR)then
             write(for_msg,*) 'gen_legacy_desc error: consistency check on PT failed'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             goto 999
          endif
          if (e1_key.ge.0) then
             write(for_msg,*)' etasef coordinate found'
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             ier=get_consistent_pt_e1(iun,etatop,'E1')
             if(ier.eq.VGD_ERROR)then
                write(for_msg,*) 'gen_legacy_desc error: consistency check on E1 failed'
                call msg(MSG_ERROR,VGD_PRFX//for_msg)
                goto 999
             endif
             call vgrid_genab_1004(hyb,etatop,ptop,a_m_8,b_m_8,error)
             if (error /= VGD_OK)goto 999
             !        ier = new_build_vert(self,kind=1,version=4,nk=nlev, &
             !                     ip1=ig1a,
             !                     ip2=ig2a,
             !                     etatop_8=dble(etatop), &
             !                     ptop_8=dble(ptop),     &
             !                     a_m_8=a_m_8,           &
             !                     b_m_8=b_m_8,           &
             !                     ip1_m=ip1s)
             goto 999
          else
             write(for_msg,*)' eta coordinate found'
             call msg(MSG_INFO,VGD_PRFX//for_msg)
             call vgrid_genab_1002_5001(1002,hyb,1.,ptop*100.d0,80000.d0,&
                  hybm,a_m_8,b_m_8,ip1,error)
             if (error /= VGD_OK)goto 999
             status = new_build_vert(self,kind=1,version=2,nk=nlev, &
                  ip1=ig1a,              &
                  ip2=ig2a,              &
                  ptop_8=ptop*100.d0,    &
                  a_m_8=a_m_8,           &
                  b_m_8=b_m_8,           &
                  ip1_m=ip1s)
             goto 999
          endif
       else if (hy_key.ge.0) then
          write(for_msg,*)' hybrid (normalized) coordinate found'
          call msg(MSG_INFO,VGD_PRFX//for_msg)
          ier=get_consistent_hy(iun,ptop,pref,rcoef)
          if(ier.eq.VGD_ERROR)then
             write(for_msg,*) 'gen_legacy_desc error: consistency check on HY failed'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             goto 999
          endif
          call vgrid_genab_1002_5001(1002,hyb,rcoef,ptop*100.d0,pref*100.d0,&
               hybm,a_m_8,b_m_8,ip1,error)
          if (error /= VGD_OK)goto 999
          status = new_build_vert(self,kind=1,version=3,nk=nlev, &
               ip1=ig1a,          &
               ip2=ig2a,          &
               ptop_8=ptop*100d0, &
               pref_8=pref*100d0, &
               rcoef1=rcoef,      &
               a_m_8=a_m_8,       &
               b_m_8=b_m_8,       &
               ip1_m=ip1s)          
          goto 999
       else 
          write(for_msg,*)' sigma coordinate found'
          if(.not.ALLOW_SIGMA)then
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             write(for_msg,*) 'gen_legacy_desc error: sigma coordinate construction is not ALLOWED. If your are certain that you want this sigma coordinate, set ALLOW_SIGMA to true e.g. stat = vgd_putopt("ALLOW_SIGMA",.true.)'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             goto 999
          endif
          call msg(MSG_INFO,VGD_PRFX//for_msg)
          call vgrid_genab_1001(hyb,hybm,a_m_8,b_m_8,ip1,error)
          if (error /= VGD_OK)goto 999
          status = new_build_vert(self,kind=1,version=1, nk=nlev,&
               ip1=ig1a,          &
               ip2=ig2a,          &
               a_m_8=a_m_8,       &
               b_m_8=b_m_8,       &
               ip1_m=ip1s)
          goto 999
       endif
    endif
    
    if (kind.eq.2) then
       write(for_msg,*)' pressure coordinate found'
       call msg(MSG_INFO,VGD_PRFX//for_msg)
       call vgrid_genab_2001(hyb,a_m_8,b_m_8,error)
       if (error /= VGD_OK)then
          goto 999
       endif
       status = new_build_vert(self,kind=2,version=1,nk=nlev, &
            ip1=ig1a,          &
            ip2=ig2a,          &
            a_m_8=a_m_8,       &
            b_m_8=b_m_8,       &
            ip1_m=ip1s)
       goto 999
    endif
    
    if (kind.eq.5.and.hy_key.ge.0) then
       write(for_msg,*)' hybrid (un-normalized) coordinate found'
       call msg(MSG_INFO,VGD_PRFX//for_msg)
       ier=get_consistent_hy(iun,ptop,pref,rcoef)
       if(ier.eq.VGD_ERROR)then
          write(for_msg,*) 'gen_legacy_desc error: consistency check on HY failed'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          goto 999
       endif
       call vgrid_genab_1002_5001(5001,hyb,rcoef,ptop*100.d0,pref*100.d0,&
            hybm,a_m_8,b_m_8,ip1,error)
       if (error /= VGD_OK)then
          goto 999
       endif
       status = new_build_vert(self,kind=5,version=1,nk=nlev, &
            ip1=ig1a,          &
            ip2=ig2a,          &
            ptop_8=ptop*100.d0, &
            pref_8=pref*100.d0, &
            rcoef1=rcoef, &
            a_m_8=a_m_8,       &
            b_m_8=b_m_8,       &
            ip1_m=ip1s)       
       goto 999
    else
       write(for_msg,*) 'gen_legacy_desc error: kind=5 but no HY found'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       goto 999
    endif

999 deallocate(ip1,hyb,hybm)              

 end function gen_legacy_desc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) !s/r consistency_hy - to check if HY in file are the same
 !*********************************************************************
 ! 
  integer function get_consistent_hy(F_iun,F_ptop,F_pref,F_rcoef) result(status)
    integer :: F_iun
    real :: F_ptop,F_pref,F_rcoef

    !Local variables
    real :: ptop,pref,rcoef,pres1,pres2
    type(FSTD_ext) :: record
    integer, parameter :: nmax=1000
    integer, dimension(nmax) :: liste
    integer :: infon,ier,read_decode_hyb,fstinl,ni,nj,nk,k,ip1,kind
    external read_decode_hyb
    
    status=VGD_ERROR

    ier=fstinl(F_iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','HY',liste,infon,nmax)
    if (ier.lt.0) then
       write(for_msg,*) 'In get_consistent_hy, problem with fstinl on HY'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    if(infon.gt.1)then
       write(for_msg,*) '  More than one HY, checking consistency ...'
       call msg(MSG_WARNING,VGD_PRFX//for_msg)
    endif
    do k=1,infon
       ier=my_fstprm(liste(k),record)
       if (ier < 0) then
          write(for_msg,*) 'In get_consistent_hy, problem with my_fstprm on k=',k
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif       
       ier=read_decode_hyb(F_iun,'HY',record%ip2,record%ip3,record%etiket,record%datev,ptop,pref,rcoef)
       if (ier.lt.0) then
          write(for_msg,*) 'In get_consistent_hy, problem with read_decode_hyb'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if(k.eq.1)then
          F_ptop =ptop
          F_pref =pref
          F_rcoef=rcoef
          ip1=record%ip1
       else
          if( &               
               ptop .ne.F_ptop .or. &
               pref .ne.F_pref .or. &
               rcoef.ne.F_rcoef)then
             write(for_msg,*) 'In get_consistent_hy, found inconsistant HY'
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             write(for_msg,*) 'ptop: ',ptop      ,' vs ',F_ptop
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             write(for_msg,*) 'pref: ',pref      ,' vs ',F_pref
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             write(for_msg,*) 'rcoef:',rcoef     ,' vs ',F_rcoef
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
          endif
          if(record%ip1.ne.ip1)then
             call convip(       ip1,pres1,kind,-1,'',.false.)
             call convip(record%ip1,pres2,kind,-1,'',.false.)
             if(abs(pres1-pres2).gt.epsilon(pres1))then
                write(for_msg,*) 'ip1:  ',record%ip1,' vs ',ip1             
                call msg(MSG_ERROR,VGD_PRFX//for_msg)
                return
             endif
          endif
       endif
    enddo    
    if(infon.gt.1)then
       write(for_msg,*) '  All HY consistent.'
       call msg(MSG_INFO,VGD_PRFX//for_msg)
    endif
    status=VGD_OK

 end function get_consistent_hy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) !s/r consistency_hy - to check if HY in file are the same
 !*********************************************************************
 ! 
 integer function get_consistent_pt_e1(F_iun,F_val,F_nomvar) result(status)
    
    integer :: F_iun
    real ::F_val
    character(len=*) :: F_nomvar
    
    ! Local variables
    integer, parameter :: nmax=1000
    integer, dimension(nmax) :: liste
    integer :: infon,ier,fstinl,fstluk,ni,nj,nk,k,istat
    real, dimension(:,:),allocatable :: work
    type(FSTD_ext) :: record

    status=VGD_ERROR

    ier=fstinl(F_iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',F_nomvar,liste,infon,nmax)

    if(infon.gt.1)then
       write(for_msg,*) '  More than one ',F_nomvar,', checking consistency ...'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
    endif
    allocate(work(ni,nj),stat=istat)
    if (istat /= 0) then
       write(for_msg,*) 'unable to allocate work(ni,nj) in get_consistent_pt_e1'
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif
    
    do k=1,infon       
       ier=my_fstprm(liste(k),record)
       if (ier < 0) then
          write(for_msg,*) 'In get_consistent_pt_e1, problem with my_fstprm on k=',k
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          return
       endif
       if(  record%ni.ne.ni.or. &
            record%nj.ne.nj.or. &
            record%nK.ne.nk)then
          write(for_msg,*) 'In get_consistent_pt_e1 : inconsistent ',F_nomvar,' frid size:'
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,*) 'ni : ',record%ni,' vs ',ni
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,*) 'nj : ',record%nj,' vs ',nj
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          write(for_msg,*) 'nk : ',record%nk,' vs ',nk
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          goto 999
       endif
       ier = fstluk(work,liste(k),ni,nj,nk)
       if (ier.lt.0) then
          write(for_msg,*) 'In get_consistent_pt_e1 : problem in fstluk ',F_nomvar,' k=',k
          call msg(MSG_ERROR,VGD_PRFX//for_msg)
          goto 999
       endif
       if(k.eq.1)then
          F_val = work(1,1)
       else
          if(work(1,1).ne.F_val)then
             write(for_msg,*) 'In get_consistent_pt_e1 : inconsistent ',F_nomvar,' ',work(1,1),' vs ',F_val
             call msg(MSG_ERROR,VGD_PRFX//for_msg)
             goto 999
          endif
       endif
    enddo
    write(for_msg,*) '  All ',F_nomvar,' consistent.'
    call msg(MSG_INFO,VGD_PRFX//for_msg)
    status=VGD_OK
999 deallocate(work)
 end function get_consistent_pt_e1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! (PRIVATE) !s/r table_update - update self%table
!* 
 integer function table_update(self) result(status)
    
    type(vgrid_descriptor), intent(inout) :: self !Vertical descriptor instance

    ! Local variables
    integer :: error,kind,version
    character(len=10) :: cvcode

    ! Set error status
    status = VGD_ERROR

    ! Initializations
    kind=self%kind
    version=self%version
    
    ! Fill table with version-specific encoder
    select case (self%vcode)
    case (5002,5003,5004,5005)
       cvcode="5002"
       error = encode_vert_5002(self,update_L=.true.)
    case DEFAULT
       write(for_msg,*) 'table_update unsupported kind and version: ',kind,version,' (vcode) ',self%vcode
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    end select
    if (error /= VGD_OK) then
       write(for_msg,'(i4)') self%vcode
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       write(for_msg,*) 'problem with encode_vert_'//trim(cvcode)
       call msg(MSG_ERROR,VGD_PRFX//for_msg)
       return
    endif

    status = VGD_OK
    return
    
 end function table_update

end module vGrid_Descriptors
