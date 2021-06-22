#include "vgrid_build_info.h"

module mod_comp_pres
   !
   implicit none
   !
   private
   !
   Public :: rpn, my_fstprm,get_pres_by_group
   !
   integer, public, parameter :: COMP_PRES_ERROR=0,COMP_PRES_OK=1
   integer, public :: my_kind,my_version
   logical, public :: verbose_L, allow_sigma_L
   !
   type rpn
      real, dimension(:,:), pointer :: data=>null()
      integer ::dateo, datev, datyp, deet, dltf, extra1, extra2, extra3, ig1,&
           ig2, ig3, ig4, ip1, ip2, ip3, iun, key, lng, nbits,&
           ni,  nj, nk, npak, npas, swa, ubc
      character(len=12) :: etiket
      character(len=4)  :: nomvar
      character(len=2)  :: typvar
      character(len=1)  :: grtyp, ctype
      logical :: rewrit
   end type rpn
   !
   !=======
   contains
   !=======
   !
   !=======================================================
   integer function my_fstprm(fstkey,record) result(status)
      !
      implicit none
      !
      integer, intent(in) :: fstkey
      type(rpn) :: record
      ! 
      ! Local variables
      !
      integer :: error
      real*8 :: nhours
      !
      !external
      !
      integer, external :: fstprm
      !
      status = COMP_PRES_ERROR
      !
      error=fstprm(fstkey,record%dateo,record%deet,record%npas, &
           record%ni,record%nj,record%nk,record%nbits,record%datyp,record%ip1,record%ip2, &
           record%ip3,record%typvar,record%nomvar,record%etiket,record%grtyp, &
           record%ig1,record%ig2,record%ig3,record%ig4,record%swa, &
           record%lng,record%dltf,record%ubc,record%extra1,record%extra2, &
           record%extra3)
      if (error < 0) then
         write(6,*) 'ERROR: in my_fstprm, cannot fstprm for fstkey ',fstkey
         return
      end if
      nhours=record%deet*record%npas/3600.d0
      call incdatr(record%datev,record%dateo,nhours)
      !
      status = COMP_PRES_OK
      !
   end function my_fstprm
   !=======================================================================
   integer function get_pres_by_group(F_lui,F_luo,F_group) result(status)
      !
      use vGrid_Descriptors, only: vgd_new,vgd_levels,vgd_get, &
           vgrid_descriptor,vgd_print,vgd_write,&
           VGD_LEN_RFLD, VGD_LEN_RFLS, VGD_NO_REF_NOMVAR,VGD_OK
      !
      implicit none
      !    
      integer :: F_lui,F_luo
      character(len=*) :: F_group
      !
      !Local variable
      !
      type(rpn) :: record,record2
      type (vgrid_descriptor) :: vgd
      integer, parameter :: nmax=1000
      integer, dimension(nmax) :: liste,liste2
      integer :: nk2,nk3,ier,fstinl,fstluk,fstecr,fstinf,ni,nj,nk,infon,n_ip1,i,j,kind,version,key
      integer, dimension(:), pointer :: ip1_list,ip1_list2,ip1_list3,ip1_single
      character(len=VGD_LEN_RFLD) :: nomvar
      character(len=VGD_LEN_RFLS) :: nomvar_ls
      real :: dummy
      real, dimension(:,:,:), pointer :: pres
      real, dimension(:,:), pointer :: work,p0ls
      real :: pp
      logical :: OK_2001_L, final_ip1_list_L
      !
      status=COMP_PRES_ERROR
      !
      nullify(ip1_list,ip1_list2,ip1_list3,ip1_single,pres,work,p0ls,record%data)
      !
      ier=vgd_new(vgd,F_lui,'fst',kind=my_kind,version=my_version)
      if(ier.ne.VGD_OK)then
         print*,'ERROR with vgd_new for var  (group) ',F_group
         return
      endif
      ier=vgd_get(vgd,'KIND - vertical coordinate ip1 kind', kind)
      if(ier.ne.VGD_OK)then
         print*,'ERROR with vgd_get on KIND'
         return
      endif
      if( kind .eq. 21 )then
          print*,'ERROR pressure computation for heights coordinate is not implemented yet,'
          print*,'      please contact the development team if you need this feature.'
          return
      endif
      !
      if(verbose_L)then
         ier=vgd_print(vgd)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_print'
            call exit(1)
         endif
      endif
      !
      allocate(ip1_single(1))
      !
      OK_2001_L=.false.
      final_ip1_list_L=.true.
      if(trim(F_group).eq.'THERMO')then
         ier=vgd_get(vgd,'VIPT - level ip1 list (t)',value=ip1_list)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_get on VIPT for var (group) ',F_group
            return
         endif
      else if(trim(F_group).eq.'MOMENTUM')then
         ier=vgd_get(vgd,'VIPM - level ip1 list (m)',value=ip1_list)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_get on VIPM for var (group) ',F_group
            return
         endif
      else if (trim(F_group).eq.'ALL_LEVELS')then
         ier=vgd_get(vgd,'VIPT - level ip1 list (t)',value=ip1_list2)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_get on VIPT for var (group) ',F_group
            return
         endif
         ier=vgd_get(vgd,'VIPM - level ip1 list (m)',value=ip1_list3)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_get on VIPM for var (group) ',F_group
            return
         endif
         nk2=size(ip1_list2)
         nk3=size(ip1_list3)
         allocate(ip1_list(nk2+nk3))
         ip1_list(1:nk2)=ip1_list2
         ip1_list(nk2+1:nk2+nk3)=ip1_list3
      else
         OK_2001_L=.true.
         final_ip1_list_L=.false. ! If there is more than one P0 in file, the ip1_list will contain all 
                                  !    ip1 for nomvar F_group and there will be repetitions.
                                  !    The ip1_list will have to be rebuild once the datev of P0 is known in P0 loop below.
         ier=fstinl(F_lui,ni,nj,nk,-1,' ',-1,-1,-1,' ',F_group,liste,n_ip1,nmax)           
         if(ier.lt.0)then
            print*,'ERROR with fstinl on ',F_group
            return
         endif
         if(n_ip1.eq.0)then
            print*,'No record of nomvar ',trim(F_group),' in input file'
            return
         endif         
         allocate(ip1_list(n_ip1),stat=ier)
         if(ier.ne.0)then
            print*,'Problem in allocate ip1_list(n_ip1)'
            return
         endif
         do i=1,n_ip1
            ier=my_fstprm(liste(i),record)
            if(ier.ne.COMP_PRES_OK)then
               print*,'ERROR with my_fstprm on ',nomvar
               return
            endif
            ip1_list(i)=record%ip1
         enddo
         kind = my_kind
         if ( kind == -1 )then
            ! Loop on ip1_list to find kind other then 4 (diag level)
            do i=1,n_ip1
               call convip(ip1_list(i), pp, kind, -1,"",.false.)
               if(kind.ne.4)exit
            enddo
         endif
      endif
      n_ip1=size(ip1_list)
      !
      ier=vgd_get(vgd,'KIND - vertical coordinate ip1 kind', kind)
      if(ier.ne.VGD_OK)then
         print*,'ERROR with vgd_get on KIND'
         return
      endif
      ier=vgd_get(vgd,'VERS - vertical coordinate version' , version)
      if(ier.ne.VGD_OK)then
         print*,'ERROR with vgd_get on VERS'
         return
      endif
      if(kind*1000+version == 2001) then
         if(.not. OK_2001_L)then
            print*,'ERROR -var trim(F_group) not allowed with pressure level file.'
            print*,'       Use a nomvar present in the input file and rerun the program e.g. -var TT'
            return
         endif
         ! Get the list of field at dirrerent time, this will be use has P0 but 
         ! will never be used in computation since 2001 is pressure levels         
         
         key=fstinf(F_lui,ni,nj,nk,-1,' ',record%ip1,-1,-1,' ',F_group)
         if(key.lt.0)then
            print*,'ERROR : cannot find variable ',nomvar,' in input file'
            return
         endif
         nomvar=F_group
      else
         ier=vgd_get(vgd,'RFLD - reference field name',value=nomvar)
         if(ier.ne.VGD_OK)then
            print*,'ERROR with vgd_get on RFLD'
            return
         endif
         ! Use wild card to find RFLD
         record%ip1=-1
      endif
      !
      ier=fstinl(F_lui,ni,nj,nk,-1,' ',record%ip1,-1,-1,' ',nomvar,liste,infon,nmax)
      if(ier.lt.0)then
         print*,'ERROR with fstinl on ',nomvar
         return
      endif
      if(infon.eq.0)then
         print*,'No record ',nomvar,' in input file'
         return
      endif
      !
      ! Loop on all p0
      !
      LOOP_ON_P0: do i=1,infon
         !
         ier=my_fstprm(liste(i),record)
         if(ier.ne.COMP_PRES_OK)then
            print*,'ERROR with my_fstprm on ',nomvar
            return
         endif
         !
         ier=vgd_write(vgd,F_luo,'fst')
         if(record%ni.ne.ni.or. &
            record%nj.ne.nj.or. &
            record%nk.ne.nk)then
            print*,'Size of record ',nomvar,' inconsistant with previous one:'
            print*,'ni',record%ni,'vs',ni
            print*,'nj',record%nj,'vs',nj
            print*,'nk',record%nk,'vd',nk
            return
         endif
         !
         if(associated(record%data))deallocate(record%data)
         allocate(record%data(ni,nj),stat=ier)
         if(ier.ne.0)then
            print*,'Problem in allocate record%data'
            return
         endif
         if(associated(work))deallocate(work)
         allocate(work(ni,nj),stat=ier)
         if(ier.ne.0)then
            print*,'Problem in allocate work'
            return
         endif
         !
         ier=fstluk(record%data,liste(i),ni,nj,nk)
         if(ier.lt.0)then
            print*,'ERROR with fstluk on ',nomvar
            return
         endif
         !
         work=record%data
         if(trim(nomvar) == "P0")record%data=record%data*100.

         ier=vgd_get(vgd,'RFLS - large scale reference field name',value=nomvar_ls,quiet=.true.)         
         if(nomvar_ls == VGD_NO_REF_NOMVAR)then
            if(associated(p0ls))deallocate(p0ls)
         else
            key = fstinf(F_lui,ni,nj,nk,record%datev,record%etiket,record%ip1,record%ip2,-1,' ',nomvar_ls)
            if(key < 0)then
               print*,'ERROR : cannot find large scale reference field ',nomvar_ls,' with the following parameters'
               write(6,'("datev: ",i12,", Etiket: ",a,", ip1: ",i4,", ip2: ",i4)') &
                    record%datev,record%etiket,record%ip1,record%ip2
               return
            endif
            if(associated(p0ls))deallocate(p0ls)
            allocate(p0ls(ni,nj),stat=ier)
            if(ier.ne.0)then
               print*,'Problem in allocate p0ls'
               return
            endif
            ier=fstluk(p0ls,key,ni,nj,nk)
            if(ier.lt.0)then
               print*,'ERROR with fstluk on ',nomvar_ls
               return
            endif
            if(trim(nomvar_ls) == "P0LS")p0ls=p0ls*100.
         endif
         
         ! Loop on ip1_list was added since memory allocation for the cube was too large with
         ! some global grids.        

         if(.not. final_ip1_list_L)then
            ! Get ip1_list for current P0 datev
            ier=fstinl(F_lui,ni,nj,nk,record%datev,' ',-1,-1,-1,' ',F_group,liste2,n_ip1,nmax)           
            if(ier.lt.0)then
               print*,'ERROR with fstinl 2 on ',F_group,' datev = ',record%datev
               return
            endif
            if(n_ip1.eq.0)then
               print*,'No record of nomvar ',trim(F_group),' datev = ',record%datev,' in input file'
               return
            endif
            ! Note we got a subset of ip1s, therefore ip1_list is long enough.
            do j=1,n_ip1
               ier=my_fstprm(liste2(j),record2)
               if(ier.ne.COMP_PRES_OK)then
                  print*,'ERROR with my_fstprm 2 on ',nomvar
                  return
               endif
               ip1_list(j)=record2%ip1
            enddo            
         endif

         LOOP_ON_IP1_LIST: do j=1,n_ip1
            ip1_single=ip1_list(j)
            if(associated(p0ls))then
               ier=vgd_levels(vgd,sfc_field=record%data,sfc_field_ls=P0ls,ip1_list=ip1_single,levels=pres)            
            else
               ier=vgd_levels(vgd,sfc_field=record%data,ip1_list=ip1_single,levels=pres)            
            endif
            if(ier.ne.VGD_OK)then
               print*,'ERROR with vgd_levels for var (group) ',F_group
               return
            endif
            pres=pres/100.
            ier=fstecr(pres,dummy,-record%nbits,F_luo,record%dateo,record%deet,record%npas, &
                 record%ni,record%nj,record%nk,ip1_single,record%ip2,record%ip3, &
                 record%typvar,'PX',record%etiket,record%grtyp, &
                 record%ig1,record%ig2,record%ig3,record%ig4,record%datyp,.true.)
            if(ier.lt.0)then
               print*,'ERROR with fstecr for var (group) ',F_group
               return
            endif
         enddo LOOP_ON_IP1_LIST
         !
      enddo LOOP_ON_P0
      deallocate(record%data,work,ip1_single)
      if(associated(pres )    )deallocate(pres)
      if(associated(ip1_list) )deallocate(ip1_list)
      if(associated(ip1_list2))deallocate(ip1_list2)
      if(associated(ip1_list3))deallocate(ip1_list3)
      if(associated(work)     )deallocate(work)
      if(associated(p0ls)     )deallocate(p0ls)
      !
      status=COMP_PRES_OK
      !
      return
      !
   end function get_pres_by_group
end module mod_comp_pres
!========================================================================
program compute_pressure
   !
   ! Modifs 
   !   Andre Plante Oct 2014, deal with many !! in file by adding -vcode key
   !   ...
   !   Andre Plante May 2016 4.1.0, do not use Vcode 5100 SLEVE since it is not
   !                                readdy yet. 
   use mod_comp_pres, only: rpn, my_fstprm,get_pres_by_group, &
        COMP_PRES_ERROR,verbose_L,my_kind,my_version,allow_sigma_L
   use vGrid_Descriptors, only: vgd_putopt
   !
   implicit none
   !
   integer, parameter :: ncle=8,lui=10,luo=11, nmax=1000
   integer :: stat,npos,exdb,exfin
   integer :: fnom,fstouv,fstfrm
   character(len=256), dimension(ncle) :: cle,val,def
   character(len=10) :: my_var
   character(len=12), parameter :: version='4.2.0'
   !
   cle=(/'s.         ','d.         ','samefile   ','var        ','verbose    ','kind       ','version    ','allow_sigma'/)
   val=(/'undef      ','undef      ','NO         ','undef      ','NO         ','undef      ','undef      ','NO         '/)
   def=(/'undef      ','undef      ','YES        ','undef      ','YES        ','undef      ','undef      ','YES        '/)
   !
   stat=exdb('r.compute_pressure',version,'NON')
   write(6,'("   * ",a)')PROJECT_VERSION_STRING
   write(6,'("   ******************************************************************************************************")')
   !
   npos=-111
   call ccard(cle,def,val,ncle,npos) 
   !
   if(trim(val(1)).eq.'undef')then
      print*,'usage : r.compute_pressure -s sorce_file -d destination_file -var (NOMVAR,THERMO,MOMENTUM,ALL_LEVELS)'
      stop
   endif
   !
   if(trim(val(2)).eq.'undef')then
      print*,'usage : r.compute_pressure -s sorce_file -d destination_file -var (NOMVAR,THERMO,MOMENTUM,ALL_LEVELS)'
      stop
   endif
   !
   my_var=val(4)
   !

   if(trim(val(6)).eq.'undef')then
      my_kind=-1
   else
      read(val(6),*)my_kind
   endif
   !read(val(6),*)my_kind
   if(my_kind.ne.-1)then
      print*,'Looking for vertical descriptor of kind ',my_kind
   endif   
   !
   if(trim(val(7)).eq.'undef')then
      my_version=-1
   else
      read(val(7),*)my_version
   endif
   if(my_kind.eq.-1.and.my_version.ne.-1)then
      print*,'ERROR; argument -kind must be used with option -version'
      call exit(1)
   endif
   
   !read(val(7),*)my_version
   if(my_version.ne.-1)then
      print*,'Looking for vertical descriptor of version ',my_version
   endif 
   !

   verbose_L=.false.
   if(val(5).eq.'YES')verbose_L=.true.
   !
   stat=fnom(lui,val(1),"RND+R/O",0)
   if(stat.lt.0)then
      print*,'ERROR with fnom on lui'
      call exit(1)
   endif
   stat=fstouv(lui,'RND')
   if(stat.le.0)then
      print*,'No record in RPN file'
      call exit(1)
   endif
   !
   stat=fnom(luo,val(2),'RND',0)
   if(stat.lt.0)then
      print*,'ERROR with fnom on file ',trim(val(2))
      call exit(1)
   endif
   stat=fstouv(luo,'RND')
   if(stat.lt.0)then
      print*,'ERROR: problem with fstouv on ',trim(val(2))
      stat=fstfrm(luo)
      call exit(1)
   endif
   !
   if(trim(val(8)).eq.'NO')then
      allow_sigma_L=.false.
   else if (trim(val(8)).eq.'YES')then
      allow_sigma_L=.true.
   else
      print*,"ERROR: option -allow_sigma does't take any value"
      call exit(1)
   endif
   
   stat = vgd_putopt("ALLOW_SIGMA",allow_sigma_L)
   !
   stat=get_pres_by_group(lui,luo,my_var)      
   if(stat.eq.COMP_PRES_ERROR)then
      print*,'ERROR: with get_pres_by_group for ',trim(my_var)
      call exit(1)
   endif
   !
   stat=fstfrm(lui)
   stat=fstfrm(luo)
   !
   stat=exfin('r.compute_pressure',version,'NON')
   !
end program compute_pressure
