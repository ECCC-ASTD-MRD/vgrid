program vcode
   !
   use vGrid_Descriptors, only: vgd_print,VGD_ERROR
   !
   implicit none
   !
   integer, parameter :: lu=10,ncle=1
   integer :: exdb,exfin,npos,stat,code
   character(len=12), parameter :: version='v_1.1.0'
   character(len=256), dimension(ncle) :: cle,val,def
#include "vgrid_version.hf"
   !
   !==========================================================================
   !   
   stat=exdb('r.vcode',version,'NON')
   write(6,'("   * ",a)')vgrid_descriptors_version
   write(6,'("   ******************************************************************************************************")')
   !
   ! Get keys
   cle=(/'-'/)
   val=(/' '/)
   def=(/' '/)
   npos=1
   call ccard(cle,def,val,ncle,npos) 
   !
   if(npos.eq.0)val='-1'
   read(val(1),*)code
   !print*,'val(1),code=',val(1),code
   if(vgd_print(code).eq.VGD_ERROR)then
      print*,'ERROR with vgd_print on vcode'
      call exit(1)
   endif
   
   stat=exfin('r.vcode',version,'NON')

end program vcode
