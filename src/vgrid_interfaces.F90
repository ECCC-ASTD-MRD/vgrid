module vgrid_5100
  use Vgrid_descriptors

  interface vgd_levels_NewIfc
    module procedure vgd_compute_pressures_5100
  end interface vgd_levels_NewIfc

  interface vgd_dpidpis_NewIfc
    module procedure vgd_compute_dpidpis_5100
  end interface vgd_dpidpis_NewIfc
end module vgrid_5100


!module vgrid_5001_5005
!  use Vgrid_descriptors
!
!  interface vgd_levels_NewIfc
!    module procedure vgd_compute_pressures_5001_5005
!  end interface vgd_levels_NewIfc
!
!  interface vgd_dpidpis_NewIfc
!    module procedure vgd_compute_dpidpis_5001_5005
!  end interface vgd_dpidpis_NewIfc
!end module vgrid_5001_5005


module vgrid_generic
  use Vgrid_descriptors

  interface vgd_levels_NewIfc
    module procedure vgd_compute_pressures_5100
  ! module procedure vgd_compute_pressures_5001_5005  ! Also for 1003
 end interface vgd_levels_NewIfc

  interface vgd_dpidpis_NewIfc
    module procedure vgd_compute_dpidpis_5100
  ! module procedure vgd_compute_dpidpis_5001_5005    ! Also for 1003
 end interface vgd_dpidpis_NewIfc
end module vgrid_generic
