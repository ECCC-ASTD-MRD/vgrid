make tests ONLY=constructor_gen_1001
make tests ONLY=constructor_gen_1002_5001
make tests ONLY=constructor_gen_5002_5003_5005
make tests ONLY=constructor_build_2001
make tests ONLY=constructor_build_2001_stat
make tests ONLY=constructor_build_all
make tests ONLY=constructor_build_vert
make tests ONLY=constructor_gen_wrong_version
make tests ONLY=constructor
make tests ONLY=read_write_toc
make tests ONLY=read_write_read_all
make tests ONLY=put_invalid
make tests ONLY=get_put_get
make tests ONLY=constructor_any
make tests ONLY=constructor_simple_gen_1001 necessite vgd_print
make tests ONLY=get_excl #Ce test me semble insignifiant
make tests ONLY=get_ip1converted
make tests ONLY=get_ip1set_value
make tests ONLY=get_set_diag_height
make tests ONLY=read_toc_get_wrong_key

# Current work:
# make tests ONLY=get_quiet # C_get_real_1d, C_get_char, C_get_logical pas fait

# Next todo:
#make tests ONLY=get_case_stat
#make tests ONLY=get_char
#make tests ONLY=get_kind_version
#make tests ONLY=get_levels_from_keys
#make tests ONLY=get_logp
#make tests ONLY=get_nonalpha
#make tests ONLY=get_rpn_vert_value

#make tests ONLY=get_valid

