DATA profiles_tree TYPE REF TO y_if_alv_tree_control.
DATA checks_tree TYPE REF TO y_if_alv_tree_control.
DATA delegates_tree TYPE REF TO y_if_alv_tree_control.
DATA profile_manager TYPE REF TO y_if_profile_manager.
DATA has_edit_mode_started TYPE abap_bool.


DATA user_command TYPE syst_ucomm.
DATA io_profilename TYPE ytab_profiles-profile.
DATA io_to_profile TYPE ytab_profiles-profile.
DATA io_delegate_name TYPE string.
DATA io_check_id TYPE vseoclass-clsname.
DATA io_check_description TYPE string.
DATA io_start_date TYPE dats.
DATA io_end_date   TYPE dats.
DATA io_creation_date TYPE dats.
DATA io_threshold TYPE ytab_checks-threshold.
DATA io_prio TYPE ytab_checks-prio.
DATA chbx_on_testcode TYPE abap_bool.
DATA chbx_on_prodcode TYPE abap_bool.
DATA chbx_allow_pcom TYPE abap_bool.
DATA lbl_pcom_name TYPE sci_pcom.
