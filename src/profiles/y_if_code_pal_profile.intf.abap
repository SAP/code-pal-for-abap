INTERFACE y_if_code_pal_profile PUBLIC.

  CONSTANTS: BEGIN OF types,
               checks    TYPE tabname VALUE 'YTAB_CHECKS',
               delegates TYPE tabname VALUE 'YTAB_DELEGATES',
               profiles  TYPE tabname VALUE 'YTAB_PROFILES',
             END OF types.

  TYPES profile_assignments TYPE STANDARD TABLE OF ytab_profiles WITH DEFAULT KEY.
  TYPES delegate_assigments TYPE STANDARD TABLE OF ytab_delegates WITH DEFAULT KEY.
  TYPES check_assignments TYPE STANDARD TABLE OF ytab_checks WITH DEFAULT KEY.

  TYPES: BEGIN OF file,
           profile   TYPE ytab_profiles,
           checks    TYPE check_assignments,
           delegates TYPE delegate_assigments,
         END OF file.

  TYPES: BEGIN OF ts_profile,
           profile TYPE ycicc_profile,
         END OF ts_profile.

  TYPES profile_names TYPE STANDARD TABLE OF ts_profile WITH DEFAULT KEY.

  TYPES: BEGIN OF check_description,
          checkid     TYPE vseoclass-clsname,
          description TYPE vseoclass-descript,
        END OF check_description.

  TYPES check_descriptions TYPE STANDARD TABLE OF check_description WITH DEFAULT KEY.
  TYPES value_help TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

  CLASS-METHODS create RETURNING VALUE(result) TYPE REF TO y_if_code_pal_profile.
  CLASS-METHODS get_checks_from_db RETURNING VALUE(result) TYPE tt_tadir.

  METHODS select_profiles IMPORTING username     TYPE syst_uname
                          RETURNING VALUE(result) TYPE profile_assignments
                          RAISING ycx_code_pal_entry_not_found.

  METHODS select_all_profiles RETURNING VALUE(result) TYPE profile_assignments
                              RAISING ycx_code_pal_entry_not_found.

  METHODS select_checks IMPORTING profile      TYPE ytab_checks-profile
                        RETURNING VALUE(result) TYPE check_assignments
                        RAISING ycx_code_pal_entry_not_found.

  METHODS select_delegates IMPORTING profile      TYPE ytab_delegates-profile
                           RETURNING VALUE(result) TYPE delegate_assigments
                           RAISING ycx_code_pal_entry_not_found.

  METHODS select_existing_checks RETURNING VALUE(result) TYPE check_descriptions
                                 RAISING ycx_code_pal_entry_not_found.

  METHODS delete_profile IMPORTING profile TYPE ytab_profiles
                         RAISING ycx_code_pal_remove_a_line.

  METHODS delete_profiles RAISING ycx_code_pal_remove_a_line.

  METHODS delete_check IMPORTING check TYPE ytab_checks
                       RAISING ycx_code_pal_remove_a_line.

  METHODS delete_delegate IMPORTING delegate TYPE ytab_delegates
                          RAISING ycx_code_pal_remove_a_line.

  METHODS import_profile IMPORTING structure TYPE file
                         RAISING ycx_code_pal_add_a_line
                                 ycx_code_pal_time_overlap
                                 ycx_code_pal_delegation_rights.

  METHODS insert_profile IMPORTING profile TYPE ytab_profiles
                         RAISING ycx_code_pal_add_a_line.

  METHODS insert_check IMPORTING check TYPE ytab_checks
                       RAISING ycx_code_pal_add_a_line
                               ycx_code_pal_time_overlap.

  METHODS insert_delegate IMPORTING delegate TYPE ytab_delegates
                          RAISING ycx_code_pal_add_a_line.

  METHODS get_registered_profiles RETURNING VALUE(result) TYPE profile_names
                                  RAISING ycx_code_pal_entry_not_found.

  METHODS get_check_description IMPORTING classname    TYPE vseoclass-clsname
                                RETURNING VALUE(result) TYPE vseoclass-descript
                                RAISING ycx_code_pal_entry_not_found.

  METHODS check_delegation_rights IMPORTING profile TYPE ytab_profiles-profile
                                  RAISING ycx_code_pal_delegation_rights.

  METHODS check_time_overlap IMPORTING check          TYPE ytab_checks
                                       selected_check TYPE ytab_checks OPTIONAL
                             RAISING ycx_code_pal_time_overlap.

  METHODS register_standard_profile RAISING cx_failed.
  METHODS cleanup_profile IMPORTING profile TYPE ycicc_profile.
  METHODS remove_all_checks IMPORTING profile TYPE ycicc_profile.
  METHODS remove_all_delegates IMPORTING profile TYPE ycicc_profile.

  METHODS profile_exists IMPORTING name          TYPE ytab_profiles-profile
                         RETURNING VALUE(result) TYPE abap_bool.

  METHODS mass_change IMPORTING name                          TYPE ytab_profiles-profile
                                config                        TYPE ytab_checks
                                change_validation_period      TYPE abap_bool
                                change_created_since          TYPE abap_bool
                                change_prio                   TYPE abap_bool
                                change_apply_prod_code        TYPE abap_bool
                                change_apply_testcode         TYPE abap_bool
                                change_allow_exemptios        TYPE abap_bool
                                change_evaluate_new_child_obj TYPE abap_bool
                      RAISING cx_failed.

ENDINTERFACE.
