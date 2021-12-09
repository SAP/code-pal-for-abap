*&---------------------------------------------------------------------*
*& Include y_code_pal_profile_interfaces
*&---------------------------------------------------------------------*
INTERFACE lif_list.

  METHODS append IMPORTING line TYPE any.
  METHODS delete_all.
  METHODS delete_at IMPORTING index TYPE i.

  METHODS get_line_at IMPORTING index         TYPE i
                      RETURNING VALUE(result) TYPE REF TO data.

  METHODS insert_at IMPORTING line  TYPE any
                              index TYPE i.

  METHODS number_of_rows RETURNING VALUE(result) TYPE i.

  METHODS is_contained IMPORTING line          TYPE any
                       RETURNING VALUE(result) TYPE abap_bool.

  METHODS set_table IMPORTING table TYPE STANDARD TABLE.

  METHODS get_table RETURNING VALUE(result) TYPE REF TO data.

  METHODS get_type_name RETURNING VALUE(result) TYPE string.

  METHODS get_line_index IMPORTING line          TYPE any
                         RETURNING VALUE(result) TYPE i.

ENDINTERFACE.



INTERFACE lif_alv_tree_control.

  METHODS list_control RETURNING VALUE(result) TYPE REF TO lif_list.

  METHODS set_field_visibility IMPORTING fieldname  TYPE tabname
                                         is_visible TYPE abap_bool DEFAULT abap_true.

  METHODS set_field_header_text IMPORTING fieldname   TYPE tabname
                                          header_text TYPE lvc_s_fcat-coltext.

  METHODS refresh_display.
  METHODS init_display.
  METHODS to_focus.

  METHODS get_selected_line RETURNING VALUE(result) TYPE REF TO data
                            RAISING   ycx_code_pal_entry_not_found.

  METHODS get_selected_index RETURNING VALUE(result) TYPE i
                             RAISING   ycx_code_pal_entry_not_found.

  METHODS set_selected_index IMPORTING index TYPE i.

  METHODS toolbar_control RETURNING VALUE(result) TYPE REF TO cl_gui_toolbar
                          RAISING   cx_failed.

  METHODS activate_toolbar RAISING cx_failed.
  METHODS deactivate_toolbar RAISING cx_failed.

ENDINTERFACE.



INTERFACE lif_profile_manager.

  TYPES profile_assignments TYPE STANDARD TABLE OF ytab_profiles WITH DEFAULT KEY.
  TYPES delegate_assigments TYPE STANDARD TABLE OF ytab_delegates WITH DEFAULT KEY.
  TYPES check_assignments TYPE STANDARD TABLE OF ytab_checks WITH DEFAULT KEY.

  TYPES: BEGIN OF file,
           profile   TYPE ytab_profiles,
           checks    TYPE check_assignments,
           delegates TYPE delegate_assigments,
         END OF file.

  TYPES:
    BEGIN OF ts_profile,
      profile TYPE ycicc_profile,
    END OF ts_profile.

  TYPES profile_names TYPE STANDARD TABLE OF ts_profile WITH DEFAULT KEY.

  TYPES:
    BEGIN OF check_description,
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

  METHODS mass_change IMPORTING name                     TYPE ytab_profiles-profile
                                config                   TYPE ytab_checks
                                change_validation_period TYPE abap_bool
                                change_created_since     TYPE abap_bool
                                change_prio              TYPE abap_bool
                                change_apply_prod_code   TYPE abap_bool
                                change_apply_testcode    TYPE abap_bool
                                change_allow_exemptios   TYPE abap_bool
                      RAISING cx_failed.

ENDINTERFACE.



INTERFACE lif_alv_events.

  TYPES: BEGIN OF simple_event,
           eventid    TYPE int4,
           appl_event TYPE char1,
         END OF simple_event.

  TYPES simple_events TYPE STANDARD TABLE OF simple_event WITH DEFAULT KEY.

  CONSTANTS mode_double_click TYPE i VALUE 1.
  CONSTANTS mode_selection_changed TYPE i VALUE 2.

  METHODS get_events IMPORTING mode          TYPE i DEFAULT mode_double_click
                     RETURNING VALUE(result) TYPE simple_events.

  METHODS handle_function_selected FOR EVENT function_selected OF cl_gui_toolbar
                                   IMPORTING fcode.

  METHODS handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree_simple.
  METHODS handle_selection_changed FOR EVENT selection_changed OF cl_gui_alv_tree_simple.
  METHODS register_handler_to_alv_tree IMPORTING alv_tree TYPE REF TO cl_gui_alv_tree_simple.
  METHODS register_handler_to_toolbar IMPORTING toolbar TYPE REF TO cl_gui_toolbar.

ENDINTERFACE.
