REPORT y_code_pal_profile.

CONSTANTS main_screen TYPE screen-group1 VALUE '100'.
CONSTANTS create_profile_screen TYPE screen-group1 VALUE '200'.
CONSTANTS assign_delegate_screen TYPE screen-group1 VALUE '300'.
CONSTANTS check_customizing_screen TYPE screen-group1 VALUE '400'.
CONSTANTS new_profile_template_screen TYPE screen-group1 VALUE '500'.
CONSTANTS copy_profile_screen TYPE screen-group1 VALUE '600'.
CONSTANTS mass_update TYPE screen-group1 VALUE '700'.

INCLUDE y_code_pal_profile_interfaces.

DATA profiles_tree TYPE REF TO lif_alv_tree_control.
DATA checks_tree TYPE REF TO lif_alv_tree_control.
DATA delegates_tree TYPE REF TO lif_alv_tree_control.
DATA profile_manager TYPE REF TO Y_IF_CODE_PAL_PROFILE.
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
DATA chbx_evaluate_new_child_obj TYPE abap_bool.
DATA io_pcom_name TYPE sci_pcom.

DATA chbx_change_vp TYPE abap_bool.
DATA chbx_change_since TYPE abap_bool.
DATA chbx_message_prio TYPE abap_bool.
DATA chbx_select_prodcode TYPE abap_bool.
DATA chbx_apply_testcode TYPE abap_bool.
DATA chbx_apply_pcom TYPE abap_bool.
DATA chbx_apply_evaluate_new_child TYPE abap_bool.

DATA io_issue_prio TYPE char12.

INCLUDE y_code_pal_profile_classes.

START-OF-SELECTION.
  profile_manager = NEW y_code_pal_profile( ).
  lcl_util=>init_profiles( sy-repid ).
  lcl_util=>init_checks( sy-repid ).
  lcl_util=>init_delegates( sy-repid ).

END-OF-SELECTION.

  CALL SCREEN main_screen.

MODULE status_0100 OUTPUT.
  SET PF-STATUS main_screen.
  SET TITLEBAR main_screen.
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS create_profile_screen.
  SET TITLEBAR create_profile_screen.
ENDMODULE.

MODULE status_0300 OUTPUT.
  SET PF-STATUS assign_delegate_screen.
  SET TITLEBAR assign_delegate_screen.
ENDMODULE.

MODULE status_0400 OUTPUT.
  SET PF-STATUS check_customizing_screen.
  SET TITLEBAR check_customizing_screen.
  lcl_util=>init_ui_400( ).
ENDMODULE.

MODULE status_0500 OUTPUT.
  SET PF-STATUS new_profile_template_screen.
  SET TITLEBAR new_profile_template_screen.
ENDMODULE.

MODULE status_0600 OUTPUT.
  SET PF-STATUS copy_profile_screen.
  SET TITLEBAR copy_profile_screen.
ENDMODULE.

MODULE status_0700 OUTPUT.
  SET PF-STATUS mass_update.
  SET TITLEBAR mass_update.
ENDMODULE.

MODULE user_command INPUT.
  IF user_command CP 'ENTR_*'.
    LEAVE TO SCREEN 0.
  ENDIF.

  CASE user_command.
    WHEN 'BACK' OR
         'EXIT' OR
         'ESC'.
      LEAVE TO SCREEN 0.

    WHEN 'PICK'.

      CASE lcl_util=>get_cursor_field( ).
        WHEN 'IO_PROFILENAME'.
          LOOP AT SCREEN.
            IF screen-group1 = create_profile_screen.
              lcl_util=>profile_f4help_200( ).

            ELSEIF screen-group1 = copy_profile_screen.
              lcl_util=>profile_f4help_600( ).

            ENDIF.
          ENDLOOP.

        WHEN 'IO_CHECK_ID'.
          lcl_util=>check_f4help( ).

      ENDCASE.

    WHEN 'BTN_INFO'.
      lcl_util=>call_check_info( ).

  ENDCASE.
ENDMODULE.

MODULE profiles_f4help_200 INPUT.
  lcl_util=>profile_f4help_200( ).
ENDMODULE.

MODULE profiles_f4help_600 INPUT.
  lcl_util=>profile_f4help_600( ).
ENDMODULE.

MODULE checks_f4help INPUT.
  lcl_util=>check_f4help( ).
ENDMODULE.

MODULE checks_f1help INPUT.
  lcl_util=>call_check_info( ).
ENDMODULE.
