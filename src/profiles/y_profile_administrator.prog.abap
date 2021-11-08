REPORT y_profile_administrator.

CONSTANTS main_screen TYPE screen-group1 VALUE '100'.
CONSTANTS create_profile_screen TYPE screen-group1 VALUE '200'.
CONSTANTS assign_delegate_screen TYPE screen-group1 VALUE '300'.
CONSTANTS check_customizing_screen TYPE screen-group1 VALUE '400'.
CONSTANTS new_profile_template_screen TYPE screen-group1 VALUE '500'.
CONSTANTS copy_profile_screen TYPE screen-group1 VALUE '600'.
CONSTANTS mass_update TYPE screen-group1 VALUE '700'.

INCLUDE y_profile_admin_data.
INCLUDE y_profile_admin_classes.

START-OF-SELECTION.
  profile_manager = NEW y_profile_manager( ).
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
  lcl_util=>init_ui_700( ).
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

    WHEN 'CHBX'.
      lcl_util=>init_ui_700( ).

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
