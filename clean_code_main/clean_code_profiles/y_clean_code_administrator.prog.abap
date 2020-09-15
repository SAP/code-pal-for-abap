REPORT y_clean_code_administrator.

INCLUDE y_clean_code_admin_data.
INCLUDE y_clean_code_admin_classes.

START-OF-SELECTION.
  profile_manager = NEW y_profile_manager( ).
  lcl_util=>init_profiles( sy-repid ).
  lcl_util=>init_checks( sy-repid ).
  lcl_util=>init_delegates( sy-repid ).

END-OF-SELECTION.

  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR '200'.
ENDMODULE.

MODULE status_0300 OUTPUT.
  SET PF-STATUS '300'.
  SET TITLEBAR '300'.
ENDMODULE.

MODULE status_0400 OUTPUT.
  SET PF-STATUS '400'.
  SET TITLEBAR '400'.
  lcl_util=>init_check_fields_active( io_check_id ).
ENDMODULE.

MODULE status_0500 OUTPUT.
  SET PF-STATUS '500'.
  SET TITLEBAR '500'.
ENDMODULE.

MODULE status_0600 OUTPUT.
  SET PF-STATUS '600'.
  SET TITLEBAR '600'.
ENDMODULE.

MODULE user_command INPUT.
  CASE user_command.
    WHEN 'BACK' OR
         'EXIT' OR
         'ESC' OR
         'ENTR_200' OR
         'ENTR_300' OR
         'ENTR_400' OR
         'ENTR_500' OR
         'ENTR_600'.
      LEAVE TO SCREEN 0.

    WHEN 'PICK'.

      CASE lcl_util=>get_cursor_field( ).
        WHEN 'IO_PROFILENAME'.
          LOOP AT SCREEN.
            IF screen-group1 EQ '200'.
              lcl_util=>profile_f4help_200( ).

            ELSEIF screen-group1 EQ '600'.
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
