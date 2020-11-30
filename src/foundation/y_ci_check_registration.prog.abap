REPORT y_ci_check_registration.

DATA: BEGIN OF comments,
        title   TYPE string VALUE 'code pal for ABAP - Check Activation Tool (Local Only)',
        runmode TYPE string VALUE 'Please choose a run mode',
      END OF comments.

DATA: BEGIN OF messages,
        checks_not_found         TYPE string VALUE 'Code Pal Checks not found.',
        successfully_activated   TYPE string VALUE 'Entry(s) Successfully activated (category + checks)',
        failed_activation        TYPE string VALUE 'Entry(s) Failed to activate',
        successfully_deactivated TYPE string VALUE 'Entry(s) Successfully deactivated (category + checks)',
        failed_deactivation      TYPE string VALUE 'Entry(s) Failed to deactivate',
        done                     TYPE string VALUE 'Done!',
      END OF messages.

START-OF-SELECTION.

  SELECTION-SCREEN BEGIN OF BLOCK part0.
  SELECTION-SCREEN COMMENT /1(83) comm0 MODIF ID mg1.

  SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME.
  SELECTION-SCREEN COMMENT /1(60) comm1 MODIF ID mg1.
  PARAMETERS: p_activa RADIOBUTTON GROUP g1,
              p_deacti RADIOBUTTON GROUP g1,
              p_reacti RADIOBUTTON GROUP g1 DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK part1.
  SELECTION-SCREEN END OF BLOCK part0.

AT SELECTION-SCREEN OUTPUT.
  comm0 = comments-title.
  comm1 = comments-runmode.

START-OF-SELECTION.
  WRITE / comments-title.
  WRITE / space.

  IF p_deacti = abap_true
  OR p_reacti = abap_true.
    TRY.
        y_code_pal_register=>deactivate_all_checks( ).
        WRITE / |{ y_code_pal_register=>successes } { messages-successfully_deactivated }|.
        WRITE / |{ y_code_pal_register=>failures } { messages-failed_deactivation }|.
      CATCH cx_failed.
        MESSAGE messages-checks_not_found TYPE 'E'.
    ENDTRY.
  ENDIF.

  IF p_activa = abap_true
  OR p_reacti = abap_true.
    TRY.
        y_code_pal_register=>activate_all_checks( ).
        WRITE / |{ y_code_pal_register=>successes } { messages-successfully_activated }|.
        WRITE / |{ y_code_pal_register=>failures } { messages-failed_activation }|.
      CATCH cx_failed.
        MESSAGE messages-checks_not_found TYPE 'E'.
    ENDTRY.
  ENDIF.

  COMMIT WORK.

  WRITE / |{ messages-done }| .
