CLASS y_clean_code_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_clean_code_manager.
    ALIASES calculate_obj_creation_date FOR y_if_clean_code_manager~calculate_obj_creation_date.
    ALIASES read_check_customizing FOR y_if_clean_code_manager~read_check_customizing.

  PRIVATE SECTION.
    METHODS determine_profiles RETURNING VALUE(result) TYPE string_table
                               RAISING   ycx_no_check_customizing.

    METHODS determine_checks IMPORTING profile       TYPE ycicc_profile
                                       checkid       TYPE seoclsname
                             RETURNING VALUE(result) TYPE y_if_clean_code_manager=>check_configurations
                             RAISING   ycx_no_check_customizing .
ENDCLASS.



CLASS Y_CLEAN_CODE_MANAGER IMPLEMENTATION.


  METHOD determine_checks.
    TRY.
        DATA(checks) = y_profile_manager=>create( )->select_checks( profile ).
      CATCH ycx_entry_not_found.
        RETURN.
    ENDTRY.

    LOOP AT checks ASSIGNING FIELD-SYMBOL(<check>)
    WHERE checkid = checkid
    AND start_date <= sy-datlo
    AND end_date >= sy-datlo.
      DATA(check_configuration) = VALUE y_if_clean_code_manager=>check_configuration( object_creation_date = <check>-objects_created_on
                                                                                      threshold = <check>-threshold
                                                                                      prio = <check>-prio
                                                                                      apply_on_productive_code = <check>-apply_on_productive_code
                                                                                      apply_on_testcode = <check>-apply_on_testcode
                                                                                      ignore_pseudo_comments = <check>-ignore_pseudo_comments ).
      result = VALUE #( BASE result ( CORRESPONDING #( check_configuration ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD determine_profiles.
    DATA obj_name TYPE string.
    DATA profile_db TYPE string VALUE `YTAB_PROFILE_REP`.
    DATA callstack TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = callstack.

    FIND FIRST OCCURRENCE OF `Y_CLEAN_CODE_REPORTING` IN TABLE callstack.
    IF sy-subrc = 0.
      SELECT SINGLE obj_name
      FROM tadir
      INTO @obj_name
      WHERE pgmid = 'R3TR'
      AND object = 'TABL'
      AND obj_name = @profile_db.

      IF sy-subrc = 0.
        SELECT profile
        FROM (profile_db)
        INTO TABLE @result.

        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    TRY.
        DATA(profiles) = y_profile_manager=>create( )->select_profiles( sy-uname ).
      CATCH ycx_entry_not_found.
        RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDTRY.

    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      APPEND <profile>-profile TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD calculate_obj_creation_date.
    result = y_object_creation_date=>get_created_on( object_type = object_type
                                                     object_name = object_name ).
  ENDMETHOD.


  METHOD read_check_customizing.
    TRY.
        DATA(profiles) = determine_profiles( ).
      CATCH ycx_no_check_customizing.
        RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDTRY.

    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      TRY.
          APPEND LINES OF determine_checks( profile = CONV #( <profile> )
                                            checkid = checkid ) TO result.
        CATCH ycx_no_check_customizing.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    IF lines( result ) = 0.
      RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
