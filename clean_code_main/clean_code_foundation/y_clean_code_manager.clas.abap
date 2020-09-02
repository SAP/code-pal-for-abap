CLASS y_clean_code_manager DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_clean_code_manager.
    ALIASES calculate_obj_creation_date FOR y_if_clean_code_manager~calculate_obj_creation_date.
    ALIASES read_check_customizing FOR y_if_clean_code_manager~read_check_customizing..
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA object_creation_date TYPE REF TO y_if_object_creation_date.
    METHODS determine_profiles IMPORTING username TYPE xubname
                               RETURNING VALUE(result) TYPE string_table
                               RAISING ycx_no_check_customizing.
    METHODS determine_checks IMPORTING profile TYPE string
                                       checkid TYPE seoclsname
                                       obj_creation_date TYPE datum
                             RETURNING VALUE(result) TYPE y_if_clean_code_manager=>check_configurations
                             RAISING ycx_no_check_customizing .
ENDCLASS.


CLASS y_clean_code_manager IMPLEMENTATION.


  METHOD determine_checks.
    DATA profile_manager TYPE REF TO object.
    DATA check_configuration TYPE y_if_clean_code_manager=>check_configuration.
    DATA checks_ref TYPE REF TO data.
    DATA check_ref TYPE REF TO data.
    DATA profile_name_ref TYPE REF TO data.
    FIELD-SYMBOLS <checks> TYPE ANY TABLE.
    FIELD-SYMBOLS <check> TYPE any.
    FIELD-SYMBOLS <profile_name> TYPE any.
    FIELD-SYMBOLS <checkid> TYPE any.
    FIELD-SYMBOLS <start_date> TYPE any.
    FIELD-SYMBOLS <end_date> TYPE any.
    FIELD-SYMBOLS <objects_created_on> TYPE any.
    FIELD-SYMBOLS <threshold> TYPE any.
    FIELD-SYMBOLS <prio> TYPE any.
    FIELD-SYMBOLS <apply_on_productive_code> TYPE any.
    FIELD-SYMBOLS <apply_on_testcode> TYPE any.

    TRY.
        CREATE DATA checks_ref TYPE STANDARD TABLE OF (`YTAB_CHECKS`) WITH DEFAULT KEY.
        ASSIGN checks_ref->* TO <checks>.

        CREATE DATA profile_name_ref TYPE (`YCICC_PROFILE`).
        ASSIGN profile_name_ref->* TO <profile_name>.
        <profile_name> = profile.

        CREATE OBJECT profile_manager TYPE (`Y_PROFILE_MANAGER`).

        DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'PROFILE'
                                                      kind  = cl_abap_objectdescr=>exporting
                                                      value = REF #( <profile_name> ) )
                                                    ( name  = 'RESULT'
                                                      kind  = cl_abap_objectdescr=>returning
                                                      value = REF #( <checks> ) ) ).

        CALL METHOD profile_manager->(`Y_IF_PROFILE_MANAGER~SELECT_CHECKS`)
          PARAMETER-TABLE ptab.

        IF <checks> IS ASSIGNED.
          CREATE DATA check_ref TYPE (`YTAB_CHECKS`).
          ASSIGN check_ref->* TO <check>.

          LOOP AT <checks> ASSIGNING <check>.
            ASSIGN COMPONENT `CHECKID` OF STRUCTURE <check> TO <checkid>.
            ASSIGN COMPONENT `START_DATE` OF STRUCTURE <check> TO <start_date>.
            ASSIGN COMPONENT `END_DATE` OF STRUCTURE <check> TO <end_date>.
            ASSIGN COMPONENT `OBJECTS_CREATED_ON` OF STRUCTURE <check> TO <objects_created_on>.
            ASSIGN COMPONENT `THRESHOLD` OF STRUCTURE <check> TO <threshold>.
            ASSIGN COMPONENT `PRIO` OF STRUCTURE <check> TO <prio>.
            ASSIGN COMPONENT `APPLY_ON_PRODUCTIVE_CODE` OF STRUCTURE <check> TO <apply_on_productive_code>.
            ASSIGN COMPONENT `APPLY_ON_TESTCODE` OF STRUCTURE <check> TO <apply_on_testcode>.

            IF <checkid> = checkid AND
               <start_date> <= sy-datlo AND
               <end_date> >= sy-datlo AND
               <objects_created_on> <= obj_creation_date.

              check_configuration-object_creation_date = <objects_created_on>.
              check_configuration-threshold = <threshold>.
              check_configuration-prio = <prio>.
              check_configuration-apply_on_productive_code = <apply_on_productive_code>.
              check_configuration-apply_on_testcode = <apply_on_testcode>.

              result = VALUE #( BASE result
                              ( CORRESPONDING #( check_configuration ) ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_sy_create_data_error
            cx_sy_create_object_error
            ycx_entry_not_found.
        RETURN.
    ENDTRY.
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
      SELECT SINGLE obj_name FROM tadir INTO obj_name
        WHERE pgmid = 'R3TR' AND
              object = 'TABL' AND
              obj_name = profile_db.
      IF sy-subrc = 0.
        SELECT profile FROM (profile_db) INTO TABLE result.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA profile_manager TYPE REF TO object.
    DATA profiles_ref TYPE REF TO data.
    FIELD-SYMBOLS <profiles> TYPE ANY TABLE.
    DATA profile_ref TYPE REF TO data.
    FIELD-SYMBOLS <profile> TYPE any.
    FIELD-SYMBOLS <profile_name> TYPE any.

    TRY.
        CREATE DATA profiles_ref TYPE STANDARD TABLE OF (`YTAB_PROFILES`) WITH DEFAULT KEY.
        ASSIGN profiles_ref->* TO <profiles>.

        CREATE DATA profile_ref TYPE (`YTAB_PROFILES`).
        ASSIGN profile_ref->* TO <profile>.

        CREATE OBJECT profile_manager TYPE (`Y_PROFILE_MANAGER`).
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        RETURN.
    ENDTRY.

    DATA(ptab) = VALUE abap_parmbind_tab( ( name  = 'USERNAME'
                                            kind  = cl_abap_objectdescr=>exporting
                                            value = REF #( username ) )
                                          ( name  = 'RESULT'
                                            kind  = cl_abap_objectdescr=>returning
                                            value = REF #( <profiles> ) ) ).
    TRY.
        CALL METHOD profile_manager->(`Y_IF_PROFILE_MANAGER~SELECT_PROFILES`)
          PARAMETER-TABLE ptab.
      CATCH ycx_entry_not_found.
        RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDTRY.

    LOOP AT <profiles> ASSIGNING <profile>.
      ASSIGN COMPONENT `PROFILE` OF STRUCTURE <profile> TO <profile_name>.
      APPEND <profile_name> TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD calculate_obj_creation_date.
    IF object_creation_date IS NOT BOUND.
      object_creation_date = NEW y_object_creation_date( ).
    ENDIF.
    CASE object_type.
      WHEN 'INTF'.
        result = object_creation_date->get_interface_create_date( object_name ).
      WHEN 'CLAS'.
        result = object_creation_date->get_class_create_date( object_name ).
      WHEN 'FUGR'.
        result = object_creation_date->get_function_group_create_date( object_name ).
      WHEN 'PROG'.
        result = object_creation_date->get_program_create_date( object_name ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD read_check_customizing.
    TRY.
        DATA(profiles) = determine_profiles( username ).
      CATCH ycx_no_check_customizing.
        RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDTRY.

    DATA(obj_creation_date) = y_if_clean_code_manager~calculate_obj_creation_date( object_name = object_name
                                                                                   object_type = object_type ).
    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      TRY.
          DATA(check_configurations) = determine_checks( profile = <profile>
                                                         checkid = checkid
                                                         obj_creation_date = obj_creation_date ).
        CATCH ycx_no_check_customizing.
          CONTINUE.
      ENDTRY.
      APPEND LINES OF check_configurations TO result.
    ENDLOOP.

    IF lines( result ) = 0.
      RAISE EXCEPTION TYPE ycx_no_check_customizing.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
