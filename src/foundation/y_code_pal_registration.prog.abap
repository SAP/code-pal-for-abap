REPORT y_code_pal_registration.

CONSTANTS: BEGIN OF reference,
             check_base           TYPE sci_chk VALUE 'Y_CODE_PAL_BASE',
             class                TYPE tadir-object VALUE 'CLAS',
             foundation           TYPE string VALUE 'FOUNDATION',
             checks               TYPE string VALUE 'CHECKS',
             modifiable           TYPE e070-trstatus VALUE 'D',
             modifiable_protected TYPE e070-trstatus VALUE 'L',
           END OF reference.

CLASS lcl_check_registration DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA name_tab TYPE STANDARD TABLE OF scitests-name READ-ONLY.

    CLASS-METHODS select_object_list RETURNING VALUE(result) LIKE name_tab
                                     RAISING   cx_failed.

    CLASS-METHODS activate_check IMPORTING name TYPE sci_chk
                                 RAISING   cx_failed
                                           cx_sy_create_object_error.

    CLASS-METHODS deactivate_check IMPORTING name TYPE sci_chk
                                   RAISING   cx_failed
                                             cx_sy_create_object_error.

    CLASS-METHODS is_check_compatible IMPORTING name TYPE sci_chk
                                      RAISING   cx_sy_create_object_error.

    CLASS-METHODS get_category_name RETURNING VALUE(result) TYPE sci_chk.

ENDCLASS.



CLASS lcl_check_registration IMPLEMENTATION.

  METHOD get_category_name.
    result = cl_abap_objectdescr=>describe_by_object_ref( NEW y_code_pal_category( ) )->get_relative_name( ).
  ENDMETHOD.


  METHOD is_check_compatible.
    DATA code_pal_check TYPE REF TO y_code_pal_base.
    IF name <> get_category_name( ).
      CREATE OBJECT code_pal_check TYPE (name).
    ENDIF.
  ENDMETHOD.


  METHOD select_object_list.
    SELECT SINGLE devclass FROM tadir
        WHERE obj_name = @reference-check_base
        AND object = @reference-class
        AND delflag = @abap_false
        INTO @DATA(packagename).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    REPLACE reference-foundation IN packagename WITH reference-checks.

    SELECT obj_name FROM tadir
      WHERE devclass = @packagename
      AND object = @reference-class
      AND delflag = @abap_false
      INTO TABLE @result.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    APPEND get_category_name( ) TO result.
    SORT result ASCENDING AS TEXT.
  ENDMETHOD.


  METHOD activate_check.
    is_check_compatible( name ).

    INSERT scitests FROM @name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.


  METHOD deactivate_check.
    is_check_compatible( name ).

    DELETE FROM scitests WHERE name = @name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_util DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS activate_all_checks.
    CLASS-METHODS deactivate_all_checks.
    CLASS-METHODS migrate_v1_to_v2 RAISING cx_failed.

  PRIVATE SECTION.
    CLASS-METHODS get_obj_list RETURNING VALUE(result) LIKE lcl_check_registration=>name_tab.

ENDCLASS.



CLASS lcl_util IMPLEMENTATION.

  METHOD get_obj_list.
    TRY.
        result = lcl_check_registration=>select_object_list( ).
      CATCH cx_failed.
        MESSAGE TEXT-001 TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD activate_all_checks.
    DATA(count_successes) = 0.
    DATA(count_errors) = 0.
    LOOP AT get_obj_list( ) ASSIGNING FIELD-SYMBOL(<name>).
      TRY.
          lcl_check_registration=>activate_check( <name> ).
          count_successes = count_successes + 1.
        CATCH cx_failed
              cx_sy_create_object_error.
          count_errors = count_errors + 1.
      ENDTRY.
    ENDLOOP.
    WRITE / |{ count_successes } { TEXT-002 }|.
    WRITE / |{ count_errors } { TEXT-003 }|.
  ENDMETHOD.


  METHOD deactivate_all_checks.
    DATA(count_successes) = 0.
    DATA(count_faults) = 0.
    LOOP AT get_obj_list( ) ASSIGNING FIELD-SYMBOL(<name>).
      TRY.
          lcl_check_registration=>deactivate_check( <name> ).
          count_successes = count_successes + 1.
        CATCH cx_failed
              cx_sy_create_object_error.
          count_faults = count_faults + 1.
      ENDTRY.
    ENDLOOP.
    WRITE / |{ count_successes } { TEXT-004 }|.
    WRITE / |{ count_faults } { TEXT-005 }|.
  ENDMETHOD.


  METHOD migrate_v1_to_v2.
    SELECT *
    FROM ytab_checks
    INTO TABLE @DATA(entries)
    WHERE checkid LIKE 'Y_CHECK_%'.

    DATA(count) = lines( entries ).

    IF count = 0.
      WRITE / TEXT-008.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    LOOP AT entries ASSIGNING FIELD-SYMBOL(<deprecated_entry>).
      REPLACE 'Y_CHECK_' WITH 'Y_PAL_' INTO <deprecated_entry>-checkid.
      <deprecated_entry>-evaluate_new_child_objects = abap_true.
    ENDLOOP.

    INSERT ytab_checks FROM TABLE @entries.

    IF sy-subrc IS NOT INITIAL.
      WRITE / TEXT-009.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    DELETE FROM ytab_checks WHERE checkid LIKE 'Y_CHECK_%'.

    IF sy-subrc IS NOT INITIAL.
      WRITE TEXT-009.
      RAISE EXCEPTION TYPE cx_failed.
    ELSE.
      WRITE / |{ count } { TEXT-007 }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.
  SELECTION-SCREEN BEGIN OF BLOCK part0.
    SELECTION-SCREEN COMMENT /1(83) comm0 MODIF ID mg1.
    SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME.
      SELECTION-SCREEN COMMENT /1(60) comm1 MODIF ID mg1.
      PARAMETERS p_activa RADIOBUTTON GROUP g1.
      PARAMETERS p_deacti RADIOBUTTON GROUP g1.
      PARAMETERS p_reacti RADIOBUTTON GROUP g1 DEFAULT 'X'.
      PARAMETERS p_migrat RADIOBUTTON GROUP g1.
    SELECTION-SCREEN END OF BLOCK part1.
  SELECTION-SCREEN END OF BLOCK part0.

AT SELECTION-SCREEN OUTPUT.
  comm0 = TEXT-010.
  comm1 = TEXT-011.

START-OF-SELECTION.
  WRITE / TEXT-010.
  WRITE / space.

  TRY.
      IF p_activa = abap_true.
        lcl_util=>activate_all_checks( ).

      ELSEIF p_deacti = abap_true.
        lcl_util=>deactivate_all_checks( ).

      ELSEIF p_reacti = abap_true.
        lcl_util=>deactivate_all_checks( ).
        lcl_util=>activate_all_checks( ).

      ELSEIF p_migrat = abap_true.
        lcl_util=>migrate_v1_to_v2( ).

      ENDIF.

      COMMIT WORK AND WAIT.

    CATCH cx_failed.
      ROLLBACK WORK.

  ENDTRY.
