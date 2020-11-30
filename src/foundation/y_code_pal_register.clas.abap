CLASS y_code_pal_register DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS activate_all_checks RAISING cx_failed.
    CLASS-METHODS deactivate_all_checks RAISING cx_failed.
    CLASS-DATA successes TYPE i.
    CLASS-DATA failures TYPE i.

  PROTECTED SECTION.
    CLASS-DATA name_tab TYPE STANDARD TABLE OF scitests-name.

    CLASS-METHODS select_object_list RETURNING VALUE(result) LIKE name_tab
                                     RAISING   cx_failed.

    CLASS-METHODS activate_check IMPORTING name TYPE sci_chk
                                 RAISING   cx_failed
                                           cx_sy_create_object_error.

    CLASS-METHODS deactivate_check IMPORTING name TYPE sci_chk
                                   RAISING   cx_failed
                                             cx_sy_create_object_error.

    CLASS-METHODS is_check_compatible  IMPORTING name TYPE sci_chk
                                       RAISING   cx_sy_create_object_error.

    CLASS-METHODS get_category_name RETURNING VALUE(result) TYPE sci_chk.

ENDCLASS.



CLASS y_code_pal_register IMPLEMENTATION.

  METHOD activate_all_checks.
    successes = failures = 0.
    LOOP AT select_object_list( ) ASSIGNING FIELD-SYMBOL(<name>).
      TRY.
          activate_check( <name> ).
          successes = successes + 1.
        CATCH cx_failed.
          failures = failures + 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD deactivate_all_checks.
    successes = failures = 0.
    LOOP AT select_object_list( ) ASSIGNING FIELD-SYMBOL(<name>).
      TRY.
          deactivate_check( <name> ).
          successes = successes + 1.
        CATCH cx_failed.
          failures = failures + 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_category_name.
    result = cl_abap_objectdescr=>describe_by_object_ref( NEW y_category_code_pal( ) )->get_relative_name( ).
  ENDMETHOD.

  METHOD is_check_compatible.
    CHECK name <> get_category_name( ).
    DATA code_pal_check TYPE REF TO y_check_base.
    CREATE OBJECT code_pal_check TYPE (name).
  ENDMETHOD.

  METHOD select_object_list.
    SELECT SINGLE devclass FROM tadir
    WHERE obj_name = 'Y_CHECK_BASE'
    AND object = 'CLAS'
    AND delflag = @abap_false
    INTO @DATA(packagename).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    REPLACE 'FOUNDATION' IN packagename WITH 'CHECKS'.

    SELECT obj_name FROM tadir
    WHERE devclass = @packagename
    AND object = 'CLAS'
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

    INSERT scitests FROM name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.

  METHOD deactivate_check.
    is_check_compatible( name ).

    DELETE FROM scitests WHERE name = name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
