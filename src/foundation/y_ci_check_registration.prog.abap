REPORT y_ci_check_registration.

DATA: BEGIN OF comments,
        title   TYPE string VALUE 'code pal for ABAP - Check Activation Tool (Local Only)',
        runmode TYPE string VALUE 'Please choose a run mode',
      END OF comments.

DATA: BEGIN OF messages,
        checks_not_found         TYPE string VALUE 'Code Pal Checks not found.',
        successfully_activated   TYPE string VALUE 'Entrys Successfully activated (category + checks)',
        failed_activation        TYPE string VALUE 'Entrys Failed to activate',
        successfully_deactivated TYPE string VALUE 'Entrys Successfully deactivated (category + checks)',
        failed_deactivation      TYPE string VALUE 'Entrys Failed to deactivate',
        done                     TYPE string VALUE 'Done!',
      END OF messages.

CONSTANTS: BEGIN OF reference,
             check_base           TYPE sci_chk VALUE 'Y_CHECK_BASE',
             class                TYPE tadir-object VALUE 'CLAS',
             foundation           TYPE string VALUE 'FOUNDATION',
             checks               TYPE string VALUE 'CHECKS',
             modifiable           TYPE e070-trstatus VALUE 'D',
             modifiable_protected TYPE e070-trstatus VALUE 'L',
             error                TYPE c LENGTH 1 VALUE 'E',
           END OF reference.

CLASS lcl_check_registration DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA name_tab TYPE STANDARD TABLE OF scitests-name READ-ONLY.

    CLASS-METHODS select_object_list
      RETURNING VALUE(result) LIKE name_tab
      RAISING   cx_failed.

    CLASS-METHODS activate_check
      IMPORTING name TYPE sci_chk
      RAISING   cx_failed
                cx_sy_create_object_error.

    CLASS-METHODS deactivate_check
      IMPORTING name TYPE sci_chk
      RAISING   cx_failed
                cx_sy_create_object_error.

    CLASS-METHODS is_check_compatible
      IMPORTING name TYPE sci_chk
      RAISING   cx_sy_create_object_error.

    CLASS-METHODS get_category_name
      RETURNING VALUE(result) TYPE sci_chk.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_check_registration IMPLEMENTATION.
  METHOD get_category_name.
    result = cl_abap_objectdescr=>describe_by_object_ref( NEW y_category_code_pal( ) )->get_relative_name( ).
  ENDMETHOD.

  METHOD is_check_compatible.
    DATA code_pal_check TYPE REF TO y_check_base.
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

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_obj_list
      RETURNING VALUE(result) LIKE lcl_check_registration=>name_tab.

ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD get_obj_list.
    TRY.
        result = lcl_check_registration=>select_object_list( ).
      CATCH cx_failed.
        MESSAGE messages-checks_not_found TYPE reference-error.
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
    WRITE: / |{ count_successes } { messages-successfully_activated }|.
    WRITE: / |{ count_errors } { messages-failed_activation }|.
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
    WRITE: / |{ count_successes } { messages-successfully_deactivated }|.
    WRITE:  / |{ count_faults } { messages-failed_deactivation }|.
  ENDMETHOD.

ENDCLASS.

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
  WRITE: / comments-title.
  WRITE: / space.

  IF p_activa = abap_true.
    lcl_util=>activate_all_checks( ).

  ELSEIF p_deacti = abap_true.
    lcl_util=>deactivate_all_checks( ).

  ELSEIF p_reacti = abap_true.
    lcl_util=>deactivate_all_checks( ).
    lcl_util=>activate_all_checks( ).

  ENDIF.
  COMMIT WORK.

  WRITE: / |{ messages-done }|.
