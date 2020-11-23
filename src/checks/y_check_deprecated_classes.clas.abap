CLASS y_check_deprecated_classes DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF deprecated_classes_type,
             original    TYPE tadir-obj_name,
             replacement TYPE tadir-obj_name,
           END OF deprecated_classes_type.

    DATA deprecated_classes TYPE TABLE OF deprecated_classes_type.

    METHODS get_refereced_type IMPORTING position TYPE sy-tabix
                               RETURNING VALUE(result) TYPE tadir-obj_name.

ENDCLASS.



CLASS y_check_deprecated_classes IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DEPRECATED_CLAS' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }deprecated-classes.md|.

    set_check_message( '&1 was deprecated and replaced by &2!' ).

    deprecated_classes = VALUE #( ( original = 'CL_AUNIT_ASSERT' replacement = 'CL_ABAP_UNIT_ASSERT' )
                                  ( original = 'IF_AUNIT_CONSTANTS' replacement = 'IF_ABAP_UNIT_CONSTANT' ) ).
  ENDMETHOD.


  METHOD execute_check.
    LOOP AT ref_scan_manager->get_structures( ) ASSIGNING FIELD-SYMBOL(<structure>)
       WHERE stmnt_type EQ scan_struc_stmnt_type-form
          OR stmnt_type EQ scan_struc_stmnt_type-method
          OR stmnt_type EQ scan_struc_stmnt_type-function
          OR stmnt_type EQ scan_struc_stmnt_type-module
          OR stmnt_type EQ scan_struc_stmnt_type-class_definition
          OR stmnt_type EQ scan_struc_stmnt_type-interface
          OR type EQ scan_struc_type-event.

      is_testcode = test_code_detector->is_testcode( <structure> ).

      TRY.
          DATA(check_configuration) = check_configurations[ apply_on_testcode = abap_true ].
        CATCH cx_sy_itab_line_not_found.
          IF is_testcode EQ abap_true.
            CONTINUE.
          ENDIF.
      ENDTRY.

      DATA(index) = <structure>-stmnt_from.

      LOOP AT ref_scan_manager->get_statements( ) ASSIGNING FIELD-SYMBOL(<statement>)
        FROM <structure>-stmnt_from TO <structure>-stmnt_to.

        inspect_tokens( index = index
                        structure = <structure>
                        statement = <statement> ).
        index = index + 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD inspect_tokens.
    CONSTANTS skip_ref_to TYPE i VALUE 2.

    LOOP AT ref_scan_manager->get_tokens( ) TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str = 'TYPE'.

      DATA(position) = sy-tabix.

      DATA(referenced_type) = get_refereced_type( position ).

      IF referenced_type = 'REF'.
        referenced_type = get_refereced_type( position + skip_ref_to ).
      ENDIF.

      TRY.
          DATA(deprecated) = deprecated_classes[ original = referenced_type ].
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(check_configuration) = detect_check_configuration( statement ).

      IF check_configuration IS INITIAL.
        RETURN.
      ENDIF.

      raise_error( statement_level     = statement-level
                   statement_index     = index
                   statement_from      = statement-from
                   error_priority      = check_configuration-prio
                   parameter_01        = deprecated-original
                   parameter_02        = deprecated-replacement ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_refereced_type.
    DATA(tokens) = ref_scan_manager->get_tokens( ).
    TRY.
        result = tokens[ position + 1 ]-str.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
