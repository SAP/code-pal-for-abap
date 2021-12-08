CLASS y_pal_deprecated_classes DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF deprecated_classes_type,
             original    TYPE tadir-obj_name,
             replacement TYPE tadir-obj_name,
           END OF deprecated_classes_type.

    DATA deprecated_classes TYPE TABLE OF deprecated_classes_type.

    METHODS get_refereced_type IMPORTING position TYPE sy-tabix
                               RETURNING VALUE(result) TYPE tadir-obj_name.

ENDCLASS.



CLASS y_pal_deprecated_classes IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC DEPRECATED_CLAS' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }deprecated-classes.md|.

    set_check_message( '&1 was deprecated and replaced by &2!' ).

    relevant_statement_types = VALUE #( BASE relevant_statement_types
                                      ( scan_struc_stmnt_type-class_definition )
                                      ( scan_struc_stmnt_type-interface ) ).

    deprecated_classes = VALUE #( ( original = 'CL_AUNIT_ASSERT' replacement = 'CL_ABAP_UNIT_ASSERT' )
                                  ( original = 'IF_AUNIT_CONSTANTS' replacement = 'IF_ABAP_UNIT_CONSTANT' ) ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CONSTANTS skip_ref_to TYPE i VALUE 2.

    LOOP AT ref_scan->tokens TRANSPORTING NO FIELDS
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

      raise_error( statement_level = statement-level
                   statement_index = index
                   statement_from = statement-from
                   check_configuration = check_configuration
                   parameter_01 = deprecated-original
                   parameter_02 = deprecated-replacement ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_refereced_type.
    TRY.
        result = ref_scan->tokens[ position + 1 ]-str.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
