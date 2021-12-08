CLASS y_pal_boolean_input_param DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_setter_method IMPORTING statement TYPE sstmnt
                             RETURNING VALUE(result) TYPE abap_bool.

    METHODS has_boolean_input_param IMPORTING statement TYPE sstmnt
                                    RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS y_pal_boolean_input_param IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC BOOL_PARAM' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }boolean-input-parameter.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-class_definition ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Split method instead of Boolean input parameter!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK keyword( ) = if_kaizen_keywords_c=>gc_methods.
    CHECK is_setter_method( statement ) = abap_false.
    CHECK has_boolean_input_param( statement ).

    DATA(configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = configuration ).
  ENDMETHOD.


  METHOD is_setter_method.
    DATA(method_name) = next1( CONV #( if_kaizen_keywords_c=>gc_methods ) ).
    result = xsdbool( method_name CS 'SET_' ).
  ENDMETHOD.


  METHOD has_boolean_input_param.
    DATA(skip) = abap_true.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to.

      IF <token>-str = 'IMPORTING'.
        skip = abap_false.
      ELSEIF <token>-str = 'EXPORTING'
      OR <token>-str = 'RETURNING'
      OR <token>-str = 'CHANGING'.
        skip = abap_true.
      ENDIF.

      IF skip = abap_true.
        CONTINUE.
      ENDIF.

      IF <token>-str = 'ABAP_BOOL'.
        result = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
