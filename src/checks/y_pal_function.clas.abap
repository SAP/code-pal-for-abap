CLASS y_pal_function DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS is_normal_mode RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_pal_function IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC CI_FUNCTION' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }function-routine.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-function ) ).
    relevant_structure_types = VALUE #( ).

    set_check_message( 'Function-Module should not be created!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK keyword( ) = if_kaizen_keywords_c=>gc_function.
    CHECK is_normal_mode( ) = abap_true.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD is_normal_mode.
    DATA(function_module) = next1( CONV #( if_kaizen_keywords_c=>gc_function ) ).
    DATA(function_modules) = manager->database_access->get_function_module( CONV #( function_module ) ).
    result = xsdbool( line_exists( function_modules[ fmode = space ] ) ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
