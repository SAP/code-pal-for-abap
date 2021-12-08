CLASS y_pal_is_interface_in_class DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_statements REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    DATA public_method_counter TYPE i VALUE 0.

    METHODS get_first_token IMPORTING statement TYPE sstmnt
                            RETURNING value(result) TYPE string.

    METHODS get_second_token IMPORTING statement TYPE sstmnt
                            RETURNING value(result) TYPE string.

    METHODS get_third_token IMPORTING statement TYPE sstmnt
                            RETURNING value(result) TYPE string.

    METHODS get_last_token IMPORTING statement TYPE sstmnt
                           RETURNING value(result) TYPE string.

    METHODS check_result IMPORTING structure TYPE sstruc.

ENDCLASS.



CLASS Y_PAL_IS_INTERFACE_IN_CLASS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC INTF_IN_CLASS' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 1.
    settings-apply_on_test_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }interface-in-class.md|.

    relevant_statement_types = VALUE #( ( scan_struc_stmnt_type-public_section ) ).
    relevant_structure_types = VALUE #(  ).

    set_check_message( '&1 public methods without interface!' ).
  ENDMETHOD.


  METHOD inspect_statements.
    public_method_counter = 0.

    super->inspect_statements( structure ).

    check_result( structure ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_first_token( statement ) = 'METHODS'
      AND get_second_token( statement ) <> 'CONSTRUCTOR'
      AND get_third_token( statement ) <> 'ABSTRACT'
      AND get_last_token( statement ) <> 'REDEFINITION'.

    CHECK get_first_token( statement ) = 'METHODS'
      AND get_second_token( statement ) <> 'FOR'
      AND get_third_token( statement ) <> 'TESTING'.

    public_method_counter = public_method_counter + 1.
  ENDMETHOD.


  METHOD get_last_token.
    result = get_token_abs( statement-to ).
  ENDMETHOD.


  METHOD get_third_token.
    result = get_token_abs( statement-from + 2 ).
  ENDMETHOD.


  METHOD get_second_token.
    result = get_token_abs( statement-from + 1 ).
  ENDMETHOD.


  METHOD get_first_token.
    result = get_token_abs( statement-from ).
  ENDMETHOD.


  METHOD check_result.
    DATA(statement_for_message) = ref_scan->statements[ structure-stmnt_from ].

    DATA(check_configuration) = detect_check_configuration( error_count = public_method_counter
                                                            statement = statement_for_message ).

    raise_error( statement_level = statement_for_message-level
                 statement_index = structure-stmnt_from
                 statement_from = statement_for_message-from
                 check_configuration = check_configuration
                 parameter_01 = |{ public_method_counter }| ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
