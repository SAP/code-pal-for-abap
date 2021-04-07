CLASS y_check_self_reference DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS method_call TYPE string VALUE 'A' ##NO_TEXT.

    METHODS has_self_reference IMPORTING statement     TYPE sstmnt
                               RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS y_check_self_reference IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC SELF_REF' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }self-reference.md|.

    set_check_message( 'Omit the self-reference me when calling an instance method!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK statement-type = method_call.
    CHECK has_self_reference( statement ) = abap_true.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.


  METHOD has_self_reference.
    LOOP AT ref_scan_manager->tokens TRANSPORTING NO FIELDS
    FROM statement-from TO statement-to
    WHERE str CP 'ME->*'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
