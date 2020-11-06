CLASS y_check_self_reference DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS method_call TYPE string VALUE 'A' ##NO_TEXT.
    CONSTANTS compute TYPE string VALUE 'C' ##NO_TEXT.
    METHODS has_method_self_referenced IMPORTING statement     TYPE sstmnt
                                       RETURNING VALUE(result) TYPE abap_bool.
    METHODS has_compute_sefl_referenced IMPORTING statement     TYPE sstmnt
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

    CHECK has_method_self_referenced( statement ) = abap_true
    OR has_compute_sefl_referenced( statement ) = abap_true.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.

  METHOD has_method_self_referenced.
    CHECK statement-type = method_call.
    LOOP AT ref_scan_manager->get_tokens( ) ASSIGNING FIELD-SYMBOL(<token>)
    FROM statement-from TO statement-to
    WHERE str CP 'me->*'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD has_compute_sefl_referenced.
    CHECK statement-type = compute.


  ENDMETHOD.

ENDCLASS.
