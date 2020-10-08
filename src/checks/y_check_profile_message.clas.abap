CLASS y_check_profile_message DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS inform REDEFINITION.
  PRIVATE SECTION.
    CLASS-DATA ran TYPE abap_bool.
ENDCLASS.

CLASS y_check_profile_message IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    has_documentation = abap_false.

    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_true.
    settings-prio = c_note.

    set_check_message( 'code pal for ABAP Profile is being used.' ).
  ENDMETHOD.


  METHOD execute_check.

    CHECK ran = abap_false.
    CHECK has_attributes = abap_false.

    DATA(check_configuration) = detect_check_configuration( VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = 1
                 statement_index     = 1
                 statement_from      = 1
                 error_priority      = check_configuration-prio ).

    ran = abap_true.

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD inform.
    super->inform( p_sub_obj_type    = ''
                   p_sub_obj_name    = ''
                   p_position        = ''
                   p_line            = ''
                   p_column          = ''
                   p_errcnt          = p_errcnt
                   p_kind            = p_kind
                   p_test            = p_test
                   p_code            = p_code
                   p_suppress        = p_suppress
                   p_param_1         = p_param_1
                   p_param_2         = p_param_2
                   p_param_3         = p_param_3
                   p_param_4         = p_param_4
                   p_inclspec        = p_inclspec
                   p_detail          = p_detail
                   p_checksum_1      = p_checksum_1
                   p_comments        = p_comments
                   p_finding_origins = p_finding_origins ).
  ENDMETHOD.


ENDCLASS.
