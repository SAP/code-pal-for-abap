CLASS y_check_profile_message DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
    METHODS inform REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA ran TYPE abap_bool.
    METHODS get_profiles RETURNING VALUE(result) TYPE y_if_profile_manager=>profile_assignments.
    METHODS list_profiles IMPORTING profiles TYPE y_if_profile_manager=>profile_assignments
                          RETURNING VALUE(result) TYPE string.
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
    settings-ignore_pseudo_comments = abap_true.

    set_check_message( '&1 Profile(s) in use: &2.' ).
  ENDMETHOD.


  METHOD execute_check.
    CHECK ran = abap_false.
    CHECK has_attributes = abap_false.

    DATA(check_configuration) = detect_check_configuration( VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    DATA(profiles) = get_profiles( ).

    raise_error( statement_level = 1
                 statement_index = 1
                 statement_from = 1
                 error_priority = check_configuration-prio
                 parameter_01 = |{ lines( profiles ) }|
                 parameter_02 = |{ list_profiles( profiles ) }| ).

    ran = abap_true.
  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


  METHOD inform.
    super->inform( p_sub_obj_type = 'TRAN'
                   p_sub_obj_name = 'Y_CODE_PAL_PROFILE'
                   p_position = ''
                   p_line = ''
                   p_column = ''
                   p_errcnt = p_errcnt
                   p_kind = p_kind
                   p_test = p_test
                   p_code = p_code
                   p_suppress = p_suppress
                   p_param_1 = p_param_1
                   p_param_2 = p_param_2
                   p_param_3 = p_param_3
                   p_param_4 = p_param_4
                   p_inclspec = p_inclspec
                   p_detail = p_detail
                   p_checksum_1 = p_checksum_1
                   p_comments = p_comments ).
  ENDMETHOD.


  METHOD get_profiles.
    TRY.
        result = y_profile_manager=>create( )->select_profiles( sy-uname ).
      CATCH ycx_entry_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD list_profiles.
    LOOP AT profiles ASSIGNING FIELD-SYMBOL(<profile>).
      result = COND #( WHEN result IS INITIAL THEN <profile>-profile
                       ELSE |{ result }, { <profile>-profile }| ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
