CLASS y_code_pal_service DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES: BEGIN OF versions,
             code_pal_for_abap TYPE string,
             sap_basis         TYPE string,
             abapGit           TYPE string,
           END OF versions.

  PROTECTED SECTION.
    METHODS raise_bad_request.
    METHODS raise_method_not_allowed.
    METHODS raise_internal_server_error.
    METHODS raise_forbidden.

    METHODS execute_import_profile.
    METHODS execute_get_versions.
    METHODS execute_regression_test.
    METHODS execute_unit_test.
    METHODS execute_ping.

    METHODS get_basis_version RETURNING VALUE(result) TYPE string.

    METHODS convert_json_to_profile IMPORTING json          TYPE string
                                    RETURNING VALUE(result) TYPE y_if_profile_manager=>file
                                    RAISING   cx_abap_invalid_value.

    METHODS list_non_executed_checks IMPORTING checks        TYPE y_if_profile_manager=>check_descriptions
                                               findings      TYPE scit_rest
                                     RETURNING VALUE(result) TYPE y_if_profile_manager=>check_descriptions.

    METHODS write_non_executed_checks IMPORTING non_executed_checks TYPE y_if_profile_manager=>check_descriptions
                                      RETURNING value(result)       TYPE string.

  PRIVATE SECTION.
    DATA request TYPE REF TO if_http_request.
    DATA response TYPE REF TO if_http_response.

ENDCLASS.


CLASS y_code_pal_service IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    request = server->request.
    response = server->response.

    CASE request->get_header_field( 'action' ).
      WHEN 'import_profile'.
        execute_import_profile( ).
      WHEN 'get_versions'.
        execute_get_versions( ).
      WHEN 'regression_test'.
        execute_regression_test( ).
      WHEN 'unit_test'.
        execute_unit_test( ).
      WHEN 'ping'.
        execute_ping( ).
      WHEN OTHERS.
        raise_bad_request( ).
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD execute_import_profile.
    IF request->get_method( ) <> 'POST'.
      raise_method_not_allowed( ).
      RETURN.
    ENDIF.

    IF request->get_content_type( ) <> 'application/json'.
      raise_bad_request( ).
      RETURN.
    ENDIF.

    DATA(json) = request->get_cdata( ).

    TRY.
        DATA(profile) = convert_json_to_profile( json ).
      CATCH cx_abap_invalid_value INTO DATA(profile_exception).
        raise_bad_request( ).
        response->set_cdata( |{ profile_exception->get_text( ) }: { profile_exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    DATA(profile_manager) = y_profile_manager=>create( ).

    TRY.
        profile_manager->import_profile( profile ).
      CATCH ycx_failed_to_add_a_line ycx_time_overlap INTO DATA(manager_exception).
        raise_internal_server_error( ).
        response->set_cdata( |{ manager_exception->get_text( ) }: { manager_exception->get_longtext( ) }| ).
        RETURN.
      CATCH ycx_no_delegation_rights.
        raise_forbidden( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD execute_get_versions.
    IF request->get_method( ) <> 'GET'.
      raise_method_not_allowed( ).
      RETURN.
    ENDIF.

    DATA(structure) = VALUE versions( code_pal_for_abap = y_code_pal_version=>abap
                                      sap_basis = get_basis_version( )
                                      abapGit = zif_abapgit_version=>gc_abap_version ).

    DATA(json) = /ui2/cl_json=>serialize( structure ).

    response->set_content_type( 'application/json' ).
    response->set_cdata( json ).
  ENDMETHOD.

  METHOD raise_bad_request.
    response->set_status( code   = '400'
                          reason = 'Bad Request' ).
  ENDMETHOD.

  METHOD raise_method_not_allowed.
    response->set_status( code   = '405'
                          reason = 'Method Not Allowed' ).
  ENDMETHOD.

  METHOD raise_forbidden.
    response->set_status( code   = '403'
                          reason = 'Forbidden' ).
  ENDMETHOD.

  METHOD raise_internal_server_error.
    response->set_status( code   = '500'
                          reason = 'Internal Server Error' ).
  ENDMETHOD.

  METHOD convert_json_to_profile.
    /ui2/cl_json=>deserialize( EXPORTING json = json
                               CHANGING data = result ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ENDIF.
  ENDMETHOD.

  METHOD get_basis_version.
    SELECT SINGLE * FROM cvers INTO @DATA(cver) WHERE component = 'SAP_BASIS'.
    result = |{ cver-release }-{ cver-extrelease }|.
  ENDMETHOD.

  METHOD execute_regression_test.
    IF request->get_method( ) <> 'GET'.
      raise_method_not_allowed( ).
      RETURN.
    ENDIF.

    TRY.
        y_profile_manager=>create( )->delete_profiles( ).
      CATCH ycx_failed_to_remove_a_line.
        raise_internal_server_error( ).
        response->set_cdata( 'Profiles were not disabled' ).
        RETURN.
    ENDTRY.

    DATA(atc) = NEW cl_satc_api_factory( ).

    TRY.
        DATA(object_set) = cl_satc_object_set_factory=>create_for_object_keys( VALUE #( ( obj_type = 'CLAS' obj_name = 'Y_DEMO_FAILURES' )
                                                                                        ( obj_type = 'PROG' obj_name = 'Y_DEMO_FAILURES' ) ) ).
      CATCH cx_satc_empty_object_set.
        raise_internal_server_error( ).
        response->set_cdata( 'Object set contains no checkable objects' ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(variant) = atc->get_repository( )->load_ci_check_variant( i_name = 'Y_CODE_PAL' ).
      CATCH cx_satc_not_found.
        raise_internal_server_error( ).
        response->set_cdata( 'Specified Code Inspector variant was not found' ).
        RETURN.
    ENDTRY.

    DATA(configuration) = atc->create_run_config_with_chk_var( i_object_set    = object_set
                                                               i_check_variant = variant
                                                               i_description   = 'Y_CODE_PAL_SERVICE' ).

    DATA(controller) = atc->create_run_controller( configuration ).

    TRY.
        controller->run( IMPORTING e_result_access = DATA(result_access) ).
      CATCH cx_satc_failure.
        raise_internal_server_error( ).
        response->set_cdata( 'ATC check run failed (no authorization, etc.)' ).
        RETURN.
    ENDTRY.

    TRY.
        result_access->get_findings( IMPORTING e_findings = DATA(findings) ).
      CATCH cx_satc_failure.
        raise_internal_server_error( ).
        response->set_cdata( 'Result access failed (no authorization, etc.)' ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(checks) = y_profile_manager=>create( )->select_existing_checks( ).
      CATCH ycx_entry_not_found INTO DATA(check_exception).
        raise_internal_server_error( ).
        response->set_cdata( |{ check_exception->get_text( ) }: { check_exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    DATA(non_executed_checks) = list_non_executed_checks( checks = checks
                                                          findings = findings ).

    IF non_executed_checks IS NOT INITIAL.
      response->set_cdata( write_non_executed_checks( non_executed_checks ) ).
    ELSE.
      response->set_cdata( 'OK' ).
    ENDIF.

  ENDMETHOD.

  METHOD list_non_executed_checks.
    CONSTANTS maintain_attributes TYPE sci_errc VALUE 106.

    result = checks.

    LOOP AT findings ASSIGNING FIELD-SYMBOL(<finding>) WHERE code <> maintain_attributes.
      DELETE result WHERE checkid = <finding>-test.
    ENDLOOP.

    " not supported
    DELETE result WHERE checkid = 'Y_CHECK_PROFILE_MESSAGE'
                     OR checkid = 'Y_CHECK_TEST_SEAM_USAGE'
                     OR checkid = 'Y_CHECK_FUNCTION'.
  ENDMETHOD.

  METHOD write_non_executed_checks.
    LOOP AT non_executed_checks ASSIGNING FIELD-SYMBOL(<non_executed_check>).
      result = COND #( WHEN result IS NOT INITIAL THEN |{ result }<br>| ).
      result = |{ result }```{ sy-tabix }. { <non_executed_check>-checkid }: { <non_executed_check>-description }```|.
    ENDLOOP.
    IF result IS INITIAL.
      RETURN.
    ENDIF.
    result = |Non-Executed Checks:<br>{ result }|.
  ENDMETHOD.

  METHOD execute_unit_test.
    raise_internal_server_error( ).
    RETURN.
  ENDMETHOD.

  METHOD execute_ping.
    RETURN.
  ENDMETHOD.

ENDCLASS.
