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
    METHODS raise_internal_code_pal_error.
    METHODS raise_forbidden.

    METHODS execute_import_profile.
    METHODS execute_get_versions.
    METHODS execute_regression_test.
    METHODS execute_unit_test.
    METHODS execute_upgrade.

    METHODS get_basis_version RETURNING VALUE(result) TYPE string.

    METHODS convert_json_to_profile IMPORTING json          TYPE string
                                    RETURNING VALUE(result) TYPE y_if_profile_manager=>file
                                    RAISING   cx_abap_invalid_value.

    METHODS list_non_executed_checks IMPORTING checks        TYPE y_if_profile_manager=>check_descriptions
                                               findings      TYPE scit_rest
                                     RETURNING VALUE(result) TYPE y_if_profile_manager=>check_descriptions.

    METHODS write_non_executed_checks IMPORTING non_executed_checks TYPE y_if_profile_manager=>check_descriptions
                                      RETURNING VALUE(result)       TYPE string.

    METHODS write_abapgit_messages IMPORTING messages      TYPE zif_abapgit_log=>ty_log_outs
                                   RETURNING value(result) TYPE string.

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
      WHEN 'upgrade'.
        execute_upgrade( ).
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
      CATCH cx_abap_invalid_value.
        raise_bad_request( ).
        RETURN.
    ENDTRY.

    DATA(profile_manager) = y_profile_manager=>create( ).

    TRY.
        profile_manager->import_profile( profile ).
      CATCH ycx_failed_to_add_a_line
            ycx_time_overlap.
        raise_internal_code_pal_error( ).
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

  METHOD raise_internal_code_pal_error.
    response->set_status( code   = '500'
                          reason = 'Internal Code Pal Error' ).
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

    DATA(atc) = NEW cl_satc_api_factory( ).

    TRY.
        DATA(object_set) = cl_satc_object_set_factory=>create_for_object_keys( VALUE #( ( obj_type = 'CLAS' obj_name = 'Y_DEMO_FAILURES' )
                                                                                        ( obj_type = 'PROG' obj_name = 'Y_DEMO_FAILURES' ) ) ).
      CATCH cx_satc_empty_object_set.
        " Object set contains no checkable objects
        raise_internal_code_pal_error( ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(variant) = atc->get_repository( )->load_ci_check_variant( i_name = 'Y_CODE_PAL' ).
      CATCH cx_satc_not_found.
        " Specified Code Inspector variant was not found
        raise_internal_code_pal_error( ).
        RETURN.
    ENDTRY.

    DATA(configuration) = atc->create_run_config_with_chk_var( i_object_set    = object_set
                                                               i_check_variant = variant
                                                               i_description   = 'Y_CODE_PAL_SERVICE' ).

    DATA(controller) = atc->create_run_controller( configuration ).

    TRY.
        controller->run( IMPORTING e_result_access = DATA(result_access) ).
      CATCH cx_satc_failure.
        " ATC check run failed (no authorization, etc.)
        raise_internal_code_pal_error( ).
        RETURN.
    ENDTRY.

    TRY.
        result_access->get_findings( IMPORTING e_findings           = DATA(findings)
                                               e_findings_extension = DATA(findings_extension) ).
      CATCH cx_satc_failure.
        " Result access failed (no authorization, etc.)
        raise_internal_code_pal_error( ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(checks) = y_profile_manager=>create( )->select_existing_checks( ).
      CATCH ycx_entry_not_found.
        raise_internal_code_pal_error( ).
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
    result = checks.
    LOOP AT findings ASSIGNING FIELD-SYMBOL(<finding>).
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
    result = |Failed:<br>{ result }|.
  ENDMETHOD.

  METHOD execute_unit_test.
    raise_internal_code_pal_error( ).
    RETURN.
  ENDMETHOD.

  METHOD execute_upgrade.
    DATA exception TYPE REF TO zcx_abapgit_exception.

    IF request->get_method( ) <> 'GET'.
      raise_method_not_allowed( ).
      RETURN.
    ENDIF.

    TRY.
        LOOP AT zcl_abapgit_repo_srv=>get_instance( )->list( ) ASSIGNING FIELD-SYMBOL(<list>).
          IF <list>->get_package( ) = '$CODE_PAL_FOR_ABAP'.
            DATA(repo) = CAST zcl_abapgit_repo_online( zcl_abapgit_repo_srv=>get_instance( )->get( <list>->get_key( ) ) ).
            EXIT.
          ENDIF.
        ENDLOOP.
      CATCH zcx_abapgit_exception INTO exception.
        raise_internal_code_pal_error( ).
        response->set_cdata( |{ exception->get_text( ) }<br>{ exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    TRY.
        repo->select_branch( 'refs/heads/master' ).
      CATCH zcx_abapgit_exception.
        raise_internal_code_pal_error( ).
        response->set_cdata( |{ exception->get_text( ) }<br>{ exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    TRY.
        DATA(checks) = repo->deserialize_checks( ).
      CATCH zcx_abapgit_exception.
        raise_internal_code_pal_error( ).
        response->set_cdata( |{ exception->get_text( ) }<br>{ exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    LOOP AT checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = 'Y'.
    ENDLOOP.

    LOOP AT checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = 'Y'.
    ENDLOOP.

    IF checks-requirements-met = 'N'.
      checks-requirements-decision = 'Y'.
    ENDIF.

    DATA(log) = repo->create_new_log( 'code pal service' ).

    TRY.
        repo->deserialize( is_checks = checks
                           ii_log = log ).
      CATCH zcx_abapgit_exception.
        raise_internal_code_pal_error( ).
        response->set_cdata( |{ exception->get_text( ) }<br>{ exception->get_longtext( ) }| ).
        RETURN.
    ENDTRY.

    IF log->get_status( ) <> 'S'.
      raise_internal_code_pal_error( ).
      response->set_cdata( write_abapgit_messages( log->get_messages( ) ) ).
      RETURN.
    ENDIF.

    SUBMIT y_ci_check_registration AND RETURN.

    response->set_cdata( 'OK' ).

  ENDMETHOD.

  METHOD write_abapgit_messages.
    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>)
    WHERE type = 'W' OR type = 'E'.
      result = COND #( WHEN result IS NOT INITIAL THEN |{ result }<br>| ).
      result = |{ result }```{ <message>-obj_type } { <message>-obj_name }: { <message>-text }```|.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
