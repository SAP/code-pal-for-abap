CLASS lcl_file DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS upload RETURNING VALUE(result) TYPE y_if_profile_manager=>file
                         RAISING   ycx_object_not_processed
                                   cx_abap_invalid_value.
    CLASS-METHODS download IMPORTING profile   TYPE y_if_profile_manager=>file-profile
                                     checks    TYPE y_if_profile_manager=>file-checks
                                     delegates TYPE y_if_profile_manager=>file-delegates
                           RAISING   ycx_object_not_processed.
ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD download.
    DATA file_content TYPE TABLE OF string.
    DATA file_path TYPE string.
    DATA file_fullpath TYPE string.

    CONCATENATE 'CODE_PAL_PROFILE-' sy-sysid sy-mandt '-' profile-profile INTO DATA(file_name).

    DATA(structure) = NEW y_if_profile_manager=>file( profile = profile
                                                      checks = checks
                                                      delegates = delegates ).

    APPEND /ui2/cl_json=>serialize( structure ) TO file_content.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension         = 'json'
        default_file_name         = file_name
      CHANGING
        filename                  = file_name
        path                      = file_path
        fullpath                  = file_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_object_not_processed.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = file_fullpath
      CHANGING
        data_tab                  = file_content
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_object_not_processed.
    ENDIF.
  ENDMETHOD.

  METHOD upload.
    DATA files TYPE filetable.
    DATA files_rc TYPE i.
    DATA filename TYPE string.
    DATA data_tab TYPE TABLE OF string.
    DATA json TYPE /ui2/cl_json=>json.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             = 'JSON (*.json)|*.json|'
      CHANGING
        file_table              = files
        rc                      = files_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_object_not_processed.
    ENDIF.

    READ TABLE files INTO filename INDEX 1.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = filename
      CHANGING
        data_tab                = data_tab
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_object_not_processed.
    ENDIF.

    LOOP AT data_tab ASSIGNING FIELD-SYMBOL(<data>).
      CONCATENATE json ' ' <data> INTO json.
    ENDLOOP.

    /ui2/cl_json=>deserialize( EXPORTING json = json
                               CHANGING data = result ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE cx_abap_invalid_value.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_util DEFINITION.                          "#EC NUMBER_METHODS
  PUBLIC SECTION.
    CLASS-METHODS:
      init_profiles IMPORTING sy_repid TYPE sy-repid,
      init_checks IMPORTING sy_repid TYPE sy-repid,
      init_delegates IMPORTING sy_repid TYPE sy-repid.

    CLASS-METHODS:
      refresh_all_trees,
      refresh_profiles,
      refresh_checks,
      refresh_delegates.

    CLASS-METHODS:
      get_selected_profile
        RETURNING VALUE(result) TYPE ytab_profiles
        RAISING   ycx_entry_not_found,
      set_selected_profile
        IMPORTING profile TYPE data,
      get_selected_delegate
        RETURNING VALUE(result) TYPE ytab_delegates
        RAISING   ycx_entry_not_found,
      get_selected_check
        RETURNING VALUE(result) TYPE ytab_checks
        RAISING   ycx_entry_not_found.

    CLASS-METHODS switch_toolbar_activation
      RAISING cx_failed.

    CLASS-METHODS:
      call_f4help
        IMPORTING referenced_field_name TYPE dfies-fieldname
                  window_title          TYPE c
                  value_table           TYPE STANDARD TABLE
        RETURNING VALUE(result) TYPE y_if_profile_manager=>value_help
        RAISING   cx_failed.

    CLASS-METHODS:
      profile_f4help_200,
      profile_f4help_600,
      check_f4help.

    CLASS-METHODS:
      init_check_fields_active,
      get_check
        IMPORTING checkid       TYPE vseoclass-clsname
        RETURNING VALUE(result) TYPE REF TO y_check_base
        RAISING   cx_sy_create_object_error,
      set_threshold_active
        IMPORTING is_active TYPE abap_bool DEFAULT abap_true,
      set_on_prodcode_active
        IMPORTING is_active TYPE abap_bool DEFAULT abap_true,
      set_on_testcode_active
        IMPORTING is_active TYPE abap_bool DEFAULT abap_true,
      set_allow_pcom_active
        IMPORTING is_active TYPE abap_bool DEFAULT abap_true,
      set_dynpro_field_active
        IMPORTING fieldname TYPE string
                  is_active TYPE abap_bool.

    CLASS-METHODS:
      assign_profile,
      unassign_profile,
      copy_profile,
      create_template_profile,
      import_profile,
      export_profile,
      get_initial_check
        RETURNING VALUE(result) TYPE ytab_checks.

    CLASS-METHODS:
      add_delegate
        RAISING cx_failed,
      remove_delegate,
      auto_re_start_delegate,
      check_delegate_rights
        RETURNING VALUE(result) TYPE abap_bool.


    CLASS-METHODS:
      init_add_check,
      init_edit_check
        IMPORTING check TYPE ytab_checks,
      check_customization
        IMPORTING edit_mode TYPE abap_bool DEFAULT abap_false
        RAISING   cx_failed,
      auto_re_start_check
        IMPORTING edit_mode TYPE abap_bool DEFAULT abap_false,
      remove_check
        IMPORTING check TYPE ytab_checks,
      remove_selected_check,
      check_check_rights
        IMPORTING profile       TYPE ycicc_profile
        RETURNING VALUE(result) TYPE abap_bool,
      check_selected_check_rights
        RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS:
      get_cursor_field
        RETURNING VALUE(result) TYPE char20,
      call_check_info,
      add_all_checks.
    CLASS-METHODS add_check
      IMPORTING
        edit_mode TYPE abap_bool DEFAULT abap_false
      RAISING
        cx_failed.
    CLASS-METHODS remove_all_checks.
    CLASS-METHODS add_missing_checks.

    CLASS-METHODS switch_bool
      IMPORTING boolean       TYPE abap_bool
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS ux_usability_switch
      IMPORTING checks        TYPE y_if_profile_manager=>check_assignments
      RETURNING VALUE(result) TYPE y_if_profile_manager=>check_assignments.

  PRIVATE SECTION.
    CLASS-METHODS request_confirmation
      IMPORTING
        text_question TYPE string.

ENDCLASS.


CLASS lcl_profile_events DEFINITION INHERITING FROM y_alv_events.
  PUBLIC SECTION.
    METHODS y_if_alv_events~handle_selection_changed REDEFINITION.
    METHODS y_if_alv_events~handle_function_selected REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_profile_events IMPLEMENTATION.

  METHOD y_if_alv_events~handle_selection_changed.
    lcl_util=>refresh_checks( ).
    lcl_util=>refresh_delegates( ).
    TRY.
        lcl_util=>switch_toolbar_activation( ).
      CATCH cx_failed. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD y_if_alv_events~handle_function_selected.
    CASE fcode.
      WHEN 'BTN_ASSIGN'.
        io_profilename = ''.
        lcl_util=>assign_profile( ).

      WHEN 'BTN_UNASSIGN'.
        lcl_util=>unassign_profile( ).

      WHEN 'BTN_COPY'.
        lcl_util=>copy_profile( ).

      WHEN 'BTN_TEMP'.
        lcl_util=>create_template_profile( ).

      WHEN 'BTN_IMPORT'.
        lcl_util=>import_profile( ).

      WHEN 'BTN_EXPORT'.
        lcl_util=>export_profile( ).

    ENDCASE.
    lcl_util=>refresh_all_trees( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_delegator_events DEFINITION INHERITING FROM y_alv_events.
  PUBLIC SECTION.
    METHODS y_if_alv_events~handle_function_selected REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_delegator_events IMPLEMENTATION.

  METHOD y_if_alv_events~handle_function_selected.
    CHECK lcl_util=>check_delegate_rights( ) = abap_true.

    CASE fcode.
      WHEN 'BTN_ADD'.
        io_delegate_name = ''.
        lcl_util=>auto_re_start_delegate( ).

      WHEN 'BTN_REMOVE'.
        lcl_util=>remove_delegate( ).

    ENDCASE.
    lcl_util=>refresh_delegates( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_check_events DEFINITION INHERITING FROM y_alv_events.
  PUBLIC SECTION.
    METHODS y_if_alv_events~handle_function_selected REDEFINITION.
  PRIVATE SECTION.
    METHODS handle_btn_info.
    METHODS handle_btn_add.
    METHODS handle_btn_edit.
ENDCLASS.

CLASS lcl_check_events IMPLEMENTATION.

  METHOD y_if_alv_events~handle_function_selected.
    IF fcode = 'BTN_INFO'.
      handle_btn_info( ).
      RETURN.
    ENDIF.

    IF lcl_util=>check_selected_check_rights( ) = abap_false.
      RETURN.
    ENDIF.

    CASE fcode.
      WHEN 'BTN_ADD'.
        handle_btn_add( ).

      WHEN 'BTN_EDIT'.
        handle_btn_edit( ).

      WHEN 'BTN_REMOVE'.
        lcl_util=>remove_selected_check( ).

      WHEN 'BTN_ADD_ALL'.
        lcl_util=>add_all_checks( ).

      WHEN 'BTN_REMOVE_ALL'.
        lcl_util=>remove_all_checks( ).

      WHEN 'BTN_MISSING_CK'.
        lcl_util=>add_missing_checks( ).

    ENDCASE.

    lcl_util=>refresh_checks( ).
  ENDMETHOD.

  METHOD handle_btn_edit.
    TRY.
        lcl_util=>init_edit_check( lcl_util=>get_selected_check( ) ).
        lcl_util=>auto_re_start_check( abap_true ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a check!'(015) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_btn_add.
    TRY.
        lcl_util=>init_add_check( ).
        lcl_util=>auto_re_start_check( ).
      CATCH ycx_entry_not_found
            cx_sy_create_object_error.
        MESSAGE 'Please select a check!'(015) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_btn_info.
    TRY.
        io_check_id = lcl_util=>get_selected_check( )-checkid.
        lcl_util=>call_check_info(  ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a check!'(015) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_util IMPLEMENTATION.
  METHOD init_profiles.
    TRY.
        profiles_tree = y_alv_tree_control=>create( alv_header_text = 'Profiles'(002)
                                                    dynpro_nr       = '0100'
                                                    docking_side    = cl_gui_docking_container=>align_at_left
                                                    ratio           = 21
                                                    type_name       = CONV #( profile_manager->types-profiles )
                                                    sort_table      = VALUE lvc_t_sort( ( spos = 1 fieldname = 'USERNAME' up = abap_true )
                                                                                        ( spos = 2 fieldname = 'PROFILE' up = abap_true ) )
                                                    sy_repid        = sy_repid
                                                    events          = NEW lcl_profile_events( )
                                                    event_mode      = y_if_alv_events=>mode_selection_changed ).

        profiles_tree->toolbar_control( )->add_button( fcode     = 'BTN_ASSIGN'
                                                       icon      = '@04@'
                                                       butn_type = cntb_btype_button
                                                       quickinfo = 'Assign'(003) ).

        profiles_tree->toolbar_control( )->add_button( fcode     = 'BTN_UNASSIGN'
                                                       icon      = '@05@'
                                                       butn_type = cntb_btype_button
                                                       quickinfo = 'Unassign'(004) ).

        profiles_tree->toolbar_control( )->add_button( fcode     = 'BTN_COPY'
                                                       icon      = '@14@'
                                                       butn_type = cntb_btype_button
                                                       quickinfo = 'Copy Profile' ).

        profiles_tree->toolbar_control( )->add_button( fcode     = 'BTN_IMPORT'
                                                       icon      = '@48@'
                                                       butn_type = cntb_btype_button
                                                       quickinfo = 'Import Profile' ).

        profiles_tree->toolbar_control( )->add_button( fcode     = 'BTN_EXPORT'
                                                       icon      = '@49@'
                                                       butn_type = cntb_btype_button
                                                       quickinfo = 'Export Profile' ).

        profiles_tree->set_field_header_text( fieldname   = 'USERNAME'
                                              header_text = 'User'(028) ).
        profiles_tree->set_field_header_text( fieldname   = 'PROFILE'
                                              header_text = 'Profile'(001) ).

        profiles_tree->init_display( ).

      CATCH cx_failed
            cx_sy_create_data_error.
        LEAVE TO SCREEN 0.

    ENDTRY.

    refresh_profiles( ).
  ENDMETHOD.

  METHOD init_checks.
    TRY.
        checks_tree = y_alv_tree_control=>create( alv_header_text = 'Checks'(013)
                                                  dynpro_nr       = '0100'
                                                  docking_side    = cl_gui_docking_container=>align_at_right
                                                  ratio           = 60
                                                  type_name       = CONV #( profile_manager->types-checks )
                                                  sort_table      = VALUE lvc_t_sort( ( spos = 1 fieldname = 'PROFILE' up = abap_true )
                                                                                      ( spos = 2 fieldname = 'CHECKID' up = abap_true ) )
                                                  sy_repid        = sy_repid
                                                  events          = NEW lcl_check_events( ) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_ADD'
                                                     icon      = '@04@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Add'(025) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_EDIT'
                                                     icon      = '@0Z@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Edit'(026) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_REMOVE'
                                                     icon      = '@05@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Remove'(027) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_INFO'
                                                     icon      = '@5E@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Check Documentation'(052) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_ADD_ALL'
                                                     icon      = '@VY@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Add All'(058) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_REMOVE_ALL'
                                                     icon      = '@VZ@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Remove All'(059) ).

        checks_tree->toolbar_control( )->add_button( fcode     = 'BTN_MISSING_CK'
                                                     icon      = '@A7@'
                                                     butn_type = cntb_btype_button
                                                     quickinfo = 'Add Missing Checks'(000) ).

        checks_tree->set_field_visibility( fieldname = 'START_DATE'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'END_DATE'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'OBJECTS_CREATED_ON'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'THRESHOLD'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'PRIO'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'APPLY_ON_PRODUCTIVE_CODE'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'APPLY_ON_TESTCODE'
                                           is_visible = abap_true ).
        checks_tree->set_field_visibility( fieldname = 'IGNORE_PSEUDO_COMMENTS'
                                           is_visible = abap_true ).

        checks_tree->set_field_header_text( fieldname   = 'PROFILE'
                                            header_text = 'Profile'(001) ).
        checks_tree->set_field_header_text( fieldname   = 'CHECKID'
                                            header_text = 'Check ID'(014) ).
        checks_tree->set_field_header_text( fieldname   = 'START_DATE'
                                            header_text = 'Start Date'(030) ).
        checks_tree->set_field_header_text( fieldname   = 'END_DATE'
                                            header_text = 'End Date'(031) ).
        checks_tree->set_field_header_text( fieldname   = 'OBJECTS_CREATED_ON'
                                            header_text = 'Apply On Objects Created Since'(032) ).
        checks_tree->set_field_header_text( fieldname   = 'PRIO'
                                            header_text = 'Message Priority'(033) ).
        checks_tree->set_field_header_text( fieldname   = 'APPLY_ON_PRODUCTIVE_CODE'
                                            header_text = 'Apply on Productive Code'(050) ).
        checks_tree->set_field_header_text( fieldname   = 'APPLY_ON_TESTCODE'
                                            header_text = 'Apply on Testcode'(034) ).

        "Cause of usability, the text is switched on the UX side.
        checks_tree->set_field_header_text( fieldname   = 'IGNORE_PSEUDO_COMMENTS'
                                            header_text = 'Allow Exemptions' ).

        checks_tree->init_display( ).

        checks_tree->deactivate_toolbar( ).

      CATCH cx_failed
            cx_sy_create_data_error.
        LEAVE TO SCREEN 0.

    ENDTRY.
  ENDMETHOD.

  METHOD init_delegates.
    TRY.
        delegates_tree = y_alv_tree_control=>create( alv_header_text = 'Delegates'(029)
                                                     dynpro_nr       = '0100'
                                                     docking_side    = cl_gui_docking_container=>align_at_right
                                                     ratio           = 40
                                                     type_name       = CONV #( profile_manager->types-delegates )
                                                     sort_table      = VALUE lvc_t_sort( ( spos = 1 fieldname = 'PROFILE' up = abap_true )
                                                                                         ( spos = 2 fieldname = 'DELEGATE' up = abap_true ) )
                                                     sy_repid        = sy_repid
                                                     events          = NEW lcl_delegator_events( ) ).

        delegates_tree->toolbar_control( )->add_button( fcode     = 'BTN_ADD'
                                                        icon      = '@04@'
                                                        butn_type = cntb_btype_button
                                                        quickinfo = 'Add'(025) ).

        delegates_tree->toolbar_control( )->add_button( fcode     = 'BTN_REMOVE'
                                                        icon      = '@05@'
                                                        butn_type = cntb_btype_button
                                                        quickinfo = 'Remove'(027) ).

        delegates_tree->set_field_header_text( fieldname   = 'PROFILE'
                                               header_text = 'Profile'(001) ).
        delegates_tree->set_field_header_text( fieldname   = 'DELEGATE'
                                               header_text = 'Delegate'(036) ).


        delegates_tree->init_display( ).

        delegates_tree->deactivate_toolbar( ).

      CATCH cx_failed
            cx_sy_create_data_error.
        LEAVE TO SCREEN 0.

    ENDTRY.
  ENDMETHOD.

  METHOD call_check_info.
    DATA base TYPE REF TO y_check_base.
    TRY.
        CREATE OBJECT base TYPE (io_check_id).
        CALL FUNCTION 'CALL_BROWSER' EXPORTING url = base->settings-documentation.

      CATCH cx_sy_create_object_error.
        MESSAGE 'Failed to find the check!'(043) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD get_cursor_field.
    GET CURSOR FIELD result.
  ENDMETHOD.

  METHOD get_selected_profile.
    FIELD-SYMBOLS: <line> TYPE ytab_profiles.
    DATA(line) = profiles_tree->get_selected_line( ).
    ASSIGN line->* TO <line>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.
    result = <line>.
    UNASSIGN <line>.
  ENDMETHOD.

  METHOD set_selected_profile.
    DATA(index) = profiles_tree->list_control( )->get_line_index( profile ).
    profiles_tree->set_selected_index( index ).
  ENDMETHOD.

  METHOD get_selected_delegate.
    FIELD-SYMBOLS: <line> TYPE ytab_delegates.
    DATA(line) = delegates_tree->get_selected_line( ).
    ASSIGN line->* TO <line>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.
    result = <line>.
    UNASSIGN <line>.
  ENDMETHOD.

  METHOD get_selected_check.
    FIELD-SYMBOLS: <line> TYPE ytab_checks.
    DATA(line) = checks_tree->get_selected_line( ).
    ASSIGN line->* TO <line>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.
    result = <line>.
    result-ignore_pseudo_comments = switch_bool( result-ignore_pseudo_comments ).
    UNASSIGN <line>.
  ENDMETHOD.

  METHOD switch_toolbar_activation.
    TRY.
        get_selected_profile( ).
        checks_tree->activate_toolbar( ).
        delegates_tree->activate_toolbar( ).
      CATCH ycx_entry_not_found.
        checks_tree->deactivate_toolbar( ).
        delegates_tree->deactivate_toolbar( ).
    ENDTRY.
  ENDMETHOD.

  METHOD refresh_checks.
    CHECK checks_tree IS BOUND.
    TRY.
        DATA(checks) = profile_manager->select_checks( get_selected_profile( )-profile ).
        checks = ux_usability_switch( checks ).
        checks_tree->list_control( )->set_table( checks ).

      CATCH ycx_entry_not_found.
        checks_tree->list_control( )->delete_all( ).

    ENDTRY.
    checks_tree->refresh_display( ).
  ENDMETHOD.

  METHOD ux_usability_switch.
    result = checks.
    LOOP AT result ASSIGNING FIELD-SYMBOL(<check>).
      <check>-ignore_pseudo_comments = switch_bool( <check>-ignore_pseudo_comments ).
    ENDLOOP.
    UNASSIGN <check>.
  ENDMETHOD.

  METHOD refresh_delegates.
    CHECK delegates_tree IS BOUND.
    TRY.
        delegates_tree->list_control( )->set_table( profile_manager->select_delegates( get_selected_profile( )-profile ) ).

      CATCH ycx_entry_not_found.
        delegates_tree->list_control( )->delete_all( ).

    ENDTRY.
    delegates_tree->refresh_display( ).
  ENDMETHOD.

  METHOD refresh_profiles.
    CHECK profiles_tree IS BOUND.
    TRY.
        profiles_tree->list_control( )->set_table( profile_manager->select_profiles( sy-uname ) ).

      CATCH ycx_entry_not_found.
        profiles_tree->list_control( )->delete_all( ).

    ENDTRY.
    profiles_tree->refresh_display( ).
  ENDMETHOD.

  METHOD refresh_all_trees.
    refresh_profiles( ).
    refresh_checks( ).
    refresh_delegates( ).
  ENDMETHOD.

  METHOD call_f4help.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield     = referenced_field_name
        value_org    = 'S'
        dynpprog     = sy-cprog
        dynpnr       = sy-dynnr
        window_title = window_title
      TABLES
        value_tab    = value_table
        return_tab   = result
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.
  ENDMETHOD.

  METHOD profile_f4help_200.
    TRY.
        DATA(f4values) = call_f4help( referenced_field_name = 'PROFILE'
                                      window_title          = 'Available Profiles'(009)
                                      value_table = profile_manager->get_registered_profiles( ) ).

        IF f4values IS NOT INITIAL.
          io_profilename = f4values[ 1 ]-fieldval.
        ENDIF.

        LEAVE TO SCREEN 200.

      CATCH cx_failed.
        MESSAGE 'Profiles not found!'(010) TYPE 'I'.

      CATCH ycx_entry_not_found.
        MESSAGE 'Profiles not registered!'(011) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD profile_f4help_600.
    TRY.
        DATA(f4values) = call_f4help( referenced_field_name = 'PROFILE'
                                      window_title          = 'Available Profiles'(009)
                                      value_table = profile_manager->get_registered_profiles( ) ).

        IF f4values IS NOT INITIAL.
          io_profilename = f4values[ 1 ]-fieldval.
        ENDIF.

        LEAVE TO SCREEN 600.

      CATCH cx_failed.
        MESSAGE 'Profiles not found!'(010) TYPE 'I'.

      CATCH ycx_entry_not_found.
        MESSAGE 'Profiles not registered!'(011) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD check_f4help.
    TRY.
        DATA(f4values) = call_f4help( referenced_field_name = 'CHECKID'
                                      window_title = 'Available Checks'(019)
                                      value_table = profile_manager->select_existing_checks( ) ).

        IF f4values IS NOT INITIAL.
          io_check_id = f4values[ 1 ]-fieldval.
          has_edit_mode_started = abap_true.
        ELSE.
          io_check_id = get_selected_check( )-checkid.
        ENDIF.
        io_check_description = profile_manager->get_check_description( io_check_id ).

        LEAVE TO SCREEN 400.

      CATCH cx_failed.
        MESSAGE 'Checks not found!'(020) TYPE 'S'.

      CATCH ycx_entry_not_found.
        MESSAGE 'Checks not registered!'(021) TYPE 'S'.

    ENDTRY.
  ENDMETHOD.

  METHOD init_check_fields_active.
    TRY.
        DATA(obj) = get_check( io_check_id ).

        IF obj->settings-disable_threshold_selection = abap_true.
          set_threshold_active( abap_false ).
        ELSE.
          set_threshold_active( abap_true ).
        ENDIF.

        IF obj->settings-disable_on_prodcode_selection = abap_true.
          set_on_prodcode_active( abap_false ).
        ELSE.
          set_on_prodcode_active( abap_true ).
        ENDIF.

        IF obj->settings-disable_on_testcode_selection = abap_true.
          set_on_testcode_active( abap_false ).
        ELSE.
          set_on_testcode_active( abap_true ).
        ENDIF.

        IF obj->settings-pseudo_comment IS INITIAL.
          set_allow_pcom_active( abap_false ).
        ELSE.
          set_allow_pcom_active( abap_true ).
        ENDIF.

        lbl_pcom_name = obj->settings-pseudo_comment.

        IF has_edit_mode_started = abap_true.
          io_threshold = obj->settings-threshold.
          io_prio = obj->settings-prio.
          io_creation_date = obj->settings-object_created_on.
          chbx_on_prodcode = obj->settings-apply_on_productive_code.
          chbx_on_testcode = obj->settings-apply_on_test_code.
          chbx_allow_pcom = switch_bool( obj->settings-ignore_pseudo_comments ).
          lbl_pcom_name = obj->settings-pseudo_comment.
          has_edit_mode_started = abap_false.
        ENDIF.

      CATCH cx_sy_create_object_error.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_check.
    CREATE OBJECT result TYPE (checkid).
  ENDMETHOD.

  METHOD set_on_prodcode_active.
    set_dynpro_field_active( fieldname = 'LBL_ON_PRODCODE'
                             is_active = is_active ).
    set_dynpro_field_active( fieldname = 'CHBX_ON_PRODCODE'
                             is_active = is_active ).
  ENDMETHOD.

  METHOD set_on_testcode_active.
    set_dynpro_field_active( fieldname = 'LBL_ON_TESTCODE'
                             is_active = is_active ).
    set_dynpro_field_active( fieldname = 'CHBX_ON_TESTCODE'
                             is_active = is_active ).
  ENDMETHOD.

  METHOD set_threshold_active.
    set_dynpro_field_active( fieldname = 'LBL_TEXT_THRESHOLD'
                             is_active = is_active ).
    set_dynpro_field_active( fieldname = 'IO_THRESHOLD'
                             is_active = is_active ).
  ENDMETHOD.

  METHOD set_allow_pcom_active.
    set_dynpro_field_active( fieldname = 'LBL_ALLOW_PCOM'
                             is_active = is_active ).
    set_dynpro_field_active( fieldname = 'CHBX_ALLOW_PCOM'
                             is_active = is_active ).
  ENDMETHOD.

  METHOD set_dynpro_field_active.
    LOOP AT SCREEN INTO DATA(line).

      IF line-name = to_upper( fieldname ).
        IF is_active = abap_true.
          line-input = 1.
        ELSE.
          line-input = 0.
        ENDIF.

        MODIFY SCREEN FROM line.
        EXIT.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD assign_profile.
    CALL SCREEN 200 STARTING AT 10 10.

    IF user_command <> 'ENTR_200'
    OR io_profilename IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        profile_manager->insert_profile( VALUE #( username = sy-uname
                                                  profile  = io_profilename
                                                  is_standard = abap_false
                                                  last_changed_by = sy-uname
                                                  last_changed_on = sy-datum
                                                  last_changed_at = sy-timlo ) ).

      CATCH ycx_failed_to_add_a_line.
        MESSAGE 'Profile already assigned!'(007) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD unassign_profile.
    TRY.
        profile_manager->delete_profile( get_selected_profile( ) ).

      CATCH ycx_failed_to_remove_a_line.
        MESSAGE 'Profile cannot be unassigned!'(008) TYPE 'I'.

      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD copy_profile.
    CALL SCREEN 600 STARTING AT 10 10.

    IF user_command <> 'ENTR_600'
    OR io_profilename IS INITIAL
    OR io_to_profile IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        IF profile_manager->profile_exists( io_to_profile ) = abap_true.
          RAISE EXCEPTION TYPE ycx_failed_to_add_a_line.
        ENDIF.

        DATA(checklist) = profile_manager->select_checks( io_profilename ).

        profile_manager->insert_profile( VALUE #( username = sy-uname
                                                  profile  = io_to_profile
                                                  is_standard = abap_false
                                                  last_changed_by = sy-uname
                                                  last_changed_on = sy-datum
                                                  last_changed_at = sy-timlo ) ).

        profile_manager->check_delegation_rights( io_to_profile ).

        LOOP AT checklist INTO DATA(check).
          check-profile = io_to_profile.
          check-last_changed_by = sy-uname.
          check-last_changed_on = sy-datum.
          check-last_changed_at = sy-timlo.
          TRY.
              profile_manager->insert_check( check ).
            CATCH ycx_failed_to_add_a_line
                  ycx_time_overlap.
              MESSAGE 'Failed to Copy the Check:'(049) && check-checkid TYPE 'I'.
          ENDTRY.
        ENDLOOP.

      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'I'.

      CATCH ycx_entry_not_found.
        MESSAGE 'Please choose a valid profile to copy from!'(047) TYPE 'I'.

      CATCH ycx_failed_to_add_a_line.
        MESSAGE 'A profile can not be copied into another existing profile!'(048) TYPE 'I'.

    ENDTRY.

  ENDMETHOD.

  METHOD  create_template_profile.
    CALL SCREEN 500 STARTING AT 10 10.

    IF user_command <> 'ENTR_500'
    OR io_profilename IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        IF profile_manager->profile_exists( io_profilename ) = abap_true.
          RAISE EXCEPTION TYPE ycx_failed_to_add_a_line.
        ENDIF.

        profile_manager->insert_profile( VALUE #( username = sy-uname
                                                  profile  = io_profilename
                                                  is_standard = abap_false
                                                  last_changed_by = sy-uname
                                                  last_changed_on = sy-datum
                                                  last_changed_at = sy-timlo ) ).

        profile_manager->check_delegation_rights( io_profilename ).

      CATCH ycx_failed_to_add_a_line
            ycx_time_overlap.
        MESSAGE 'Template creation has failed!'(045) TYPE 'I'.

      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD import_profile.
    TRY.
        DATA(structure) = lcl_file=>upload( ).
      CATCH ycx_object_not_processed.
        MESSAGE 'Failed to Import!'(054) TYPE 'E'.
      CATCH cx_abap_invalid_value.
        MESSAGE 'Invalid Imported File!'(055) TYPE 'E'.
    ENDTRY.

    IF profile_manager->profile_exists( structure-profile-profile ) = abap_true.
      request_confirmation( | Would you like to replace the { structure-profile-profile } profile? | ).
      check_check_rights( structure-profile-profile ).
    ENDIF.

    TRY.
        profile_manager->import_profile( structure ).
      CATCH ycx_failed_to_add_a_line
            ycx_time_overlap
            ycx_no_delegation_rights.
        MESSAGE 'Failed to Import!'(054) TYPE 'E'.
    ENDTRY.

    MESSAGE 'Action Executed Successfully!'(056) TYPE 'S'.
  ENDMETHOD.

  METHOD export_profile.
    TRY.
        DATA(profile) = get_selected_profile( ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'W'.
    ENDTRY.

    TRY.
        DATA(checks) = profile_manager->select_checks( profile-profile ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Checks not found!'(020) TYPE 'E'.
    ENDTRY.

    TRY.
        DATA(delegates) = profile_manager->select_delegates( profile-profile ).
      CATCH ycx_entry_not_found.
        MESSAGE 'There must be at least one delegate!'(024) TYPE 'E'.
    ENDTRY.

    TRY.
        lcl_file=>download( profile = profile
                            checks = checks
                            delegates = delegates ).
      CATCH ycx_object_not_processed.
        MESSAGE 'Failed to Export!'(053) TYPE 'E'.
    ENDTRY.

    MESSAGE 'Action Executed Successfully!'(056) TYPE 'S'.
  ENDMETHOD.

  METHOD get_initial_check.
    result-profile = ''.
    result-checkid = ''.
    result-start_date = '20190101'.
    result-end_date = '99991231'.
    result-objects_created_on = '20160101'.
    result-apply_on_testcode = abap_false.
    result-threshold = 0.
    result-prio = 'E'.
    result-last_changed_by = sy-uname.
    result-last_changed_on = sy-datum.
    result-last_changed_at = sy-timlo.
    result-ignore_pseudo_comments = abap_false.
  ENDMETHOD.

  METHOD auto_re_start_delegate.
    TRY.
        CALL SCREEN 300 STARTING AT 10 10.
        add_delegate( ).

      CATCH cx_failed.
        auto_re_start_delegate( ).

    ENDTRY.
  ENDMETHOD.

  METHOD add_delegate.
    CHECK user_command = 'ENTR_300' AND io_delegate_name <> space.

    TRY.
        profile_manager->insert_delegate( VALUE #( profile = get_selected_profile( )-profile
                                                   delegate = io_delegate_name ) ).

      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.

      CATCH ycx_failed_to_add_a_line.
        MESSAGE 'Delegate already exist!'(022) TYPE 'I'.
        RAISE EXCEPTION TYPE cx_failed.
    ENDTRY.
  ENDMETHOD.

  METHOD remove_delegate.
    TRY.
        get_selected_delegate( ).

      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a delegate!'(035)  TYPE 'I'.
        RETURN.

    ENDTRY.

    TRY.
        DATA(lines_of_delegates) = lines( profile_manager->select_delegates( get_selected_profile( )-profile ) ).
        IF lines_of_delegates <= 1.
          RAISE EXCEPTION TYPE ycx_entry_not_found.
        ENDIF.

        profile_manager->delete_delegate( get_selected_delegate( ) ).

      CATCH ycx_entry_not_found.
        MESSAGE 'There must be at least one delegate!'(024) TYPE 'I'.

      CATCH ycx_failed_to_remove_a_line.
        MESSAGE 'Delegate cannot be removed!'(023) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD check_delegate_rights.
    TRY.
        DATA(prof) = get_selected_profile( ).
        IF prof-is_standard = abap_true.
          RAISE EXCEPTION TYPE cx_failed.
        ENDIF.

        profile_manager->check_delegation_rights( prof-profile ).
        result = abap_true.

      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.

      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'I'.

      CATCH cx_failed.
        MESSAGE 'The rights of the standard profile cannot be delegated!'(040) TYPE 'I'.

    ENDTRY.
  ENDMETHOD.

  METHOD auto_re_start_check.
    TRY.
        CALL SCREEN 400 STARTING AT 10 10.
        check_customization( edit_mode ).

      CATCH cx_failed.
        auto_re_start_check( edit_mode ).

    ENDTRY.
  ENDMETHOD.

  METHOD init_add_check.
    DATA obj TYPE REF TO y_check_base.

    io_check_description = ''.
    io_start_date = '20190101'.
    io_end_date = '99991231'.
    io_creation_date = '20160101'.
    io_prio = 'E'.
    io_threshold = 0.
    chbx_on_prodcode = abap_true.
    chbx_on_testcode = abap_true.
    chbx_allow_pcom = abap_true.
    lbl_pcom_name = space.

    TRY.
        CREATE OBJECT obj TYPE (io_check_id).
        io_creation_date = obj->settings-object_created_on.
        io_threshold = obj->settings-threshold.
        io_prio = obj->settings-prio.
        chbx_on_prodcode = obj->settings-apply_on_productive_code.
        chbx_on_testcode = obj->settings-apply_on_test_code.
        chbx_allow_pcom = switch_bool( obj->settings-ignore_pseudo_comments ).
        lbl_pcom_name = obj->settings-pseudo_comment.
      CATCH cx_sy_create_object_error.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD init_edit_check.
    DATA(check_line) = check.

    io_check_id = check_line-checkid.
    io_start_date = check_line-start_date.
    io_end_date = check_line-end_date.
    io_creation_date = check_line-objects_created_on.
    io_threshold = check_line-threshold.
    io_prio = check_line-prio.
    chbx_on_prodcode = check_line-apply_on_productive_code.
    chbx_on_testcode = check_line-apply_on_testcode.
    chbx_allow_pcom = switch_bool( check_line-ignore_pseudo_comments ).

    TRY.
        io_check_description = profile_manager->get_check_description( check_line-checkid ).

      CATCH ycx_entry_not_found.
        io_check_description = ''.

    ENDTRY.

    FREE check_line.
  ENDMETHOD.

  METHOD check_customization.
    CHECK user_command = 'ENTR_400'
    AND io_check_id <> space.

    add_check( edit_mode ).
  ENDMETHOD.

  METHOD add_check.
    TRY.
        DATA(profile) = get_selected_profile( )-profile.
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.
    ENDTRY.

    DATA(check) = VALUE ytab_checks( profile = profile
                                     checkid = io_check_id
                                     start_date = io_start_date
                                     end_date = io_end_date
                                     objects_created_on = io_creation_date
                                     threshold = io_threshold
                                     prio = io_prio
                                     apply_on_productive_code = chbx_on_prodcode
                                     apply_on_testcode = chbx_on_testcode
                                     ignore_pseudo_comments = switch_bool( chbx_allow_pcom )
                                     last_changed_by = sy-uname
                                     last_changed_on = sy-datum
                                     last_changed_at = sy-timlo ).

    TRY.
        profile_manager->get_check_description( check-checkid ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Check is not registered!'(044) TYPE 'I'.
        RAISE EXCEPTION TYPE cx_failed.
    ENDTRY.

    IF chbx_on_prodcode = abap_false
    AND chbx_on_testcode = abap_false.
      MESSAGE 'Please choose Productive Code and/or Testcode for check execution!'(051) TYPE 'I'.
      RAISE EXCEPTION TYPE cx_failed.
    ENDIF.

    TRY.
        IF edit_mode = abap_true.
          profile_manager->check_time_overlap( check = check
                                               selected_check = get_selected_check( ) ).

          profile_manager->delete_check( get_selected_check( ) ).
        ELSE.
          profile_manager->check_time_overlap( check = check ).
        ENDIF.

        profile_manager->insert_check( check ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Check is not registered!'(044) TYPE 'I'.
        RAISE EXCEPTION TYPE cx_failed.

      CATCH ycx_failed_to_add_a_line
            ycx_failed_to_remove_a_line.
        MESSAGE 'Check already exist!'(016) TYPE 'I'.
        RAISE EXCEPTION TYPE cx_failed.

      CATCH ycx_time_overlap.
        MESSAGE 'Please select a different start / end date to avoid a time overlap!'(037) TYPE 'I'.
        RAISE EXCEPTION TYPE cx_failed.

    ENDTRY.
  ENDMETHOD.

  METHOD remove_check.
    TRY.
        profile_manager->delete_check( check ).
      CATCH ycx_failed_to_remove_a_line.
        MESSAGE 'Check cannot be removed!'(018) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD remove_selected_check.
    TRY.
        remove_check( get_selected_check( ) ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a check!'(015) TYPE 'W'.
    ENDTRY.
  ENDMETHOD.

  METHOD check_check_rights.
    TRY.
        profile_manager->check_delegation_rights( profile ).
        result = abap_true.
      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'W'.
      CATCH cx_failed.
        MESSAGE 'Insufficient rights to edit the standard profile!'(039) TYPE 'W'.
    ENDTRY.
  ENDMETHOD.

  METHOD check_selected_check_rights.
    TRY.
        result = check_check_rights( get_selected_profile( )-profile ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'W'.
    ENDTRY.
  ENDMETHOD.

  METHOD request_confirmation.
    DATA answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = | Confirmation |
        text_question         = text_question
        display_cancel_button = abap_false
      IMPORTING
        answer                = answer.

    IF answer <> 1.
      MESSAGE 'Action Canceled.' TYPE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD add_all_checks.
    TRY.
        DATA(profile) = get_selected_profile( )-profile.
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.
    ENDTRY.

    TRY.
        profile_manager->check_delegation_rights( profile ).
      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'W'.
    ENDTRY.

    TRY.
        profile_manager->select_checks( profile ).
        request_confirmation( | Would you like to replace the current checks? | ).
      CATCH ycx_entry_not_found.
        request_confirmation( | Would you like to add all the checks? | ).
    ENDTRY.

    profile_manager->remove_all_checks( profile ).

    TRY.
        DATA(available_checks) = profile_manager->select_existing_checks( ).
      CATCH ycx_entry_not_found.
        MESSAGE 'Checks not registered!'(021) TYPE 'S'.
    ENDTRY.

    LOOP AT available_checks ASSIGNING FIELD-SYMBOL(<check>).
      io_check_id = <check>-checkid.
      init_add_check( ).
      TRY.
          add_check( ).
        CATCH cx_failed.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    MESSAGE 'Action Executed Successfully!'(056) TYPE 'S'.
  ENDMETHOD.


  METHOD remove_all_checks.
    TRY.
        DATA(profile) = get_selected_profile( )-profile.
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.
    ENDTRY.

    TRY.
        profile_manager->check_delegation_rights( profile ).
      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'W'.
    ENDTRY.

    TRY.
        profile_manager->select_checks( profile ).
      CATCH ycx_entry_not_found.
        RETURN.
    ENDTRY.

    request_confirmation( | Would you like to remove all the checks? | ).

    profile_manager->remove_all_checks( profile ).

    MESSAGE 'Action Executed Successfully!'(056) TYPE 'S'.
  ENDMETHOD.

  METHOD add_missing_checks.
    DATA missing_checks TYPE STANDARD TABLE OF ycicc_checkid.

    TRY.
        DATA(profile) = get_selected_profile( )-profile.
      CATCH ycx_entry_not_found.
        MESSAGE 'Please select a profile!'(005) TYPE 'I'.
    ENDTRY.

    TRY.
        profile_manager->check_delegation_rights( profile ).
      CATCH ycx_no_delegation_rights.
        MESSAGE 'You are not a delegate of the profile!'(006) TYPE 'W'.
    ENDTRY.

    TRY.
        DATA(checks_available) = profile_manager->select_checks( profile ).
        request_confirmation( | Would you like to add all missing checks? | ).
      CATCH ycx_entry_not_found.
        add_all_checks( ).
        RETURN.
    ENDTRY.

    DATA(list_of_all_checks) = profile_manager->get_checks_from_db( ).

    LOOP AT list_of_all_checks ASSIGNING FIELD-SYMBOL(<check>).
      IF NOT line_exists( checks_available[ checkid = <check>-obj_name ] ).
        APPEND <check>-obj_name TO missing_checks.
      ENDIF.
    ENDLOOP.

    IF missing_checks IS INITIAL.
      MESSAGE 'No checks are missing!'(060) TYPE 'I'.
      RETURN.
    ENDIF.

    LOOP AT missing_checks ASSIGNING FIELD-SYMBOL(<checkname>).
      io_check_id = <checkname>.
      init_add_check( ).
      TRY.
          add_check( ).
        CATCH cx_failed.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
    MESSAGE 'Action Executed Successfully!'(056) TYPE 'S'.
  ENDMETHOD.


  METHOD switch_bool.
    result = xsdbool( boolean = abap_false ).
  ENDMETHOD.


ENDCLASS.
