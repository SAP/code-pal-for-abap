CLASS lcl_unit_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_alv_tree_control.

    METHODS setup.
    METHODS init_profiles.
    METHODS set_all_fields_invisible FOR TESTING.
    METHODS autosize_all_fields_if FOR TESTING.
    METHODS autosize_all_fields_else FOR TESTING.
    METHODS get_excluded_toolbars FOR TESTING.
    METHODS get_selected_index FOR TESTING.
    METHODS get_selected_line FOR TESTING.
    METHODS set_field_header_text FOR TESTING.
    METHODS set_field_visibility_true FOR TESTING.
    METHODS set_field_visibility_false FOR TESTING.

ENDCLASS.

CLASS y_alv_tree_control DEFINITION LOCAL FRIENDS lcl_unit_test.

CLASS lcl_unit_test IMPLEMENTATION.

  METHOD set_field_visibility_false.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' no_out = abap_false ) TO cut->fieldcats.

    cut->y_if_alv_tree_control~set_field_visibility( fieldname  = 'NAME'
                                                     is_visible = abap_false ).

    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' no_out = abap_true ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'set_field_visibility to false is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD set_field_visibility_true.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' no_out = abap_true ) TO cut->fieldcats.

    cut->y_if_alv_tree_control~set_field_visibility( fieldname  = 'NAME'
                                                     is_visible = abap_true ).

    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' no_out = abap_false ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'set_field_visibility to true is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD set_field_header_text.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' ) TO cut->fieldcats.

    cut->y_if_alv_tree_control~set_field_header_text( fieldname   = 'NAME'
                                                      header_text = 'text' ).

    APPEND VALUE lvc_s_fcat( fieldname = 'NAME' coltext = 'text' ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'set_field_header_text is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD get_selected_line.
    init_profiles( ).
    TRY.
        DATA(sl) = cut->y_if_alv_tree_control~get_selected_line( ).
      CATCH ycx_entry_not_found.
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>assert_initial( act  = sl
                                         msg  = 'get selected line is not initial!'
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD get_selected_index.
    init_profiles( ).
    TRY.
        DATA(si) = cut->y_if_alv_tree_control~get_selected_index( ).
      CATCH ycx_entry_not_found.
        RETURN.
    ENDTRY.
    cl_abap_unit_assert=>assert_initial( act  = si
                                         msg  = 'get selected index is not initial!'
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD init_profiles.
    DATA profiles TYPE STANDARD TABLE OF ytab_profiles.
    APPEND VALUE ytab_profiles( username = 'RANDOM' profile = 'TEST' ) TO profiles.
    cut->y_if_alv_tree_control~list_control( )->set_table( profiles ).
  ENDMETHOD.

  METHOD set_all_fields_invisible.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND INITIAL LINE TO cut->fieldcats.

    cut->set_all_fields_invisible( ).

    APPEND VALUE lvc_s_fcat( no_out = abap_true ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'fieldcats are not invisible!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD autosize_all_fields_if.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND VALUE lvc_s_fcat( dd_outlen = 10 coltext = 'text' ) TO cut->fieldcats.

    cut->autosize_all_fields( ).

    APPEND VALUE lvc_s_fcat( dd_outlen = 10 outputlen = 17 coltext = 'text' ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'fieldcats autosize, if branch is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD autosize_all_fields_else.
    DATA mock_fieldcats TYPE lvc_t_fcat.
    CLEAR cut->fieldcats.
    APPEND VALUE lvc_s_fcat( dd_outlen = 2 coltext = 'text' ) TO cut->fieldcats.

    cut->autosize_all_fields( ).

    APPEND VALUE lvc_s_fcat( dd_outlen = 2 outputlen = 11 coltext = 'text' ) TO mock_fieldcats.

    cl_abap_unit_assert=>assert_equals( exp  = mock_fieldcats
                                        act  = cut->fieldcats
                                        msg  = 'fieldcats autosize, else branch is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD get_excluded_toolbars.
    DATA exp TYPE ui_functions.
    APPEND cl_gui_alv_tree_simple=>mc_fc_calculate TO exp.
    APPEND cl_gui_alv_tree_simple=>mc_fc_print_back TO exp.
    APPEND cl_gui_alv_tree_simple=>mc_fc_current_variant TO exp.
    APPEND cl_gui_alv_tree_simple=>mc_fc_change_hierarchy TO exp.

    cl_abap_unit_assert=>assert_equals( exp  = exp
                                        act  = cut->get_excluded_toolbars( )
                                        msg  = 'The returning parameter of get_excluding_toolbars is incorrect!'
                                        quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

  METHOD setup.
    TRY.
        cut = NEW y_alv_tree_control( type_name   = 'YTAB_PROFILES'
                                      sort_table  = VALUE #( )
                                      alv_tree    = NEW #( )
                                      alv_header  = VALUE #( ) ).
      CATCH cx_sy_create_data_error
            cx_failed.
        cl_abap_unit_assert=>abort( 'cut cannot be initialized!' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
