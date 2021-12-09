CLASS ltc_exporting DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_method_output_param( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_user EXPORTING name TYPE abap_bool' )
      ( '                      RETURNING VALUE(active) TYPE abap_bool.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_user.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_name RETURNING VALUE(result) TYPE abap_bool.' )
      ( '    METHODS: get_active RETURNING VALUE(result) TYPE abap_bool.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_name.' )
      ( '  ENDMETHOD.' )
      ( '  METHOD get_active.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_user EXPORTING name TYPE abap_bool' )
      ( '                      RETURNING VALUE(active) TYPE abap_bool. "#EC PARAMETER_OUT ' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_user.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_changing DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_changing IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_method_output_param( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_user CHANGING  name TYPE abap_bool' )
      ( '                      RETURNING VALUE(active) TYPE abap_bool.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_user.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_name CHANGING VALUE(result) TYPE abap_bool.' )
      ( '    METHODS: get_active CHANGING VALUE(result) TYPE abap_bool.' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_name.' )
      ( '  ENDMETHOD.' )
      ( '  METHOD get_active.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( 'CLASS lcl_classname DEFINITION.' )
      ( '  PUBLIC SECTION.' )
      ( '    METHODS: get_user CHANGING  name TYPE abap_bool' )
      ( '                      RETURNING VALUE(active) TYPE abap_bool. "#EC PARAMETER_OUT ' )
      ( 'ENDCLASS.' )

      ( 'CLASS lcl_classname IMPLEMENTATION.' )
      ( '  METHOD get_user.' )
      ( '  ENDMETHOD.' )
      ( 'ENDCLASS.' )
    ).
  ENDMETHOD.

ENDCLASS.
