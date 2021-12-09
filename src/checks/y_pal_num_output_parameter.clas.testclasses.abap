CLASS ltc_exporting DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_num_output_parameter( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example EXPORTING exporting_01 TYPE REF TO cl_oo_class ' )
      ( '                               exporting_02 TYPE REF TO cl_oo_class ' )
      ( '                     RAISING  cx_dynamic_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example EXPORTING exporting_01 TYPE REF TO cl_oo_class ' )
      ( '                     RAISING  cx_dynamic_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     METHODS example EXPORTING exporting_01 TYPE REF TO cl_oo_class ' )
      ( '                               exporting_02 TYPE REF TO cl_oo_class ' )
      ( '                     RAISING  cx_dynamic_check. "#EC NUM_OUTPUT_PARA ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
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
    result ?= NEW y_pal_num_output_parameter( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS example CHANGING changing_01 TYPE REF TO cl_oo_class ' )
      ( '                              changing_02 TYPE REF TO cl_oo_class ' )
      ( '                     RAISING  cx_dynamic_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS example CHANGING changing_01 TYPE REF TO cl_oo_class ' )
      ( '                           RAISING  cx_dynamic_check. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS example CHANGING changing_01 TYPE REF TO cl_oo_class ' )
      ( '                              changing_02 TYPE REF TO cl_oo_class ' )
      ( '                     RAISING  cx_dynamic_check. "#EC NUM_OUTPUT_PARA ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
