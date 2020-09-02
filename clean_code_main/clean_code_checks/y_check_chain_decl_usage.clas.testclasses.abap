CLASS local_test_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_chain_decl_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA file TYPE REF TO cl_abap_json.' )
      ( '     DATA data TYPE REF TO data.' )
      ( '     DATA: ' )
      ( '       string TYPE string, ' )
      ( '       json TYPE REF TO cl_abap_json, ' )
      ( '       mandt LIKE sy-mandt. ' )
      ( |     string = 'value'. | )
      ( '     json = NEW cl_abap_json( ). ' )
      ( '     mandt = sy-mandt. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA file TYPE REF TO cl_abap_json.' )
      ( '     DATA: payload TYPE REF TO cl_abap_json.' )
      ( '     DATA data TYPE REF TO data.' )
      ( |     DATA(string) = 'value'. | )
      ( '     DATA(json) = NEW cl_abap_json( ). ' )
      ( '     DATA(mandt) = sy-mandt. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example. ' )
      ( '     DATA file TYPE REF TO cl_abap_json.' )
      ( '     DATA data TYPE REF TO data.' )
      ( '     DATA: "#EC CHAIN_DECL_USAG ' )
      ( '       string TYPE string, ' )
      ( '       json TYPE REF TO cl_abap_json, ' )
      ( '       mandt LIKE sy-mandt. ' )
      ( |     string = 'value'. | )
      ( '     json = NEW cl_abap_json( ). ' )
      ( '     mandt = sy-mandt. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
