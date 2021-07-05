CLASS ltc_string DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_string IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_untranslatable_message( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE 'File not found!' TYPE 'W'. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE i002(00). | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE 'File not found!' TYPE 'W'. "#EC UNTRANSL_MSG | )
    ).
  ENDMETHOD.

ENDCLASS.
