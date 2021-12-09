CLASS ltc_one_exporting DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_one_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_returning( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name EXPORTING result TYPE string. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name RETURNING VALUE(result) TYPE string. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name EXPORTING result TYPE string. "#EC PREFER_RET ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_two_exportings DEFINITION INHERITING FROM ltc_one_exporting FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_two_exportings IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_contact EXPORTING name TYPE string ' )
      ( '                                     email TYPE string. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_contact. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_multiple_one_exporting DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_one_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_returning( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 3.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name EXPORTING result TYPE string. ' )
      ( '       METHODS get_age EXPORTING result TYPE string. ' )
      ( '       METHODS get_email EXPORTING result TYPE string. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name RETURNING VALUE(result) TYPE string. ' )
      ( '       METHODS get_age RETURNING VALUE(result) TYPE string. ' )
      ( '       METHODS get_email RETURNING VALUE(result) TYPE string. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_name EXPORTING result TYPE string. "#EC PREFER_RET ' )
      ( '       METHODS get_age EXPORTING result TYPE string. "#EC PREFER_RET ' )
      ( '       METHODS get_email EXPORTING result TYPE string. "#EC PREFER_RET ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_name. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_standard_table DEFINITION INHERITING FROM ltc_one_exporting FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_standard_table IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS get_entries EXPORTING table TYPE STANDARD TABLE. ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD get_entries. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_export_and_returning DEFINITION INHERITING FROM ltc_one_exporting FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_export_and_returning IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION. ' )
      ( '   CLASS example DEFINITION. ' )
      ( '     PUBLIC SECTION. ' )
      ( '       METHODS run ' )
      ( '         EXPORTING ' )
      ( '           !error        TYPE char1 ' )
      ( '         RETURNING ' )
      ( '           VALUE(result) TYPE string_table. "#EC NUM_OUTPUT_PARA ' )
      ( '   ENDCLASS. ' )

      ( '   CLASS example IMPLEMENTATION. ' )
      ( '     METHOD run. ' )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
