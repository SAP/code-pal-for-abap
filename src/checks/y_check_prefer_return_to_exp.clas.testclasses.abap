CLASS ltc_one_exporting DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_one_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_return_to_exp( ).
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
      ( |       result = 'code pal'. | )
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
      ( |       result = 'code pal'. | )
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
      ( |       result = 'code pal'. | )
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
      ( |       name = 'code pal'. | )
      ( |       email = 'codepal@codepal.com'. | )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_multiple_one_exporting DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_one_exporting IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_prefer_return_to_exp( ).
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
      ( |       result = 'code pal'. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( |       result = 5. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( |       result = 'codepal@codepal.com'. | )
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
      ( |       result = 'code pal'. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( |       result = 5. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( |       result = 'codepal@codepal.com'. | )
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
      ( |       result = 'code pal'. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_age. ' )
      ( |       result = 5. | )
      ( '     ENDMETHOD. ' )
      ( '     METHOD get_email. ' )
      ( |       result = 'codepal@codepal.com'. | )
      ( '     ENDMETHOD. ' )
      ( '   ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
