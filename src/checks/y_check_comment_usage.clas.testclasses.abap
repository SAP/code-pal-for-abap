CLASS ltc_report DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_report IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_comment_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'AT SELECTION-SCREEN.' )
      ( '  DATA name3 TYPE string. ' )
      ( '*COMMENT ' )
      ( '  "do something ' )
      ( '  "do more ' )
      ( '  "do much more ' )
      ( '  "but not any time soon ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( 'AT SELECTION-SCREEN.' )
      ( '  DATA name3 TYPE string. ' )
      ( '  "?<html> ' )
      ( '*"*COMMENT ' )
      ( '*" ' )
      ( '  "! docu ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    RETURN.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_class DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_comment_usage( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( '  CLASS y_example DEFINITION.' )
      ( '    PUBLIC SECTION. ' )
      ( '      METHODS example.' )
      ( '  ENDCLASS. ' )

      ( '  CLASS y_example IMPLEMENTATION.' )
      ( '    METHOD example.' )
      ( '* COMMENT ' )
      ( '     "do something ' )
      ( '     "do more ' )
      ( '     "do much more ' )
      ( '     "but not any time soon ' )
      ( '    ENDMETHOD.' )
      ( '  ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( '  CLASS y_example DEFINITION.' )
      ( '    PUBLIC SECTION. ' )
      ( '      METHODS example.' )
      ( '  ENDCLASS. ' )

      ( '  CLASS y_example IMPLEMENTATION.' )
      ( '    METHOD example.' )
      ( '      DATA name3 TYPE string. ' )
      ( '      "?<html> ' )
      ( '*"*COMMENT ' )
      ( '*" ' )
      ( '      "! docu ' )
      ( '    ENDMETHOD.' )
      ( '  ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    RETURN.
  ENDMETHOD.

ENDCLASS.
