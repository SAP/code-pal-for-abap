CLASS ltc_instance DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '     DATA four TYPE i VALUE 4. ' )
      ( '     DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA nine TYPE i VALUE 9. ' )
      ( '     DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_static DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_static IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     CLASS-DATA six TYPE i VALUE 6. ' )
      ( '     CLASS-DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     CLASS-DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     CLASS-DATA six TYPE i VALUE 6. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     CLASS-DATA six TYPE i VALUE 6. ' )
      ( '     CLASS-DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     CLASS-DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_mixed DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_mixed IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 2. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. "#EC NUMBER_ATTR ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA one TYPE i VALUE 1. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2. ' )
      ( '     DATA three TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA four TYPE i VALUE 4. ' )
      ( '     CLASS-DATA five TYPE i VALUE 5. ' )
      ( '     DATA six TYPE i VALUE 6. ' )
      ( '     DATA seven TYPE i VALUE 7. ' )
      ( '     CLASS-DATA eight TYPE i VALUE 8. ' )
      ( '     CLASS-DATA nine TYPE i VALUE 9. ' )
      ( '     CLASS-DATA ten TYPE i VALUE 10. ' )
      ( '     DATA eleven TYPE i VALUE 11. ' )
      ( '     CLASS-DATA twelve TYPE i VALUE 12. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
