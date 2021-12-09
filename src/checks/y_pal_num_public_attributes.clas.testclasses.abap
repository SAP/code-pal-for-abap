CLASS ltc_instance DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_num_public_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     DATA: BEGIN OF one, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
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
      ( '     CONSTANTS two TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION.  ' )
      ( '   PUBLIC SECTION. "#EC NUM_PUBLIC_ATTR ' )
      ( '     DATA: BEGIN OF one, ' )
      ( '             index  TYPE i, ' )
      ( '             spfli_wa TYPE spfli, ' )
      ( '           END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
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
    result ?= NEW y_pal_num_public_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA: BEGIN OF one, ' )
      ( '                   index  TYPE i, ' )
      ( '                   spfli_wa TYPE spfli, ' )
      ( '                 END OF one. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
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
      ( '     CONSTANTS two TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     DATA three TYPE i VALUE 3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION.  ' )
      ( '   PUBLIC SECTION. "#EC NUM_PUBLIC_ATTR ' )
      ( '     CLASS-DATA: BEGIN OF one, ' )
      ( '                   index  TYPE i, ' )
      ( '                   spfli_wa TYPE spfli, ' )
      ( '                 END OF one. ' )
      ( '     CLASS-DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
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
    result ?= NEW y_pal_num_public_attributes( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     CLASS-DATA: BEGIN OF one, ' )
      ( '                   index  TYPE i, ' )
      ( '                   spfli_wa TYPE spfli, ' )
      ( '                 END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( '   PROTECTED SECTION. ' )
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
      ( '     CONSTANTS two TYPE i VALUE 2. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. "#EC NUM_PUBLIC_ATTR ' )
      ( '     CLASS-DATA: BEGIN OF one, ' )
      ( '                   index  TYPE i, ' )
      ( '                   spfli_wa TYPE spfli, ' )
      ( '                 END OF one. ' )
      ( '     DATA two TYPE i VALUE 2 READ-ONLY. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-DATA three TYPE i VALUE 3. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
