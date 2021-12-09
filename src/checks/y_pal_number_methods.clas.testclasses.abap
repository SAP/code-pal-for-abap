CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_number_methods( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '     METHODS two. ' )
      ( '     METHODS three. ' )
      ( '     METHODS four. ' )
      ( '     METHODS five. ' )
      ( '     METHODS six. ' )
      ( '     METHODS seven. ' )
      ( '     METHODS eight. ' )
      ( '     METHODS nine. ' )
      ( '     METHODS ten. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS eleven. ' )
      ( '     CLASS-METHODS twelve. ' )
      ( '     CLASS-METHODS thirteen. ' )
      ( '     CLASS-METHODS fourteen. ' )
      ( '     CLASS-METHODS fifteen. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS: ' )
      ( '       sixteen, ' )
      ( '       seventeen, ' )
      ( '       eighteen, ' )
      ( '       nineteen, ' )
      ( '       twenty. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ENDMETHOD. ' )
      ( '   METHOD two. ENDMETHOD. ' )
      ( '   METHOD three. ENDMETHOD. ' )
      ( '   METHOD four. ENDMETHOD. ' )
      ( '   METHOD five. ENDMETHOD. ' )
      ( '   METHOD six. ENDMETHOD. ' )
      ( '   METHOD seven. ENDMETHOD. ' )
      ( '   METHOD eight. ENDMETHOD. ' )
      ( '   METHOD nine. ENDMETHOD. ' )
      ( '   METHOD ten. ENDMETHOD. ' )
      ( '   METHOD eleven. ENDMETHOD. ' )
      ( '   METHOD twelve. ENDMETHOD. ' )
      ( '   METHOD thirteen. ENDMETHOD. ' )
      ( '   METHOD fourteen. ENDMETHOD. ' )
      ( '   METHOD fifteen. ENDMETHOD. ' )
      ( '   METHOD sixteen. ENDMETHOD. ' )
      ( '   METHOD seventeen. ENDMETHOD. ' )
      ( '   METHOD eighteen. ENDMETHOD. ' )
      ( '   METHOD nineteen. ENDMETHOD. ' )
      ( '   METHOD twenty. ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. "#EC NUMBER_METHODS ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '   PROTECTED SECTION. ' )
      ( '   PRIVATE SECTION. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ' )
      ( '   ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )
      ( ' CLASS y_example_class DEFINITION. "#EC NUMBER_METHODS ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS one. ' )
      ( '     METHODS two. ' )
      ( '     METHODS three. ' )
      ( '     METHODS four. ' )
      ( '     METHODS five. ' )
      ( '     METHODS six. ' )
      ( '     METHODS seven. ' )
      ( '     METHODS eight. ' )
      ( '     METHODS nine. ' )
      ( '     METHODS ten. ' )
      ( '   PROTECTED SECTION. ' )
      ( '     CLASS-METHODS eleven. ' )
      ( '     CLASS-METHODS twelve. ' )
      ( '     CLASS-METHODS thirteen. ' )
      ( '     CLASS-METHODS fourteen. ' )
      ( '     CLASS-METHODS fifteen. ' )
      ( '   PRIVATE SECTION. ' )
      ( '     METHODS: ' )
      ( '       sixteen, ' )
      ( '       seventeen, ' )
      ( '       eighteen, ' )
      ( '       nineteen, ' )
      ( '       twenty. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD one. ENDMETHOD. ' )
      ( '   METHOD two. ENDMETHOD. ' )
      ( '   METHOD three. ENDMETHOD. ' )
      ( '   METHOD four. ENDMETHOD. ' )
      ( '   METHOD five. ENDMETHOD. ' )
      ( '   METHOD six. ENDMETHOD. ' )
      ( '   METHOD seven. ENDMETHOD. ' )
      ( '   METHOD eight. ENDMETHOD. ' )
      ( '   METHOD nine. ENDMETHOD. ' )
      ( '   METHOD ten. ENDMETHOD. ' )
      ( '   METHOD eleven. ENDMETHOD. ' )
      ( '   METHOD twelve. ENDMETHOD. ' )
      ( '   METHOD thirteen. ENDMETHOD. ' )
      ( '   METHOD fourteen. ENDMETHOD. ' )
      ( '   METHOD fifteen. ENDMETHOD. ' )
      ( '   METHOD sixteen. ENDMETHOD. ' )
      ( '   METHOD seventeen. ENDMETHOD. ' )
      ( '   METHOD eighteen. ENDMETHOD. ' )
      ( '   METHOD nineteen. ENDMETHOD. ' )
      ( '   METHOD twenty. ENDMETHOD. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
