CLASS ltc_append_to DEFINITION INHERITING FROM y_unit_test_base FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PROTECTED SECTION.
    METHODS: get_cut REDEFINITION,
      get_code_with_issue REDEFINITION,
      get_code_without_issue REDEFINITION,
      get_code_with_exemption REDEFINITION.

ENDCLASS.


CLASS ltc_append_to IMPLEMENTATION.

  METHOD get_code_without_issue.

    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_insert_into_table TYPE TABLE OF string. ' )
      ( '     INSERT `example` INTO TABLE prefer_insert_into_table. ' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).

  ENDMETHOD.

  METHOD get_code_with_exemption.

    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_insert_into_table TYPE TABLE OF string. ' )
      ( '     APPEND `example` TO prefer_insert_into_table. "#EC PREF_INSERT_INT' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).

  ENDMETHOD.

  METHOD get_code_with_issue.

    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_insert_into_table TYPE TABLE OF string. ' )
      ( '     APPEND `example` TO prefer_insert_into_table.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).

  ENDMETHOD.

  METHOD get_cut.
    result ?= NEW y_check_prefer_insert_into( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_append_initial_line DEFINITION INHERITING FROM ltc_append_to FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_append_initial_line IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_insert_into_table TYPE TABLE OF string. ' )
      ( '     FIELD-SYMBOLS <example> TYPE string. ' )
      ( '     APPEND INITIAL LINE TO prefer_insert_into_table ASSIGNING <example>.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_append_lines_of DEFINITION INHERITING FROM ltc_append_to FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_append_lines_of IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA prefer_insert_into_table TYPE TABLE OF string. ' )
      ( '     DATA example_table TYPE TABLE OF string. ' )
      ( '     APPEND LINES OF example_table TO prefer_insert_into_table.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_select DEFINITION INHERITING FROM ltc_append_to FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_select IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )
      ( ' START-OF-SELECTION.' )
      ( '   DATA example_table TYPE TABLE OF sflight.' )
      ( '   SELECT * FROM sflight APPENDING TABLE example_table.' )
    ).
  ENDMETHOD.

ENDCLASS.
