CLASS ltc_default_key DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_expected_count REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_default_key IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_default_key( ).
  ENDMETHOD.

  METHOD get_expected_count.
    result = 2.
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. ' )

      ( '   DATA itab1 TYPE my_type. ' )
      ( '   DATA itab2 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES my_type TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object. ' )

      ( '   DATA itab1 TYPE my_type. ' )
      ( '   DATA itab2 TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )

      ( '   DATA itab1 TYPE my_type. ' )
      ( '   DATA itab2 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_empty_key DEFINITION INHERITING FROM ltc_default_key FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_empty_key IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES my_type TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.' )

      ( '   DATA itab1 TYPE my_type. ' )
      ( '   DATA itab2 TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_instance_attribute DEFINITION INHERITING FROM ltc_default_key FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_instance_attribute IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA itab1 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.' )
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
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object.' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA itab1 TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object.' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )
      ( '   PRIVATE SECTION. ' )
      ( '     DATA itab1 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_static_attribute DEFINITION INHERITING FROM ltc_default_key FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_static_attribute IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA itab1 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.' )
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
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object.' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA itab1 TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object.' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' CLASS y_example DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     TYPES my_type TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )
      ( '   PRIVATE SECTION. ' )
      ( '     CLASS-DATA itab1 TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY. "#EC DEFAULT_KEY' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example IMPLEMENTATION. ' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_multiple_types DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_types IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_default_key( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES: BEGIN OF my_type, ' )
      ( '            non_unique TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object, ' )
      ( '            default_key TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY, ' )
      ( '            empty_key TYPE STANDARD TABLE OF tadir WITH EMPTY KEY, ' )
      ( '          END OF my_type. ' )

      ( '   DATA tables TYPE my_type. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES: BEGIN OF my_type, ' )
      ( '            non_unique TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object, ' )
      ( '            empty_key TYPE STANDARD TABLE OF tadir WITH EMPTY KEY, ' )
      ( '          END OF my_type. ' )

      ( '   DATA tables TYPE my_type. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   TYPES: BEGIN OF my_type, ' )
      ( '            non_unique TYPE STANDARD TABLE OF tadir WITH NON-UNIQUE KEY object, ' )
      ( '            default_key TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY, "#EC DEFAULT_KEY ' )
      ( '            empty_key TYPE STANDARD TABLE OF tadir WITH EMPTY KEY, ' )
      ( '          END OF my_type. ' )

      ( '   DATA tables TYPE my_type. ' )
    ).
  ENDMETHOD.

ENDCLASS.
