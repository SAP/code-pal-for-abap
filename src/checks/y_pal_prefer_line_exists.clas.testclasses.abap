*"* use this source file for your ABAP unit test classes
CLASS ltc_read_table DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_read_table IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_line_exists( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( |   READ TABLE tadir TRANSPORTING NO FIELDS WITH KEY object = 'y_check_prefer_line_exists'. | )

      ( '   IF sy-subrc = 0. ' )
      ( '     exists = abap_true. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( |   DATA(exists) = xsdbool( line_exists( tadir[ object = 'y_check_prefer_line_exists' ] ) ). | )
      ( |   DATA(index) = line_index( tadir[ object = 'y_check_prefer_line_exists' ] ). | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( |   READ TABLE tadir TRANSPORTING NO FIELDS WITH KEY object = 'y_check_prefer_line_exists'. "#EC PREF_LINE_EX | )

      ( '   IF sy-subrc = 0. ' )
      ( '     exists = abap_true. ' )
      ( '   ENDIF. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_loop_at DEFINITION INHERITING FROM ltc_read_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_loop_at IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( |   LOOP AT tadir TRANSPORTING NO FIELDS WHERE object = 'y_check_prefer_line_exists'. | )
      ( '     exists = abap_true. ' )
      ( '   ENDLOOP. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( |   LOOP AT tadir TRANSPORTING NO FIELDS WHERE object = 'y_check_prefer_line_exists'. "#EC PREF_LINE_EX | )
      ( '     exists = abap_true. ' )
      ( '   ENDLOOP. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_loop_at_from_to DEFINITION INHERITING FROM ltc_loop_at FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_loop_at_from_to IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA from_list TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( |   LOOP AT from_list TRANSPORTING NO FIELDS WHERE object = 'y_check_prefer_line_exists'. | )
      ( '     exists = abap_true. ' )
      ( '   ENDLOOP. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA tadir TYPE TABLE OF tadir. ' )
      ( '   DATA exists TYPE abap_bool. ' )

      ( '   LOOP AT tadir TRANSPORTING NO FIELDS ' )
      ( '   FROM 1 TO 50 ' )
      ( |   WHERE object = 'y_check_prefer_line_exists'. | )
      ( '     exists = abap_true. ' )
      ( '   ENDLOOP. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_loop_in DEFINITION INHERITING FROM ltc_loop_at FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_loop_in IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.      ' )
      ( '   DATA actual TYPE TABLE OF tadir. ' )
      ( '   DATA expected TYPE RANGE OF tadir-obj_name. ' )

      ( '   LOOP AT actual TRANSPORTING NO FIELDS WHERE obj_name IN expected. ' )
      ( '   EXIT. ' )
      ( '   ENDLOOP. ' )
    ).
  ENDMETHOD.

ENDCLASS.
