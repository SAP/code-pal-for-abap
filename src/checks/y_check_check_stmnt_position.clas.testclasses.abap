CLASS ltc_not_in_first_position DEFINITION INHERITING FROM y_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_not_in_first_position IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     CHECK name = name2.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     CHECK name2 = name.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     CHECK name = name2. "#EC CHECK_POSITION ' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_in_loop DEFINITION INHERITING FROM ltc_not_in_first_position FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_in_loop IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       CHECK <tadir>-delflag = abap_true.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_after_loop DEFINITION INHERITING FROM ltc_not_in_first_position FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_after_loop IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     CHECK name = name2.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     IF name <> name2.' )
      ( '       RETURN.' )
      ( '     ENDIF.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '     CHECK name = name2.  "#EC CHECK_POSITION' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_before_loop DEFINITION INHERITING FROM ltc_not_in_first_position FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_before_loop IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_check_check_stmnt_position( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     CHECK name = name2.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     IF name <> name2.' )
      ( '       RETURN.' )
      ( '     ENDIF.' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )

      ( ' CLASS y_example_class DEFINITION. ' )
      ( '   PUBLIC SECTION. ' )
      ( '     METHODS example. ' )
      ( ' ENDCLASS. ' )

      ( ' CLASS y_example_class IMPLEMENTATION. ' )
      ( '   METHOD example.' )
      ( '     DATA tadir TYPE TABLE OF tadir.' )
      ( '     DATA name type string.' )
      ( '     DATA(name2) = name.' )
      ( '     name = name2.' )
      ( '     CHECK name = name2.  "#EC CHECK_POSITION' )
      ( '     LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).' )
      ( '       IF <tadir>-delflag = abap_true.' )
      ( '         CONTINUE.' )
      ( '       ENDIF.' )
      ( '     ENDLOOP.' )
      ( '   ENDMETHOD.' )
      ( ' ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.
