CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_max_nesting_depth( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )
      ( 'START-OF-SELECTION.' )

      ( 'DATA val_a TYPE i VALUE 1.' )
      ( 'DATA val_b TYPE i VALUE 2.' )
      ( 'DATA itab TYPE STANDARD TABLE OF i.' )

      ( 'IF val_a = 1.' )
      ( ' val_b = 2.' )
      ( ' LOOP AT itab INTO DATA(line).' )
      ( '  AT FIRST.' )
      ( '  ENDAT.' )
      ( '  CASE line.' )
      ( '   WHEN 0.' )
      ( '     IF sy-tabix = 1.' )
      ( '       IF val_b <> 1.' )
      ( '         val_b = 3.' )
      ( '       ENDIF.' )
      ( '     ENDIF.' )
      ( '  ENDCASE.' )
      ( ' ENDLOOP.' )
      ( 'ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )
      ( 'START-OF-SELECTION.' )

      ( 'DATA val_a TYPE i VALUE 1.' )
      ( 'DATA val_b TYPE i VALUE 2.' )
      ( 'IF val_a = 1.' )
      ( ' val_b = 2.' )
      ( 'ENDIF.' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT ut_test.' )
      ( 'START-OF-SELECTION.' )

      ( 'DATA val_a TYPE i VALUE 1.' )
      ( 'DATA val_b TYPE i VALUE 2.' )
      ( 'DATA itab TYPE STANDARD TABLE OF i.' )

      ( 'IF val_a = 1.' )
      ( ' val_b = 2.' )
      ( ' LOOP AT itab INTO DATA(line).' )
      ( '  AT FIRST.' )
      ( '  ENDAT.' )
      ( '  CASE line.' )
      ( '   WHEN 0.' )
      ( '     IF sy-tabix = 1.' )
      ( '       IF val_b <> 1.' )
      ( '         val_b = 3.' )
      ( '       ENDIF.' )
      ( '     ENDIF.' )
      ( '  ENDCASE.' )
      ( ' ENDLOOP.' )
      ( 'ENDIF. "#EC CI_NESTING' )
    ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_macro DEFINITION INHERITING FROM local_test_class FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_macro IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT ut_test.' )

      ( ' DEFINE example. ' )
      ( '   IF &1 = &2. ' )
      ( '   ENDIF. ' )
      ( ' END-OF-DEFINITION.' )

      ( ' START-OF-SELECTION.' )
      ( '   DATA val_a TYPE i VALUE 1.' )
      ( '   DATA val_b TYPE i VALUE 2.' )
      ( '   DATA itab TYPE STANDARD TABLE OF i.' )

      ( '   IF val_a = 1.' )
      ( '     val_b = 2.' )
      ( '     LOOP AT itab INTO DATA(line).' )
      ( '       AT FIRST.' )
      ( '       ENDAT.' )
      ( '       CASE line.' )
      ( '         WHEN 0.' )
      ( '           IF sy-tabix = 1.' )
      ( '             example sy-uname sy-sysid.' )
      ( '           ENDIF.' )
      ( '       ENDCASE.' )
      ( '     ENDLOOP.' )
      ( '   ENDIF.' )
    ).
  ENDMETHOD.

ENDCLASS.
