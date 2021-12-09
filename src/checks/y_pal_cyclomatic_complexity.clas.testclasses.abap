CLASS local_test_class DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS local_test_class IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_cyclomatic_complexity( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example.              ' )
      ( ' START-OF-SELECTION.            ' )
      ( '   DATA(complex) = abap_false.  ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_true.     ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example.              ' )
      ( ' START-OF-SELECTION.            ' )
      ( '   DATA(complex) = abap_false.  ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.         ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example.              ' )
      ( ' START-OF-SELECTION.            ' )
      ( '   DATA(complex) = abap_false.  ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_false.    ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF.                       ' )
      ( '   IF complex = abap_false.     ' )
      ( '     IF complex = abap_false.   ' )
      ( '       complex = abap_true.     ' )
      ( '     ENDIF.                     ' )
      ( '   ENDIF. "#EC CI_CYCLO         ' )
    ).
  ENDMETHOD.

ENDCLASS.
