CLASS ltc_single_if DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_single_if IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_case_to_elseif( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.        ' )
      ( '  DATA(value) = 0.          ' )
      ( '                            ' )
      ( '  IF sy-mandt = 000.        ' )
      ( '    value = 0.              ' )
      ( '  ELSEIF sy-mandt = 100.    ' )
      ( '    value = 1.              ' )
      ( '  ELSEIF sy-mandt = 200.    ' )
      ( '    value = 2.              ' )
      ( '  ELSEIF sy-mandt = 300.    ' )
      ( '    value = 3.              ' )
      ( '  ELSEIF sy-mandt = 400.    ' )
      ( '    value = 4.              ' )
      ( '  ELSEIF sy-mandt = 500.    ' )
      ( '    value = 5.              ' )
      ( '  ENDIF.                    ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.        ' )
      ( '  DATA(value) = 0.          ' )
      ( '                            ' )
      ( '  CASE sy-mandt.            ' )
      ( '    WHEN 000.               ' )
      ( '      value = 0.            ' )
      ( '    WHEN 100.               ' )
      ( '      value = 1.            ' )
      ( '    WHEN 200.               ' )
      ( '      value = 2.            ' )
      ( '    WHEN 300.               ' )
      ( '      value = 3.            ' )
      ( '    WHEN 400.               ' )
      ( '      value = 4.            ' )
      ( '    WHEN 500.               ' )
      ( '      value = 5.            ' )
      ( '  ENDCASE.                  ' )
    ).

  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.                   ' )
      ( '  DATA(value) = 0.                     ' )
      ( '                                       ' )
      ( '  IF sy-mandt = 000. "#EC PREFER_CASE  ' )
      ( '    value = 0.                         ' )
      ( '  ELSEIF sy-mandt = 100.               ' )
      ( '    value = 1.                         ' )
      ( '  ELSEIF sy-mandt = 200.               ' )
      ( '    value = 2.                         ' )
      ( '  ELSEIF sy-mandt = 300.               ' )
      ( '    value = 3.                         ' )
      ( '  ELSEIF sy-mandt = 400.               ' )
      ( '    value = 4.                         ' )
      ( '  ELSEIF sy-mandt = 500.               ' )
      ( '    value = 5.                         ' )
      ( '  ENDIF.                               ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_ifs DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_ifs IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_case_to_elseif( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.          ' )
      ( '  DATA(value) = 0.            ' )
      ( '                              ' )
      ( |  IF sy-langu = 'EN'.         | )
      ( '    IF sy-mandt = 000.        ' )
      ( '      value = 0.              ' )
      ( '    ELSEIF sy-mandt = 100.    ' )
      ( '      value = 1.              ' )
      ( '    ELSEIF sy-mandt = 200.    ' )
      ( '      value = 2.              ' )
      ( '    ENDIF.                    ' )
      ( |  ELSEIF sy-langu = 'DE'.     | )
      ( '    IF sy-mandt = 300.        ' )
      ( '      value = 3.              ' )
      ( '    ELSEIF sy-mandt = 400.    ' )
      ( '      value = 4.              ' )
      ( '    ELSEIF sy-mandt = 500.    ' )
      ( '      value = 5.              ' )
      ( '    ELSEIF sy-mandt = 600.    ' )
      ( '      value = 6.              ' )
      ( '    ELSEIF sy-mandt = 700.    ' )
      ( '      value = 7.              ' )
      ( '    ENDIF.                    ' )
      ( |  ELSEIF sy-langu = 'PT'.     | )
      ( '      value = 8.              ' )
      ( |  ELSEIF sy-mandt = 100.      | )
      ( '      value = 8.              ' )
      ( |  ELSEIF sy-mandt = 200.      | )
      ( '      value = 8.              ' )
      ( '  ENDIF.                      ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.        ' )
      ( '  DATA(value) = 0.          ' )
      ( '                            ' )
      ( |  IF sy-langu = 'EN'.       | )
      ( '    CASE sy-mandt.          ' )
      ( '      WHEN 000.             ' )
      ( '        value = 0.          ' )
      ( '      WHEN 100.             ' )
      ( '        value = 1.          ' )
      ( '      WHEN 200.             ' )
      ( '        value = 2.          ' )
      ( '    ENDCASE.                ' )
      ( |  ELSEIF sy-langu = 'DE'.   | )
      ( '    CASE sy-mandt.          ' )
      ( '      WHEN 300.             ' )
      ( '        value = 3.          ' )
      ( '      WHEN 400.             ' )
      ( '        value = 4.          ' )
      ( '      WHEN 500.             ' )
      ( '        value = 5.          ' )
      ( '      WHEN 600.             ' )
      ( '        value = 6.          ' )
      ( '      WHEN 700.             ' )
      ( '        value = 7.          ' )
      ( '    ENDCASE.                ' )
      ( |  ELSEIF sy-langu = 'PT'.   | )
      ( '      value = 8.            ' )
      ( |  ELSEIF sy-mandt = 100.    | )
      ( '      value = 8.            ' )
      ( |  ELSEIF sy-mandt = 200.    | )
      ( '      value = 8.            ' )
      ( '  ENDIF.                    ' )
    ).

  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example. ' )

      ( ' START-OF-SELECTION.                     ' )
      ( '  DATA(value) = 0.                       ' )
      ( '                                         ' )
      ( |  IF sy-langu = 'EN'.                    | )
      ( '    IF sy-mandt = 000.                   ' )
      ( '      value = 0.                         ' )
      ( '    ELSEIF sy-mandt = 100.               ' )
      ( '      value = 1.                         ' )
      ( '    ELSEIF sy-mandt = 200.               ' )
      ( '      value = 2.                         ' )
      ( '    ENDIF.                               ' )
      ( |  ELSEIF sy-langu = 'DE'.                | )
      ( '    IF sy-mandt = 300. "#EC PREFER_CASE  ' )
      ( '      value = 3.                         ' )
      ( '    ELSEIF sy-mandt = 400.               ' )
      ( '      value = 4.                         ' )
      ( '    ELSEIF sy-mandt = 500.               ' )
      ( '      value = 5.                         ' )
      ( '    ELSEIF sy-mandt = 600.               ' )
      ( '      value = 6.                         ' )
      ( '    ELSEIF sy-mandt = 700.               ' )
      ( '      value = 7.                         ' )
      ( '    ENDIF.                               ' )
      ( |  ELSEIF sy-langu = 'PT'.                | )
      ( '      value = 8.                         ' )
      ( |  ELSEIF sy-mandt = 100.                 | )
      ( '      value = 8.                         ' )
      ( |  ELSEIF sy-mandt = 200.                 | )
      ( '      value = 8.                         ' )
      ( '  ENDIF.                                 ' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_multiple_conditions DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_multiple_conditions IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_prefer_case_to_elseif( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( 'REPORT y_example.                            ' )

      ( | START-OF-SELECTION.                         | )
      ( |  DATA(value) = 0.                           | )
      ( |                                             | )
      ( |  IF sy-mandt = 000 AND sy-langu = 'PT'.     | )
      ( |    value = 0.                               | )
      ( |  ELSEIF sy-mandt = 100 AND sy-langu = 'PT'. | )
      ( |    value = 1.                               | )
      ( |  ELSEIF sy-mandt = 100 AND sy-langu = 'EN'. | )
      ( |    value = 2.                               | )
      ( |  ELSEIF sy-mandt = 200 AND sy-langu = 'EN'. | )
      ( |    value = 2.                               | )
      ( |  ELSEIF sy-mandt = 300.                     | )
      ( |    value = 3.                               | )
      ( |  ELSEIF sy-mandt = 400.                     | )
      ( |    value = 4.                               | )
      ( |  ELSEIF sy-mandt = 500.                     | )
      ( |    value = 5.                               | )
      ( |  ELSEIF sy-mandt = 600.                     | )
      ( |    value = 6.                               | )
      ( |  ELSEIF sy-mandt = 700.                     | )
      ( |    value = 7.                               | )
      ( |  ELSEIF sy-mandt = 800 AND sy-langu = 'RU'. | )
      ( |    value = 8.                               | )
      ( |  ENDIF.                                     | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( 'REPORT y_example.                             ' )

      ( ' START-OF-SELECTION.                          ' )
      ( '   DATA(value) = 0.                           ' )

      ( |   IF sy-mandt = 000 AND sy-langu = 'PT'.     | )
      ( '     value = 0.                               ' )
      ( |   ELSEIF sy-mandt = 100 AND sy-langu = 'PT'. | )
      ( '     value = 1.                               ' )
      ( |   ELSEIF sy-mandt = 100 AND sy-langu = 'EN'. | )
      ( '     value = 2.                               ' )
      ( |   ELSEIF sy-mandt = 200 AND sy-langu = 'EN'. | )
      ( '     value = 2.                               ' )
      ( |   ELSEIF sy-mandt = 800 AND sy-langu = 'RU'. | )
      ( |    value = 8.                                | )
      ( '   ENDIF.                                     ' )

      ( '   CASE sy-mandt.                             ' )
      ( '     WHEN 300.                                ' )
      ( '       value = 3.                             ' )
      ( '     WHEN 400.                                ' )
      ( '       value = 4.                             ' )
      ( '     WHEN 500.                                ' )
      ( '       value = 5.                             ' )
      ( '     WHEN 600.                                ' )
      ( '       value = 6.                             ' )
      ( '     WHEN 700.                                ' )
      ( '       value = 7.                             ' )
      ( '   ENDCASE.                                   ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( 'REPORT y_example.                                         ' )

      ( | START-OF-SELECTION.                                      | )
      ( |  DATA(value) = 0.                                        | )
      ( |                                                          | )
      ( |  IF sy-mandt = 000 AND sy-langu = 'PT'. "#EC PREFER_CASE | )
      ( |    value = 0.                                            | )
      ( |  ELSEIF sy-mandt = 100 AND sy-langu = 'PT'.              | )
      ( |    value = 1.                                            | )
      ( |  ELSEIF sy-mandt = 100 AND sy-langu = 'EN'.              | )
      ( |    value = 2.                                            | )
      ( |  ELSEIF sy-mandt = 200 AND sy-langu = 'EN'.              | )
      ( |    value = 2.                                            | )
      ( |  ELSEIF sy-mandt = 300.                                  | )
      ( |    value = 3.                                            | )
      ( |  ELSEIF sy-mandt = 400.                                  | )
      ( |    value = 4.                                            | )
      ( |  ELSEIF sy-mandt = 500.                                  | )
      ( |    value = 5.                                            | )
      ( |  ELSEIF sy-mandt = 600.                                  | )
      ( |    value = 6.                                            | )
      ( |  ELSEIF sy-mandt = 700.                                  | )
      ( |    value = 7.                                            | )
      ( |  ELSEIF sy-mandt = 800 AND sy-langu = 'RU'.              | )
      ( |    value = 8.                                            | )
      ( |  ENDIF.                                                  | )
    ).
  ENDMETHOD.

ENDCLASS.
