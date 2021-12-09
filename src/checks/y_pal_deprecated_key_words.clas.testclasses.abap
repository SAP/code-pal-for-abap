CLASS ltc_move DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_move IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_key_words( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'example'. | )
      ( |   MOVE '' TO text. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'example '. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'example'. | )
      ( |   MOVE '' TO text. "#EC DEPRECATED_KEY | )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_translate DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_translate IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_deprecated_key_words( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'example'. | )
      ( |   TRANSLATE text TO UPPER CASE. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'EXAMPLE'. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT ut_test.' )

      ( ' START-OF-SELECTION.' )
      ( |   DATA(text) = 'example'. | )
      ( |   TRANSLATE text TO UPPER CASE. "#EC DEPRECATED_KEY | )
    ).
  ENDMETHOD.

ENDCLASS.
