CLASS ltc_string DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_string IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_message_translation( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE 'File not found!' TYPE 'W'. | )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE i002(00). | )
      ( |   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. | )
      ( |   MESSAGE TEXT-006 TYPE 'I' DISPLAY LIKE 'S'. | )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( ' START-OF-SELECTION. ' )
      ( |   MESSAGE 'File not found!' TYPE 'W'. "#EC MSG_TRANSL | )
    ).
  ENDMETHOD.

ENDCLASS.
