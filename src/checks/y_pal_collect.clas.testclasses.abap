CLASS ltc_sorted_table DEFINITION INHERITING FROM y_code_pal_unit_test_base FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_cut REDEFINITION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_without_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_sorted_table IMPLEMENTATION.

  METHOD get_cut.
    result ?= NEW y_pal_collect( ).
  ENDMETHOD.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE SORTED TABLE OF seats WITH NON-UNIQUE KEY carrid connid. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. ' )
    ).
  ENDMETHOD.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE SORTED TABLE OF seats WITH UNIQUE KEY carrid connid. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE SORTED TABLE OF seats WITH NON-UNIQUE KEY carrid connid. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. "#EC COLLECT ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_hashed_table DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_without_issue REDEFINITION.
ENDCLASS.

CLASS ltc_hashed_table IMPLEMENTATION.

  METHOD get_code_without_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE HASHED TABLE OF seats WITH UNIQUE KEY carrid connid. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_standard_table DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_standard_table IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE STANDARD TABLE OF seats. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE STANDARD TABLE OF seats. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. "#EC COLLECT ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_default_table DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_default_table IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE TABLE OF seats. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA: BEGIN OF seats, ' )
      ( '             carrid   TYPE sflight-carrid, ' )
      ( '             connid   TYPE sflight-connid, ' )
      ( '             seatsocc TYPE sflight-seatsocc, ' )
      ( '           END OF seats.' )

      ( '     DATA seats_tab LIKE TABLE OF seats. ' )

      ( |     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '     COLLECT seats INTO seats_tab. "#EC COLLECT ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_class_attriute DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
    METHODS get_code_with_exemption REDEFINITION.
ENDCLASS.

CLASS ltc_class_attriute IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )

      ( '     CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '       PUBLIC SECTION. ' )
      ( '         METHODS example FOR TESTING. ' )
      ( '       PROTECTED SECTION. ' )
      ( '         CLASS-DATA: BEGIN OF seats, ' )
      ( '                 carrid   TYPE sflight-carrid, ' )
      ( '                 connid   TYPE sflight-connid, ' )
      ( '                 seatsocc TYPE sflight-seatsocc, ' )
      ( '               END OF seats.' )

      ( '     ENDCLASS.' )

      ( '     CLASS lcl_classname IMPLEMENTATION. ' )
      ( '       METHOD example. ' )
      ( '         DATA seats_tab LIKE TABLE OF seats. ' )
      ( |         seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '         COLLECT seats INTO seats_tab. ' )
      ( '       ENDMETHOD. ' )
      ( '     ENDCLASS. ' )
    ).
  ENDMETHOD.

  METHOD get_code_with_exemption.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )

      ( '     CLASS lcl_classname DEFINITION FOR TESTING. ' )
      ( '       PUBLIC SECTION. ' )
      ( '         METHODS example FOR TESTING. ' )
      ( '       PROTECTED SECTION. ' )
      ( '         DATA: BEGIN OF seats, ' )
      ( '                 carrid   TYPE sflight-carrid, ' )
      ( '                 connid   TYPE sflight-connid, ' )
      ( '                 seatsocc TYPE sflight-seatsocc, ' )
      ( '               END OF seats.' )
      ( '     ENDCLASS.' )

      ( '     CLASS lcl_classname IMPLEMENTATION. ' )
      ( '       METHOD example. ' )
      ( '         DATA seats_tab LIKE TABLE OF seats. ' )

      ( |         seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ). | )
      ( '         COLLECT seats INTO seats_tab. "#EC COLLECT ' )
      ( '       ENDMETHOD. ' )
      ( '     ENDCLASS. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_ddic_table_type DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_ddic_table_type IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     DATA table TYPE flprice_t. ' )
      ( |     DATA(entry) = VALUE flprice_s( currency = 'BRL' price = 10 ). | )
      ( '     COLLECT entry INTO table. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_types_table_type DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_types_table_type IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION.      ' )
      ( '     TYPES local_type TYPE TABLE OF flprice_s. ' )
      ( '     DATA table TYPE local_type. ' )
      ( |     DATA(entry) = VALUE flprice_s( currency = 'BRL' price = 10 ). | )
      ( '     COLLECT entry INTO table. ' )
    ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_table_headerline DEFINITION INHERITING FROM ltc_sorted_table FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    METHODS get_code_with_issue REDEFINITION.
ENDCLASS.

CLASS ltc_table_headerline IMPLEMENTATION.

  METHOD get_code_with_issue.
    result = VALUE #(
      ( ' REPORT y_example. ' )
      ( '   START-OF-SELECTION. ' )
      ( '     DATA table TYPE TABLE OF flprice_s WITH HEADER LINE. ' )
      ( '     COLLECT table. ' )
    ).
  ENDMETHOD.

ENDCLASS.
