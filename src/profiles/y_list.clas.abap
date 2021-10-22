CLASS y_list DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_list .
    METHODS constructor IMPORTING !type_name TYPE string
                        RAISING cx_sy_create_data_error.

  PRIVATE SECTION.
    DATA table_component TYPE REF TO data.
    DATA comp_type_name  TYPE string VALUE ''.

ENDCLASS.



CLASS y_list IMPLEMENTATION.


  METHOD constructor.
    CREATE DATA table_component TYPE TABLE OF (type_name).
    comp_type_name = type_name.
  ENDMETHOD.


  METHOD y_if_list~append.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    APPEND line TO <table>.
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~delete_all.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    <table> = VALUE #( ).
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~delete_at.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    DELETE <table> INDEX index.
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~get_line_at.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    READ TABLE <table> ASSIGNING FIELD-SYMBOL(<line1>) INDEX index.
    IF sy-subrc = 0.
      CREATE DATA result LIKE <line1>.
      ASSIGN result->* TO FIELD-SYMBOL(<line2>).
      <line2> = <line1>.
      UNASSIGN: <line2>.
    ENDIF.
    UNASSIGN: <table>, <line1>.
  ENDMETHOD.


  METHOD y_if_list~get_table.
    result = table_component.
  ENDMETHOD.


  METHOD y_if_list~get_type_name.
    result = comp_type_name.
  ENDMETHOD.


  METHOD y_if_list~insert_at.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    INSERT line INTO <table> INDEX index.
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~is_contained.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    FIND FIRST OCCURRENCE OF line IN TABLE <table>.
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.
    UNASSIGN: <table>.
  ENDMETHOD.


  METHOD y_if_list~number_of_rows.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    result = 0.
    ASSIGN table_component->* TO <table>.
    DESCRIBE TABLE <table> LINES result.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~modify_at.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    MODIFY <table> FROM line INDEX index.
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~set_table.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    <table> = table.
    SORT <table> ASCENDING.
    UNASSIGN <table>.
  ENDMETHOD.


  METHOD y_if_list~get_line_index.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    ASSIGN table_component->* TO <table>.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<entry>).
      IF <entry> = line.
        result = sy-tabix.
      ENDIF.
    ENDLOOP.
    UNASSIGN <table>.
  ENDMETHOD.
ENDCLASS.
