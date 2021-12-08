CLASS y_code_pal_example DEFINITION PUBLIC FINAL CREATE PUBLIC.

  " Testing Comment_Usage:
  " Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  " Sed volutpat mi aliquam condimentum pretium.
  " Suspendisse sit amet vulputate enim. Suspendisse gravida lorem magna, ac consectetur libero venenatis sed.
  " Aliquam dapibus enim sed mauris mollis accumsan.
  " Aliquam erat volutpat.
  " Quisque vehicula orci at erat hendrerit faucibus.
  " Vivamus id nibh justo. Pellentesque non tincidunt libero.
  " Pellentesque eget quam maximus, egestas turpis nec, ullamcorper augue.
  " Vestibulum rhoncus rhoncus ante, non iaculis est pellentesque at.
  " Maecenas dui tellus, feugiat nec malesuada vitae, sodales quis sem.
  " Mauris aliquam aliquet laoreet. Ut eu blandit ligula.
  " Cras augue nulla, mattis facilisis quam non, bibendum pulvinar leo.
  " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean vitae magna metus.
  " In quis varius justo. Ut non nunc sem.
  " Pellentesque finibus eu libero sed ornare.
  " Etiam augue lorem, luctus at risus ac, aliquam lobortis leo.
  " Nullam lobortis dignissim rhoncus. Praesent quis hendrerit nulla.
  " Curabitur risus metus, condimentum ultrices odio in, eleifend semper massa.
  " Nam in auctor sapien, in volutpat tortor.
  " Integer tincidunt erat auctor tellus vestibulum, sed volutpat tellus imperdiet.
  " Aenean tempor metus ut magna porttitor, nec finibus mauris malesuada.
  " Donec pellentesque ullamcorper tortor, quis aliquet mauris tempor vestibulum.
  " Nunc vulputate sapien vel viverra dapibus. Nullam dapibus tristique velit, sit amet bibendum nulla pulvinar id.
  " Etiam egestas placerat urna non placerat. Maecenas eu hendrerit leo, id posuere justo.
  " Nam et arcu nec orci sagittis blandit. Quisque eu tortor in enim malesuada lacinia.
  " Morbi aliquam nibh sed porttitor eleifend. In lobortis nibh lorem, vitae vulputate nisl blandit et.
  " Donec sollicitudin sit amet sapien eget pellentesque. Proin venenatis urna nec laoreet pulvinar.
  " Nullam ut nisl ante. Phasellus in dui non risus hendrerit efficitur eget vitae magna.
  " Sed accumsan varius nisi, et laoreet turpis accumsan efficitur.
  " Ut at neque sit amet nisl consectetur iaculis eu ut dui. In eu mauris dui.
  " Vestibulum vel nisl et lectus tempus viverra vitae id purus. Duis scelerisque sodales pretium.
  " Praesent ornare aliquam massa, ac tristique lorem faucibus ac.
  " Donec accumsan enim velit. Integer condimentum turpis id risus tempus finibus.
  " Fusce sed lorem arcu. Suspendisse potenti. Nam dapibus risus in enim finibus egestas.
  " Ut ut felis mauris. Fusce finibus elit erat, at tincidunt quam tempor nec.
  " Mauris commodo lacus ut ex dignissim, et congue odio semper. Suspendisse eleifend pellentesque odio, ut porta dui.
  " Generated 5 paragraphs, 375 words, 2567 bytes of Lorem Ipsum

  PUBLIC SECTION.
    DATA attribute_1 TYPE string.
    METHODS is_interface_in_class.

    " Event Usage
    EVENTS event_one.
    CLASS-EVENTS event_two.
    EVENTS event_three.
    EVENTS event_four.
    EVENTS event_five.

  PROTECTED SECTION.
    DATA attribute_2 TYPE string.
    DATA attribute_3 TYPE string.
    DATA attribute_4 TYPE string.
    DATA attribute_5 TYPE string.
    DATA attribute_6 TYPE string.

    METHODS call_method_usage.
    METHODS chain_declaration_usage.
    METHODS check_statement_position.
    METHODS check_in_loop.
    METHODS comment_position.
    METHODS comment_type.
    METHODS cx_root_usage.
    METHODS cyclomatic_complexity.
    METHODS sub_assign_read_table.
    METHODS deprecated_key_words.
    METHODS empty_catches.
    METHODS empty_if_branch.
    METHODS empty_procedure.
    METHODS equals_sign_chaining.
    METHODS magic_number.
    METHODS method_output_parameter
      EXPORTING
        !error        TYPE char1
      RETURNING
        VALUE(result) TYPE string_table. "#EC NUM_OUTPUT_PARA
    METHODS method_return_bool
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS max_nesting_depth.
    METHODS non_class_based_exception
      EXCEPTIONS
        no_class_based.
    METHODS number_executable_statements.
    METHODS number_output_parameters
      EXPORTING
        !output_1 TYPE string
        !output_2 TYPE string.
    METHODS receiving_usage.
    METHODS external_call_in_prod_code_ok.
    METHODS boolean_input_parameter
      IMPORTING
        !do_save TYPE abap_bool.
    METHODS omit_optional_exporting.
    METHODS optional_parameters
      IMPORTING
        !name TYPE string OPTIONAL.
    METHODS returning_name
      RETURNING
        VALUE(name) TYPE string.
    METHODS self_reference.
    METHODS test_seam_usage.
    METHODS multiple_pseudo_comments
      IMPORTING
        !name      TYPE string
        !surname   TYPE string OPTIONAL
        !active    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(age) TYPE i. "#EC RET_NAME #EC BOOL_PARAM "#EC OPTL_PARAM
    METHODS prefer_is_not_to_not_is.
    METHODS prefer_case_to_elseif.
    METHODS prefer_new_to_create_object.
    METHODS prefer_line_exists.
    METHODS deprecated_classes.
    METHODS scope_of_variable.
    METHODS default_key.
    METHODS prefer_returning EXPORTING result TYPE string.
    METHODS text_assembly.
    METHODS collect.
    METHODS prefer_pragmas.

  PRIVATE SECTION.
    DATA attribute_7 TYPE string.
    DATA attribute_8 TYPE string.
    DATA attribute_9 TYPE string.
    DATA attribute_10 TYPE string.
    DATA attribute_11 TYPE string.
    DATA attribute_12 TYPE string.

ENDCLASS.



CLASS Y_CODE_PAL_EXAMPLE IMPLEMENTATION.


  METHOD boolean_input_parameter.
    RETURN.
  ENDMETHOD.


  METHOD call_method_usage.
    CALL METHOD is_interface_in_class.
  ENDMETHOD.


  METHOD chain_declaration_usage.
    DATA:
      car_model   TYPE string,
      system_name TYPE string,
      has_saved   TYPE abap_bool.
  ENDMETHOD.


  METHOD check_in_loop.
    DATA tadir TYPE TABLE OF tadir.
    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
      CHECK <tadir>-author = sy-uname.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_statement_position.
    attribute_1 = 'DSAG'.
    CHECK attribute_3 = 'WALLDORF 2020'.
  ENDMETHOD.


  METHOD comment_position.
  " Temporary code
    DATA(active) = abap_true.
  ENDMETHOD.


  METHOD comment_type.
*   Temporary code
    DATA(active) = abap_true.
  ENDMETHOD.


  METHOD cx_root_usage.
    TRY.
        RAISE EXCEPTION TYPE ycx_code_pal_entry_not_found.
      CATCH cx_root.                                   "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.


  METHOD cyclomatic_complexity.
    DATA(complex) = abap_false.
    IF complex = abap_false.
      IF complex = abap_false.
        complex = abap_false.
      ENDIF.
    ENDIF.
    IF complex = abap_false.
      IF complex = abap_false.
        complex = abap_false.
      ENDIF.
    ENDIF.
    IF complex = abap_false.
      IF complex = abap_false.
        complex = abap_false.
      ENDIF.
    ENDIF.
    IF complex = abap_false.
      IF complex = abap_false.
        complex = abap_false.
      ENDIF.
    ENDIF.
    IF complex = abap_false.
      IF complex = abap_false.
        complex = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD deprecated_classes.
    DATA aunit TYPE REF TO cl_aunit_assert.
  ENDMETHOD.


  METHOD deprecated_key_words.
    MOVE attribute_1 TO attribute_2.
  ENDMETHOD.


  METHOD empty_catches.
    TRY.
        DATA(string) = 1.
      CATCH ycx_code_pal_entry_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD empty_if_branch.
    IF attribute_3 = ` `.
    ENDIF.
  ENDMETHOD.


  METHOD empty_procedure.
  ENDMETHOD.


  METHOD equals_sign_chaining.
    DATA val1 TYPE c.
    DATA val2 TYPE c.
    DATA val3 TYPE c.
    val1 = val2 = val3.
  ENDMETHOD.


  METHOD external_call_in_prod_code_ok.
    SUBMIT demo_program_submit_rep AND RETURN.
  ENDMETHOD.


  METHOD is_interface_in_class.
    RETURN.
  ENDMETHOD.


  METHOD magic_number.
    DO 5 TIMES.
      RETURN.
    ENDDO.
  ENDMETHOD.


  METHOD max_nesting_depth.
    DATA(times) = 1.
    IF times = times.
      IF times = times.
        IF times = times.
          IF times = times.
            IF times = times.
              times = times.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD method_output_parameter.
    RETURN.
  ENDMETHOD.


  METHOD method_return_bool.
    RETURN.
  ENDMETHOD.


  METHOD multiple_pseudo_comments.
    RETURN.
  ENDMETHOD.


  METHOD non_class_based_exception.
    IF attribute_1 = `DKOM`.
      RAISE no_class_based.
    ENDIF.
  ENDMETHOD.


  METHOD number_executable_statements.
    IF attribute_1 = 'DE'.
      attribute_2 = '2020'.
      attribute_3 = 'GERMANY'.
      attribute_4 = 'WALLDORF'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'IN'.
      attribute_2 = '2020'.
      attribute_3 = 'INDIA'.
      attribute_4 = 'BLR'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'CN'.
      attribute_2 = '2020'.
      attribute_3 = 'CHINA'.
      attribute_4 = 'SHG'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'BR'.
      attribute_2 = '2020'.
      attribute_3 = 'BRASIL'.
      attribute_4 = 'SLEO'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'PT'.
      attribute_2 = '2020'.
      attribute_3 = 'PORTUGAL'.
      attribute_4 = 'LIS'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'IT'.
      attribute_2 = '2020'.
      attribute_3 = 'ITALIA'.
      attribute_4 = 'ROM'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'ES'.
      attribute_2 = '2020'.
      attribute_3 = 'SPAIN'.
      attribute_4 = 'MAD'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
    IF attribute_1 = 'FR'.
      attribute_2 = '2020'.
      attribute_3 = 'FRANCE'.
      attribute_4 = 'PAR'.
      attribute_5 = 'DEMO'.
      attribute_6 = 'CODE_PAL'.
    ENDIF.
  ENDMETHOD.


  METHOD number_output_parameters.
    RETURN.
  ENDMETHOD.


  METHOD omit_optional_exporting.
    boolean_input_parameter( EXPORTING do_save = abap_true ).
  ENDMETHOD.


  METHOD optional_parameters.
    RETURN.
  ENDMETHOD.


  METHOD prefer_case_to_elseif.
    DATA(value) = 0.

    IF sy-mandt = 000.                                    "#EC CI_MAGIC
      value = 0.
    ELSEIF sy-mandt = 100.                                "#EC CI_MAGIC
      value = 1.
    ELSEIF sy-mandt = 200.                                "#EC CI_MAGIC
      value = 2.
    ELSEIF sy-mandt = 300.                                "#EC CI_MAGIC
      value = 3.
    ELSEIF sy-mandt = 400.                                "#EC CI_MAGIC
      value = 4.
    ELSEIF sy-mandt = 500.                                "#EC CI_MAGIC
      value = 5.
    ENDIF.
  ENDMETHOD.


  METHOD prefer_new_to_create_object.
    DATA demo_failures TYPE REF TO y_code_pal_example.
    CREATE OBJECT demo_failures.
  ENDMETHOD.


  METHOD prefer_is_not_to_not_is.
    IF NOT attribute_1 IS INITIAL.
      attribute_1 = attribute_2.
    ENDIF.
  ENDMETHOD.


  METHOD prefer_line_exists.
    DATA tadir TYPE TABLE OF tadir.
    DATA exists TYPE abap_bool.
    READ TABLE tadir TRANSPORTING NO FIELDS WITH KEY object = 'y_demo_failures'.
    IF sy-subrc = 0.
      exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD receiving_usage.
    method_output_parameter(
      IMPORTING error = DATA(error)
      RECEIVING result = DATA(sum) ).
  ENDMETHOD.


  METHOD returning_name.
    RETURN.
  ENDMETHOD.


  METHOD scope_of_variable.
    IF sy-subrc = 0.
      DATA(value) = 1.
    ELSE.
      value = 2.
    ENDIF.
  ENDMETHOD.


  METHOD self_reference.
    me->comment_type( ).
  ENDMETHOD.


  METHOD sub_assign_read_table.
    DATA attributes TYPE string_table.

    READ TABLE attributes ASSIGNING FIELD-SYMBOL(<attribute>) WITH KEY table_line = '1'.
    " Do something with <attribute>

    READ TABLE attributes INTO <attribute> WITH KEY table_line = '2'.
    " Do something with <attribute>
  ENDMETHOD.


  METHOD test_seam_usage.
    " SAP_BASIS >= 7.50
    " TEST-SEAM read_report_name.
    "   DATA(report_name) = sy-repid.
    " END-TEST-SEAM.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE


  METHOD default_key.
    DATA tadir TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY.
  ENDMETHOD.


  METHOD prefer_returning.
    result = ''.
  ENDMETHOD.


  METHOD text_assembly.
    DATA(first) = 'A'.
    DATA(second) = 'B'.
    DATA(third) = 'C'.

    WRITE first && ': ' && second && ' - ' && third.
  ENDMETHOD.


  METHOD collect.
     DATA: BEGIN OF seats,
             carrid   TYPE sflight-carrid,
             connid   TYPE sflight-connid,
             seatsocc TYPE sflight-seatsocc,
           END OF seats.

     DATA seats_tab LIKE SORTED TABLE OF seats WITH NON-UNIQUE KEY carrid connid.

     seats = VALUE #( carrid = '01' connid = '02' seatsocc = 30 ).
     COLLECT seats INTO seats_tab.
  ENDMETHOD.


  METHOD prefer_pragmas.
    TRY.
        DATA(div_by_zero) = 1 / 0.
      CATCH cx_root.               "#EC NO_HANDLER "#EC NEED_CX_ROOT
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
