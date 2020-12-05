CLASS y_unit_test_coverage DEFINITION SHARED MEMORY ENABLED PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    CLASS-METHODS get IMPORTING program_name  TYPE programm
                                object        TYPE cl_aucv_task=>ty_object_directory_element
                                coverage_type TYPE REF TO ce_scv_coverage_type
                      RETURNING VALUE(result) TYPE decfloat16
                      RAISING   cx_scv_execution_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF buffer_entry,
              object TYPE cl_aucv_task=>ty_object_directory_element,
              coverages TYPE if_scv_coverage=>tab,
           END OF buffer_entry.

    CLASS-DATA buffer TYPE TABLE OF buffer_entry WITH KEY object.

    CLASS-METHODS get_coverage IMPORTING object        TYPE cl_aucv_task=>ty_object_directory_element
                                         coverage_type TYPE REF TO ce_scv_coverage_type
                               RETURNING VALUE(result) TYPE decfloat16.

ENDCLASS.



CLASS y_unit_test_coverage IMPLEMENTATION.

  METHOD get.

    IF line_exists( buffer[ object = object ] ).
      result = get_coverage( object = object
                             coverage_type = coverage_type ).
      RETURN.
    ENDIF.

    DATA(aunit) = cl_aucv_task=>create( i_measure_coverage = abap_true ).

    aunit->add_associated_unit_tests( VALUE #( ( object ) ) ).

    aunit->run( if_aunit_task=>c_run_mode-catch_short_dump ).

    TRY.
        DATA(coverages) = aunit->get_coverage_measurement( )->build_program_result( program_name )->get_coverages( ).
      CATCH cx_scv_call_error.
        " Object not supported (e.g global test class)
        RAISE EXCEPTION TYPE cx_scv_execution_error.
    ENDTRY.

    buffer = VALUE #( BASE buffer
                    ( object = object coverages = coverages ) ).

    result = get_coverage( object = object
                           coverage_type = coverage_type ).

  ENDMETHOD.

  METHOD get_coverage.

    DATA(entry) = buffer[ object = object ].

    DATA(coverage) = entry-coverages[ table_line->type = coverage_type ].

    result = round( val = coverage->get_percentage( )
                    dec = 2 ).

  ENDMETHOD.

ENDCLASS.
