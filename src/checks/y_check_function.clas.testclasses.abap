CLASS lth_function DEFINITION.
  PROTECTED SECTION.
    DATA cut TYPE REF TO ltd_check_function.

    METHODS given_function_group.
    METHODS when_run.
    METHODS then_finds.

  PRIVATE SECTION.
    DATA expected_issues TYPE i.

ENDCLASS.


CLASS lth_function IMPLEMENTATION.

  METHOD given_function_group.
    " Get one that has RFC enabled and disabled
    SELECT SINGLE tfdir~pname
    FROM tfdir AS tfdir
    INNER JOIN tfdir AS enabled  ON enabled~pname = tfdir~pname
                                AND enabled~fmode <> @space
    WHERE tfdir~fmode = @space
    INTO @DATA(function_group).

    SELECT COUNT( * )
    FROM tfdir
    WHERE pname = @function_group
    AND tfdir~fmode = @space
    INTO @expected_issues.

    cut->set_ref_scan( y_code_pal_ref_scan_double=>get_from_fuction_group( function_group ) ).
  ENDMETHOD.

  METHOD when_run.
    cut->run( ).
  ENDMETHOD.

  METHOD then_finds.
    cl_abap_unit_assert=>assert_equals( act  = cut->manager->statistics->count-notes
                                        exp  = expected_issues
                                        quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->manager->statistics->count-errors
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->manager->statistics->count-warnings
                                         quit = if_aunit_constants=>quit-no ).
    cl_abap_unit_assert=>assert_initial( act = cut->manager->statistics->count-pseudo_comments
                                         quit = if_aunit_constants=>quit-no ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_function DEFINITION FOR TESTING INHERITING FROM lth_function RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS test FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.

ENDCLASS.


CLASS ltc_function IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test.
    given_function_group( ).
    when_run( ).
    then_finds( ).
  ENDMETHOD.

ENDCLASS.
