CLASS lcl_unit_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO y_code_pal_profile.

    METHODS setup.
    METHODS is_point_in_time_true FOR TESTING.
    METHODS is_point_in_time_false FOR TESTING.
    METHODS has_time_collision_true FOR TESTING.
    METHODS has_time_collision_false FOR TESTING.

ENDCLASS.

CLASS y_code_pal_profile DEFINITION LOCAL FRIENDS lcl_unit_test.

CLASS lcl_unit_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW y_code_pal_profile( ).
  ENDMETHOD.

  METHOD is_point_in_time_true.
    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->is_point_in_time( time_start = '20010101'
                                                                      time_end   = '20011231'
                                                                      point      = '20010101' )
                                        msg  = 'result of "is_point_in_time_true_2001" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->is_point_in_time( time_start = '20020101'
                                                                      time_end   = '20021231'
                                                                      point      = '20020505' )
                                        msg  = 'result of "is_point_in_time_true_2002" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->is_point_in_time( time_start = '20030101'
                                                                      time_end   = '20031231'
                                                                      point      = '20031231' )
                                        msg  = 'result of "is_point_in_time_true_2003" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).
  ENDMETHOD.

  METHOD is_point_in_time_false.
    cl_abap_unit_assert=>assert_equals( exp  = abap_false
                                        act  = cut->is_point_in_time( time_start = '20040101'
                                                                      time_end   = '20041231'
                                                                      point      = '20031231' )
                                        msg  = 'result of "is_point_in_time_false_2004" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_false
                                        act  = cut->is_point_in_time( time_start = '20050101'
                                                                      time_end   = '20051231'
                                                                      point      = '20060101' )
                                        msg  = 'result of "is_point_in_time_false_2005" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).
  ENDMETHOD.

  METHOD has_time_collision_true.
    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20000101'
                                                                        timeline_two_end   = '20050606'
                                                                        )
                                        msg  = 'result of "has_time_collided_true" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20050606'
                                                                        timeline_two_end   = '20120101'
                                                                        )
                                        msg  = 'result of "has_time_collided_true" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20000606'
                                                                        timeline_two_end   = '20120101'
                                                                        )
                                        msg  = 'result of "has_time_collided_true" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_true
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20010606'
                                                                        timeline_two_end   = '20090101'
                                                                        )
                                        msg  = 'result of "has_time_collided_true" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).
  ENDMETHOD.

  METHOD has_time_collision_false.
    cl_abap_unit_assert=>assert_equals( exp  = abap_false
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20000101'
                                                                        timeline_two_end   = '20001231'
                                                                        )
                                        msg  = 'result of "has_time_collided_false" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).

    cl_abap_unit_assert=>assert_equals( exp  = abap_false
                                        act  = cut->has_time_collision( timeline_one_start = '20010101'
                                                                        timeline_one_end   = '20101231'
                                                                        timeline_two_start = '20110101'
                                                                        timeline_two_end   = '20111231'
                                                                        )
                                        msg  = 'result of "has_time_collided_false" is incorrect!'
                                        quit = if_aunit_constants=>quit-no
                                        ).
  ENDMETHOD.

ENDCLASS.
