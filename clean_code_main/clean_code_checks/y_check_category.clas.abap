CLASS y_check_category DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_category_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS if_ci_test~display_documentation
        REDEFINITION .
  PROTECTED SECTION.
    METHODS get_class_description
        RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
ENDCLASS.


CLASS y_check_category IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    description = get_class_description(  ).
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '001'.
  ENDMETHOD.


  METHOD if_ci_test~display_documentation.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = NEW lcl_check_base_mock( )->settings-documentation
        window_name            = ' '
        new_window             = 'X'
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD get_class_description.
    TRY.
      result = NEW cl_oo_class( myname )->class-descript.
    CATCH cx_class_not_existent.
      result = 'Description Not Available'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
