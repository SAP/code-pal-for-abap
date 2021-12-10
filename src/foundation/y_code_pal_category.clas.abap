class Y_CODE_PAL_CATEGORY definition
  public
  inheriting from CL_CI_CATEGORY_ROOT
  create public .

public section.

  methods CONSTRUCTOR .

  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  PROTECTED SECTION.
    METHODS get_class_description
        RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CODE_PAL_CATEGORY IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    description = get_class_description( ).
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '001'.
  ENDMETHOD.


  METHOD if_ci_test~display_documentation.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = NEW lcl_code_pal_base( )->settings-documentation
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
