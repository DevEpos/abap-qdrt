"! <p class="shorttext synchronized" lang="en">Executes SQL queries</p>
CLASS zcl_qdrt_sql_query_executor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          query type ref to zcl_qdrt_sql_query,
      "! <p class="shorttext synchronized" lang="en">Execute Sql SELECT</p>
      "!
      execute_select
        IMPORTING
          max_row_count TYPE i DEFAULT 100
        RETURNING
          VALUE(result) TYPE zif_qdrt_ty_global=>ty_sql_query_result
        RAISING
          zcx_qdrt_sql_query_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      query type ref to zcl_qdrt_sql_query.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create subroutine for sql statment</p>
      create_subroutine_code
        IMPORTING
          max_row_count  TYPE i OPTIONAL
          only_count     TYPE abap_bool OPTIONAL
        EXPORTING
          VALUE(program) TYPE program
          VALUE(message) TYPE string.
ENDCLASS.



CLASS zcl_qdrt_sql_query_executor IMPLEMENTATION.


  METHOD constructor.
    me->query = query.
  ENDMETHOD.


  METHOD execute_select.
    create_subroutine_code(
      EXPORTING max_row_count = max_row_count
      IMPORTING program       = DATA(program)
                message       = result-message ).

    IF result-message IS NOT INITIAL.
      RETURN.
    ENDIF.

***    DATA(lo_timer) = NEW zcl_dbbr_timer( ).
    DATA(subroutine_class) = |\\PROGRAM={ program }\\CLASS=MAIN|.
***    lo_timer->start( ).
    CALL METHOD (subroutine_class)=>execute RECEIVING dyn_result = result-data.
***    lo_timer->stop( ).

***    rs_table_result-query_execution_time = lo_timer->get_duration_string( ).

  ENDMETHOD.


  METHOD create_subroutine_code.
    DATA: code_lines      TYPE string_table,
          code_lines_temp TYPE string_table.

    code_lines = VALUE #(
      ( |REPORT ZSQL_QUERY.| )
      ( )
      ( |CLASS main DEFINITION.| )
      ( |  PUBLIC SECTION.| )
      ( |    CLASS-METHODS execute| )
      ( |      RETURNING| )
      ( |        VALUE(dyn_result) TYPE REF TO data| )
      ( |      RAISING| )
      ( |        zcx_qdrt_sql_query_error.| )
      ( |ENDCLASS.| )
      ( )
      ( |CLASS main IMPLEMENTATION.| )
      ( |  METHOD execute.| )
      ( |    FIELD-SYMBOLS: <result_lines> TYPE TABLE.| )
      ( ) ).

*.. Add parameters with their chosen values
    LOOP AT query->parameters ASSIGNING FIELD-SYMBOL(<parameter>).
      DATA(param_line) = |    DATA { <parameter>-name } TYPE|.

      IF <parameter>-type IS NOT INITIAL.
        IF <parameter>-is_range = abap_true.
          param_line = |{ param_line } RANGE OF { <parameter>-type }|.
        ELSE.
          param_line = |{ param_line } { <parameter>-type }|.
        ENDIF.
      ELSE.
        param_line = |{ param_line } { <parameter>-inttype } LENGTH { <parameter>-length }|.
        IF <parameter>-decimals > 0.
          param_line = |{ param_line } DECIMALS { <parameter>-decimals }|.
        ENDIF.
      ENDIF.

*.... Fill the value for the parameter
      IF <parameter>-value IS NOT INITIAL.
        param_line = |{ param_line } VALUE '{ <parameter>-value }'|.
      ELSEIF <parameter>-default_value_raw IS NOT INITIAL.
        param_line = |{ param_line } VALUE { <parameter>-default_value_raw }|.
      ENDIF.
      param_line = |{ param_line }.|.
      code_lines = VALUE #( BASE code_lines
        ( param_line )
      ).
    ENDLOOP.

    code_lines = VALUE #( BASE code_lines ( ) ).
    CLEAR code_lines_temp.

*.. Fill range parameters with values
    LOOP AT query->parameters ASSIGNING <parameter> WHERE value_list IS NOT INITIAL.
      code_lines = VALUE #( BASE code_lines ( |    { <parameter>-name } = VALUE #( | ) ).

      LOOP AT <parameter>-value_list ASSIGNING FIELD-SYMBOL(<range_value>).

        DATA(code_line) = |      ( sign = '{ <range_value>-sign }' option = '{ <range_value>-option }' low = '{ <range_value>-low }'|.
        IF <range_value>-high IS NOT INITIAL.
          code_line = |{ code_line } high = '{ <range_value>-high }'|.
        ENDIF.
        code_line = |{ code_line } )|.

        code_lines = VALUE #( BASE code_lines ( code_line ) ).
      ENDLOOP.

      code_lines = VALUE #( BASE code_lines ( |{ code_line } ).| ) ).
    ENDLOOP.

    code_lines = VALUE #( BASE code_lines ( ) ).

*.. Add lines of the select statement
    SPLIT query->query_data-select_source AT cl_abap_char_utilities=>cr_lf INTO TABLE code_lines_temp.

    ASSIGN code_lines_temp[ query->query_data-last_row_in_select_stmnt ] TO FIELD-SYMBOL(<last_query_line>).
    IF query->query_data-is_single_result_query = abap_true.
      <last_query_line> = |{ <last_query_line>(query->query_data-last_row_offset) } INTO @DATA(sql_result)|.
    ELSE.
      <last_query_line> = |{ <last_query_line>(query->query_data-last_row_offset) } INTO TABLE @DATA(sql_result)|.
    ENDIF.
    IF  max_row_count IS SUPPLIED AND
        max_row_count > 0 AND
        query->query_data-is_single_result_query = abap_false AND
        query->query_data-main_select_stmnt_type <> zcl_dbbr_sql_query_parser=>c_keywords-union.
      <last_query_line> = |{ <last_query_line> } UP TO { max_row_count } ROWS.|.
    ELSE.
      <last_query_line> = |{ <last_query_line> }.|.
    ENDIF.

    code_lines = VALUE #( BASE code_lines
      ( |    TRY.| )
      ( LINES OF code_lines_temp ) ).

    IF query->query_data-is_single_result_query = abap_true.
      code_lines = VALUE #( BASE code_lines
       ( |        CREATE DATA dyn_result LIKE TABLE OF sql_result.| ) ).
    ELSE.
      code_lines = VALUE #( BASE code_lines
       ( |        CREATE DATA dyn_result LIKE sql_result.| ) ).
    ENDIF.

    code_lines = VALUE #( BASE code_lines
      ( |        ASSIGN dyn_result->* to <result_lines>.| ) ).

    IF query->query_data-is_single_result_query = abap_true.
      code_lines = VALUE #( BASE code_lines
        ( |      INSERT sql_result INTO TABLE <result_lines>.| ) ).
    ELSE.
      code_lines = VALUE #( BASE code_lines
        ( |      <result_lines> = sql_result.| ) ).
    ENDIF.

    code_lines = VALUE #( BASE code_lines
      ( |      CATCH cx_root INTO DATA(sql_error).| )
      ( |        RAISE EXCEPTION TYPE zcx_qdrt_sql_query_error| )
      ( |          EXPORTING| )
      ( |            previous = sql_error.| )
      ( |    ENDTRY.| )
      ( |  ENDMETHOD.| )
      ( |ENDCLASS.| ) ).

*.. Generate the subroutine
    GENERATE SUBROUTINE POOL code_lines
                        NAME program
                        MESSAGE message.               "#EC CI_GENERATE
  ENDMETHOD.

ENDCLASS.
