"! <p class="shorttext synchronized" lang="en">Database query</p>
CLASS zcl_qdrt_sql_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    EVENTS:
      request_result
        EXPORTING
          VALUE(max_rows) TYPE i.

    DATA:
      "! <p class="shorttext synchronized" lang="en">SQL Query information</p>
      query_data TYPE zif_qdrt_ty_global=>ty_sql_query READ-ONLY,

      "! <p class="shorttext synchronized" lang="en">List of parameter definitions</p>
      parameters TYPE zif_qdrt_ty_global=>ty_sql_query_params READ-ONLY.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create new query instance</p>
      constructor
        IMPORTING
          query_data TYPE zif_qdrt_ty_global=>ty_sql_query
          parameters TYPE zif_qdrt_ty_global=>ty_sql_query_params,
      "! <p class="shorttext synchronized" lang="en">Set value for a certain parameter</p>
      set_parameter_value
        IMPORTING
          name        TYPE fieldname
          value       TYPE zif_qdrt_ty_global=>ty_generic_value OPTIONAL
          value_range TYPE zif_qdrt_ty_global=>ty_selopts OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Retrieves query result</p>
      get_result
        IMPORTING
          max_rows      TYPE i DEFAULT 100
        RETURNING
          VALUE(result) TYPE zif_qdrt_ty_global=>ty_sql_query_result,
      "! <p class="shorttext synchronized" lang="en">Sets query result</p>
      set_result
        IMPORTING
          result TYPE zif_qdrt_ty_global=>ty_sql_query_result,
      "! <p class="shorttext synchronized" lang="en">Retrieves single value from result</p>
      "! This is only possible if the SQL query result consists of a single value
      get_single_value_result
        EXPORTING
          result TYPE any.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      query_result TYPE zif_qdrt_ty_global=>ty_sql_query_result.
ENDCLASS.



CLASS zcl_qdrt_sql_query IMPLEMENTATION.


  METHOD constructor.
    me->query_data = query_data.
    me->parameters = parameters.
  ENDMETHOD.


  METHOD set_parameter_value.
    ASSIGN parameters[ name = name ] TO FIELD-SYMBOL(<param>).
    CHECK sy-subrc = 0.

    IF value IS SUPPLIED.
      <param>-value = value.
    ELSEIF value_range IS SUPPLIED.
      <param>-value_list = value_range.
    ENDIF.

  ENDMETHOD.


  METHOD set_result.
    query_result = result.
  ENDMETHOD.


  METHOD get_result.
    IF query_result IS INITIAL.
      RAISE EVENT request_result
        EXPORTING
          max_rows = max_rows.
    ENDIF.
    result = query_result.
  ENDMETHOD.


  METHOD get_single_value_result.
    FIELD-SYMBOLS: <result_lines> TYPE table,
                   <result_line>  TYPE any.

    IF query_result IS INITIAL.
      RETURN.
    ENDIF.


    ASSIGN query_result-data->* TO <result_lines>.
    IF sy-subrc <> 0 OR lines( <result_lines> ) > 1.
      RETURN.
    ENDIF.

    ASSIGN <result_lines>[ 1 ] TO <result_line>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DESCRIBE FIELD <result_line> TYPE DATA(line_type) COMPONENTS DATA(lv_comp_count).
    IF line_type = 'u' AND lv_comp_count = 1.
      ASSIGN COMPONENT 1 OF STRUCTURE <result_line> TO FIELD-SYMBOL(<lv_value>).
      TRY.
          result = CONV #( <lv_value> ).
        CATCH cx_conversion_failed.
      ENDTRY.
    ELSEIF line_type NA 'vh'.
      result = CONV #( <result_line> ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.
