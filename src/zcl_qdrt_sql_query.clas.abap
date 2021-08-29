"! <p class="shorttext synchronized" lang="en">Database query</p>
CLASS zcl_qdrt_sql_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
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
      "!
      set_parameter_value
        IMPORTING
          name        TYPE fieldname
          value       TYPE zif_qdrt_ty_global=>ty_generic_value OPTIONAL
          value_range TYPE zif_qdrt_ty_global=>ty_selopts OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
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


ENDCLASS.
