"! <p class="shorttext synchronized" lang="en">Group configuration</p>
INTERFACE zif_qdrt_aggregation_config
  PUBLIC .

  TYPES:
    BEGIN OF ty_aggregation_expression,
      column_key TYPE fieldname,
      function   TYPE string,
    END OF ty_aggregation_expression,

    ty_aggregation_expressions TYPE STANDARD TABLE OF ty_aggregation_expression WITH EMPTY KEY,

    BEGIN OF ty_having_expression,
      column_key TYPE fieldname,
      function   TYPE string,
      operation  TYPE string,
      value1     TYPE string,
      value2     TYPE string,
    END OF ty_having_expression,

    ty_having_expressions TYPE STANDARD TABLE OF ty_having_expression WITH EMPTY KEY,

    BEGIN OF ty_aggregation_config,
      aggregation_expressions TYPE ty_aggregation_expressions,
      having_expressions      TYPE ty_having_expressions,
    END OF ty_aggregation_config.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns order by clause for select</p>
    get_group_by_clause
      RETURNING
        VALUE(result) TYPE string_table,

    "! <p class="shorttext synchronized" lang="en">Returns having clause for select</p>
    get_having_clause
      RETURNING
        VALUE(result) TYPE string_table,

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if given field exists in a HAVING Expr.</p>
    is_field_in_having_expr
      IMPORTING
        fieldname     TYPE fieldname
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if given field is grouped or aggregated</p>
    is_field_in_aggr_expr
      IMPORTING
        fieldname     TYPE fieldname
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if no aggregation config is empty</p>
    is_empty
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
