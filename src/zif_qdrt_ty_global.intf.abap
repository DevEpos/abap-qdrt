"! <p class="shorttext synchronized" lang="en">Global types for Quick Data Reporter API</p>
INTERFACE zif_qdrt_ty_global
  PUBLIC.

  TYPES:
    ty_generic_value        TYPE c LENGTH 132,
    ty_tabname_range        TYPE RANGE OF tabname,
    "! <p class="shorttext synchronized" lang="en">Type of database entity</p>
    ty_entity_type          TYPE string,
    "! <p class="shorttext synchronized" lang="en">Name of database entity</p>
    ty_entity_name          TYPE c LENGTH 30,

    "! <p class="shorttext synchronized" lang="en">Fieldname in SQL</p>
    ty_sql_fieldname        TYPE c LENGTH 62,

    "! <p class="shorttext synchronized" lang="en">Fieldname with alias</p>
    ty_fieldname_with_alias TYPE c LENGTH 35,

    "! <p class="shorttext synchronized" lang="en">SQL function</p>
    ty_sql_function         TYPE c LENGTH 10,

    "! <p class="shorttext synchronized" lang="en">Filter value</p>
    ty_filter_value         TYPE c LENGTH 132,

    "! <p class="shorttext synchronized" lang="en">Metadata of column for Data Preview</p>
    BEGIN OF ty_col_metadata,
      name        TYPE string,
      type        TYPE string,
      length      TYPE int4,
      decimals    TYPE decimals,
      short_desc  TYPE string,
      medium_desc TYPE string,
      long_desc   TYPE string,
      semantics   TYPE string,
      unit_field  TYPE string,
    END OF ty_col_metadata,

    "! <p class="shorttext synchronized" lang="en">Table of column metadata</p>
    ty_col_metadata_t TYPE STANDARD TABLE OF ty_col_metadata WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Data for Data Preview</p>
    BEGIN OF ty_data_preview,
      col_metadata TYPE ty_col_metadata_t,
      data         TYPE REF TO data,
    END OF ty_data_preview,

    "! <p class="shorttext synchronized" lang="en">Generic range structure</p>
    BEGIN OF ty_selopt,
      sign   TYPE ddsign,
      option TYPE ddoption,
      low    TYPE ty_generic_value,
      high   TYPE ty_generic_value,
    END OF ty_selopt,

    "! <p class="shorttext synchronized" lang="en">Generic range table</p>
    ty_selopts TYPE STANDARD TABLE OF ty_selopt WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Extended SELOPT for SQL Where condition</p>
    BEGIN OF ty_selopt_sql,
      sqlfieldname TYPE ty_sql_fieldname,
      field        TYPE ty_fieldname_with_alias,
      sign         TYPE ddsign,
      option       TYPE ddoption,
      low          TYPE ty_filter_value,
      high         TYPE ty_filter_value,
      subquery     TYPE string,
      sql_function TYPE ty_sql_function,
    END OF ty_selopt_sql,

    ty_selopts_sql TYPE STANDARD TABLE OF ty_selopt_sql WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Data of SQL query</p>
    BEGIN OF ty_sql_query,
      source                   TYPE string,
      select_source            TYPE string,
      last_row_in_select_stmnt TYPE i,
      last_row_offset          TYPE i,
      main_select_stmnt_type   TYPE string,
      is_single_result_query   TYPE abap_bool,
      db_entities              TYPE ty_tabname_range,
    END OF ty_sql_query,

    "! <p class="shorttext synchronized" lang="en">Parameter in SQL query</p>
    BEGIN OF ty_sql_query_param,
      name              TYPE fieldname,
      type              TYPE string,
      is_range          TYPE abap_bool,
      length            TYPE int2,
      decimals          TYPE int1,
      inttype           TYPE inttype,
      default_value     TYPE ty_generic_value,
      default_value_raw TYPE ty_generic_value,
      value             TYPE ty_generic_value,
      value_list        TYPE ty_selopts,
    END OF ty_sql_query_param,

    ty_sql_query_params TYPE STANDARD TABLE OF ty_sql_query_param WITH EMPTY KEY.

ENDINTERFACE.
