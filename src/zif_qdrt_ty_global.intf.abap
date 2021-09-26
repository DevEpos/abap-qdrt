"! <p class="shorttext synchronized" lang="en">Global types for Quick Data Reporter API</p>
INTERFACE zif_qdrt_ty_global
  PUBLIC.

  TYPES:
    ty_generic_value        TYPE c LENGTH 132,
    ty_tabname_range        TYPE RANGE OF tabname,
    "! <p class="shorttext synchronized" lang="en">Type of a value help</p>
    ty_value_help_type      TYPE c LENGTH 30,
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

    "! <p class="shorttext synchronized" lang="en">Type of field</p>
    ty_field_type           TYPE c LENGTH 30,

    "! <p class="shorttext synchronized" lang="en">Source type of DDL source</p>
    ty_ddl_source_type      TYPE c LENGTH 1,

    BEGIN OF ty_ddl_source,
      ddlname     TYPE ddlname,
      source_type TYPE ty_ddl_source_type,
    END OF ty_ddl_source,

    ty_ddl_source_type_range TYPE RANGE OF ty_ddl_source_type,

    "! <p class="shorttext synchronized" lang="en">Settings for query execution</p>
    BEGIN OF ty_query_exec_settings,
      max_rows           TYPE i,
      offset             TYPE i,
      determine_max_rows TYPE abap_bool,
      no_data_select     TYPE abap_bool,
    END OF ty_query_exec_settings,

    "! <p class="shorttext synchronized" lang="en">Field information</p>
    BEGIN OF ty_field_info,
      name               TYPE fieldname,
      is_key             TYPE abap_bool,
      datatype           TYPE dynptype,
      decimals           TYPE decimals,
      length             TYPE ddleng,
      rollname           TYPE rollname,
      domname            TYPE domname,
      has_fix_values     TYPE abap_bool,
      short_description  TYPE scrtext_s,
      medium_description TYPE scrtext_m,
      long_description   TYPE scrtext_l,
      field_text         TYPE as4text,
      has_value_help     TYPE abap_bool,
      ref_field          TYPE fieldname,
      ref_table          TYPE tabname,
      checktable         TYPE tabname,
      is_lowercase       TYPE abap_bool,
    END OF ty_field_info,

    "! <p class="shorttext synchronized" lang="en"></p>
    BEGIN OF ty_field_metadata,
      name               TYPE string,
      is_key             TYPE abap_bool,
      rollname           TYPE rollname,
      type               TYPE string,
      is_numeric         TYPE abap_bool,
      is_total_possible  TYPE abap_bool,
      max_length         TYPE i,
      precision          TYPE i,
      scale              TYPE i,
      short_description  TYPE string,
      medium_description TYPE string,
      long_description   TYPE string,
      field_text         TYPE string,
      semantics          TYPE string,
      unit_field         TYPE string,
      has_value_help     TYPE abap_bool,
      display_format     TYPE string,
      value_help_type    TYPE string,
    END OF ty_field_metadata,

    "! <p class="shorttext synchronized" lang="en">Table of column metadata</p>
    ty_fields_metadata TYPE STANDARD TABLE OF ty_field_metadata WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Metadata of DB entity</p>
    BEGIN OF ty_entity_metadata,
      fields     TYPE ty_fields_metadata,
      parameters TYPE ty_fields_metadata,
    END OF ty_entity_metadata,

    "! <p class="shorttext synchronized" lang="en">Information about search help</p>
    BEGIN OF ty_search_help_info,
      name        TYPE shlpname,
      description TYPE ddtext,
    END OF ty_search_help_info,

    ty_search_help_infos TYPE STANDARD TABLE OF ty_search_help_info WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Value help metadata</p>
    BEGIN OF ty_vh_metadata,
      type            TYPE string,
      value_help_name TYPE shlpname,
      description     TYPE ddtext,
      source_tab      TYPE tabname,
      source_field    TYPE fieldname,
      token_key_field TYPE fieldname,
      fields          TYPE zif_qdrt_ty_global=>ty_fields_metadata,
      filter_fields   TYPE string_table,
      output_fields   TYPE string_table,
    END OF ty_vh_metadata,

    ty_t_vh_metadata TYPE STANDARD TABLE OF ty_vh_metadata WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Data for Data Preview</p>
    BEGIN OF ty_data_preview,
      col_metadata TYPE ty_fields_metadata,
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

    ty_sql_query_params TYPE STANDARD TABLE OF ty_sql_query_param WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Result of SQL query</p>
    BEGIN OF ty_sql_query_result,
      data    TYPE REF TO data,
      message TYPE string,
    END OF ty_sql_query_result.

ENDINTERFACE.
