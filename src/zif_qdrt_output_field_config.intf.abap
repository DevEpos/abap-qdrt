"! <p class="shorttext synchronized" lang="en">Output field configuration</p>
INTERFACE zif_qdrt_output_field_config
  PUBLIC .

  TYPES:
    BEGIN OF ty_output_field,
      field_name TYPE fieldname,
      function   TYPE string,
      index      TYPE i,
      visible    TYPE abap_bool,
    END OF ty_output_field,

    ty_output_fields TYPE STANDARD TABLE OF ty_output_field WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns the select clause</p>
    get_select_clause
      RETURNING
        VALUE(result) TYPE string_table,
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if field is an output field</p>
    is_output_field
      IMPORTING
        fieldname     TYPE fieldname
      RETURNING
        VALUE(result) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">Sets value for 'Use count field'</p>
    "! If flag 'Use count field' is active an additional COUNT field is added to
    "! the select clause.
    set_use_count_field
      IMPORTING
        value TYPE abap_bool DEFAULT abap_true,
    "! <p class="shorttext synchronized" lang="en">Sets value for 'Select only specified'</p>
    "! If flag 'Select only specified' is active only the specified fields will
    "! be added to the select clause
    set_select_only_specified
      IMPORTING
        value TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if aggregation function are used</p>
    has_aggr_fields
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
