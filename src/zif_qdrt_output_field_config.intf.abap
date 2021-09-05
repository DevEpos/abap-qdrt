"! <p class="shorttext synchronized" lang="en">Output field configuration</p>
INTERFACE zif_qdrt_output_field_config
  PUBLIC .

  TYPES:
    BEGIN OF ty_output_field,
      column_key TYPE fieldname,
      index      TYPE i,
      visible    TYPE abap_bool,
    END OF ty_output_field,

    ty_output_fields TYPE STANDARD TABLE OF ty_output_field WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns the select clause</p>
    get_select_clause
      RETURNING
        VALUE(result) TYPE string_table.
ENDINTERFACE.
