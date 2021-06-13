"! <p class="shorttext synchronized" lang="en">Global types for Quick Data Reporter API</p>
INTERFACE zif_qdrt_ty_global
  PUBLIC.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Type of database entity</p>
    ty_entity_type TYPE string,
    "! <p class="shorttext synchronized" lang="en">Name of database entity</p>
    ty_entity_name TYPE c LENGTH 30,

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
    END OF ty_data_preview.

ENDINTERFACE.
