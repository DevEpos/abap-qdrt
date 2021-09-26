"! <p class="shorttext synchronized" lang="en">Value Help Data provider</p>
INTERFACE zif_qdrt_vh_data_provider
  PUBLIC.

  TYPES:
    BEGIN OF ty_vh_request,
      type            TYPE zif_qdrt_ty_global=>ty_value_help_type,
      value_help_name TYPE shlpname,
      source_tab      TYPE tabname,
      source_field    TYPE fieldname,
      max_rows        TYPE i,
    END OF ty_vh_request.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves vh data</p>
    get_data
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_qdrt_appl_error.

ENDINTERFACE.
